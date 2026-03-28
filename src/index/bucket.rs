use std::collections::{HashMap, VecDeque};
use std::sync::Arc;

use nucleo_matcher::{
    Config as MatcherConfig, Matcher, Utf32Str,
    pattern::{CaseMatching, Normalization, Pattern},
};
use parking_lot::RwLock;
use rustc_hash::FxHashSet;

use crate::index::{
    ClassMetadata, ClassOrigin, FieldSummary, IndexedJavaModule, MethodSummary, NameTable,
    intern_str,
};

type MroCacheMap = HashMap<Arc<str>, (Vec<Arc<MethodSummary>>, Vec<Arc<FieldSummary>>)>;
type OwnerKey = Arc<str>;

struct BucketState {
    classes: HashMap<Arc<str>, Arc<ClassMetadata>>,
    modules: HashMap<Arc<str>, Arc<IndexedJavaModule>>,
    by_origin: HashMap<ClassOrigin, Vec<Arc<str>>>,
    simple_name_index: HashMap<Arc<str>, Vec<Arc<ClassMetadata>>>,
    package_index: HashMap<Arc<str>, Vec<Arc<ClassMetadata>>>,
    owner_index: HashMap<OwnerKey, Vec<Arc<ClassMetadata>>>,
    name_table: Arc<NameTable>,
    mro_cache: MroCacheMap,
}

pub struct BucketIndex {
    inner: RwLock<BucketState>,
}

impl BucketIndex {
    pub fn new() -> Self {
        let state = BucketState {
            classes: HashMap::with_capacity(100_000),
            modules: HashMap::new(),
            by_origin: HashMap::new(),
            simple_name_index: HashMap::with_capacity(100_000),
            package_index: HashMap::with_capacity(10_000),
            owner_index: HashMap::with_capacity(50_000),
            name_table: Arc::new(NameTable(FxHashSet::default())),
            mro_cache: HashMap::new(),
        };

        Self {
            inner: RwLock::new(state),
        }
    }

    pub fn add_classes(&self, classes: Vec<ClassMetadata>) {
        let mut inner = self.inner.write();

        for mut class in classes {
            Self::intern_class(&mut class);

            let internal = Arc::clone(&class.internal_name);
            let simple = Arc::clone(&class.name);
            let pkg = class.package.clone();
            let owner_internal = class.inner_class_of.clone();
            let origin = class.origin.clone();
            let rc = Arc::new(class);

            inner.classes.insert(Arc::clone(&internal), Arc::clone(&rc));
            inner
                .simple_name_index
                .entry(Arc::clone(&simple))
                .or_default()
                .push(Arc::clone(&rc));

            if let Some(p) = pkg {
                inner
                    .package_index
                    .entry(Arc::clone(&p))
                    .or_default()
                    .push(Arc::clone(&rc));
            }
            if let Some(owner_name) = owner_internal {
                inner
                    .owner_index
                    .entry(owner_name)
                    .or_default()
                    .push(Arc::clone(&rc));
            }

            inner
                .by_origin
                .entry(origin)
                .or_default()
                .push(Arc::clone(&internal));

            Arc::make_mut(&mut inner.name_table)
                .0
                .insert(Arc::clone(&internal));
        }

        inner.mro_cache.clear();
    }

    pub fn add_modules(&self, modules: Vec<IndexedJavaModule>) {
        let mut inner = self.inner.write();
        for module in modules {
            let name = Arc::from(module.name());
            inner.modules.insert(name, Arc::new(module));
        }
    }

    pub fn update_source(&self, origin: ClassOrigin, classes: Vec<ClassMetadata>) -> bool {
        let mut filtered = Vec::new();

        for c in classes {
            if let Some(existing) = self.get_class(&c.internal_name)
                && matches!(existing.origin, ClassOrigin::Jar(_))
            {
                // the LSP only trusts .class
                tracing::warn!(class = %c.internal_name, "BLOCKED: Prevented source AST from corrupting bytecode index!");
                continue;
            }

            filtered.push(c);
        }

        if filtered.is_empty() {
            return false;
        }

        if self.same_classes_for_origin(&origin, &filtered) {
            return false;
        }

        self.remove_by_origin(&origin);
        self.add_classes(filtered);
        true
    }

    pub fn get_class(&self, internal_name: &str) -> Option<Arc<ClassMetadata>> {
        let inner = self.inner.read();
        inner.classes.get(internal_name).cloned()
    }

    pub fn get_module(&self, module_name: &str) -> Option<Arc<IndexedJavaModule>> {
        let inner = self.inner.read();
        inner.modules.get(module_name).cloned()
    }

    pub fn get_source_type_name(&self, internal: &str) -> Option<String> {
        let class = self.get_class(internal)?;
        Some(class.qualified_source_name_with(|owner_internal| self.get_class(owner_internal)))
    }

    pub fn get_classes_by_simple_name(&self, simple_name: &str) -> Vec<Arc<ClassMetadata>> {
        let inner = self.inner.read();
        inner
            .simple_name_index
            .get(simple_name)
            .cloned()
            .unwrap_or_default()
    }

    pub fn direct_inner_classes_of(&self, owner_internal: &str) -> Vec<Arc<ClassMetadata>> {
        self.direct_inner_classes_by_owner(owner_internal)
    }

    pub fn direct_inner_classes_by_owner(&self, owner_internal: &str) -> Vec<Arc<ClassMetadata>> {
        let inner = self.inner.read();
        inner
            .owner_index
            .get(owner_internal)
            .cloned()
            .unwrap_or_default()
    }

    pub fn resolve_direct_inner_class(
        &self,
        owner_internal: &str,
        simple_name: &str,
    ) -> Option<Arc<ClassMetadata>> {
        self.direct_inner_classes_of(owner_internal)
            .into_iter()
            .find(|c| c.matches_simple_name(simple_name))
    }

    pub fn classes_in_package(&self, pkg: &str) -> Vec<Arc<ClassMetadata>> {
        let normalized = pkg.replace('.', "/");
        let inner = self.inner.read();
        inner
            .package_index
            .get(normalized.as_str())
            .cloned()
            .unwrap_or_default()
    }

    pub fn has_package(&self, pkg: &str) -> bool {
        let normalized = pkg.replace('.', "/");
        let inner = self.inner.read();
        inner.package_index.contains_key(normalized.as_str())
    }

    pub fn has_classes_in_package(&self, pkg: &str) -> bool {
        let normalized = pkg.replace('.', "/");
        let inner = self.inner.read();
        inner
            .package_index
            .get(normalized.as_str())
            .is_some_and(|v| !v.is_empty())
    }

    pub fn package_names(&self) -> Vec<Arc<str>> {
        let inner = self.inner.read();
        inner.package_index.keys().cloned().collect()
    }

    pub fn module_names(&self) -> Vec<Arc<str>> {
        let inner = self.inner.read();
        inner.modules.keys().cloned().collect()
    }

    pub fn resolve_imports(&self, imports: &[Arc<str>]) -> Vec<Arc<ClassMetadata>> {
        let mut result = Vec::new();
        for import in imports {
            if import.ends_with(".*") {
                let pkg = import.trim_end_matches(".*").replace('.', "/");
                let classes = self.classes_in_package(&pkg);
                tracing::debug!(
                    import = import.as_ref(),
                    pkg,
                    count = classes.len(),
                    "wildcard import expanded"
                );
                result.extend(classes.into_iter());
            } else {
                let internal = import.replace('.', "/");
                tracing::debug!(import = import.as_ref(), internal, "exact import lookup");
                if let Some(cls) = self.get_class(&internal) {
                    tracing::debug!(internal, "exact import found");
                    result.push(cls);
                } else {
                    tracing::debug!(internal, "exact import NOT FOUND");
                }
            }
        }
        result
    }

    pub fn collect_inherited_members(
        &self,
        class_internal: &str,
    ) -> (Vec<Arc<MethodSummary>>, Vec<Arc<FieldSummary>>) {
        if let Some(cached) = {
            let inner = self.inner.read();
            inner.mro_cache.get(class_internal).cloned()
        } {
            return cached;
        }

        let mut methods: Vec<Arc<MethodSummary>> = Vec::new();
        let mut fields: Vec<Arc<FieldSummary>> = Vec::new();
        let mut seen_methods: FxHashSet<(Arc<str>, Arc<str>)> = Default::default();
        let mut seen_fields: FxHashSet<Arc<str>> = Default::default();
        let mut seen_classes: FxHashSet<Arc<str>> = Default::default();
        let mut queue: VecDeque<Arc<str>> = Default::default();

        queue.push_back(Arc::from(class_internal));

        while let Some(internal) = queue.pop_front() {
            if !seen_classes.insert(Arc::clone(&internal)) {
                continue;
            }

            let meta = match self.get_class(&internal) {
                Some(m) => m,
                None => continue,
            };

            for method in &meta.methods {
                let key = (Arc::clone(&method.name), Arc::clone(&method.desc()));
                if seen_methods.insert(key) {
                    methods.push(Arc::new(method.clone()));
                }
            }
            for field in &meta.fields {
                if seen_fields.insert(Arc::clone(&field.name)) {
                    fields.push(Arc::new(field.clone()));
                }
            }

            if let Some(ref super_name) = meta.super_name
                && !super_name.is_empty()
            {
                queue.push_back(super_name.clone());
            }
            for iface in &meta.interfaces {
                if !iface.is_empty() {
                    queue.push_back(Arc::clone(iface));
                }
            }
        }
        let result = (methods, fields);
        self.inner
            .write()
            .mro_cache
            .insert(Arc::from(class_internal), result.clone());

        result
    }

    pub fn mro(&self, class_internal: &str) -> Vec<Arc<ClassMetadata>> {
        let mut result = Vec::new();
        let mut seen: std::collections::HashSet<Arc<str>> = std::collections::HashSet::new();
        let mut queue: VecDeque<Arc<str>> = VecDeque::new();

        queue.push_back(Arc::from(class_internal));
        while let Some(internal) = queue.pop_front() {
            if !seen.insert(internal.clone()) {
                continue;
            }
            let meta = match self.get_class(&internal) {
                Some(m) => m,
                None => continue,
            };
            if let Some(ref super_name) = meta.super_name
                && !super_name.is_empty()
            {
                queue.push_back(super_name.clone());
            }
            for iface in &meta.interfaces {
                if !iface.is_empty() {
                    queue.push_back(iface.clone());
                }
            }
            result.push(meta);
        }
        result
    }

    pub fn fuzzy_autocomplete(&self, query: &str, limit: usize) -> Vec<Arc<str>> {
        if limit == 0 {
            return vec![];
        }

        let inner = self.inner.read();
        if query.is_empty() {
            let mut names: Vec<_> = inner.simple_name_index.keys().cloned().collect();
            names.sort_unstable_by(|a, b| a.as_ref().cmp(b.as_ref()));
            names.truncate(limit);
            return names;
        }

        let pattern = Pattern::parse(query, CaseMatching::Smart, Normalization::Smart);
        let mut matcher = Matcher::new(MatcherConfig::DEFAULT);
        let mut utf32_buf = Vec::new();
        let mut scored = Vec::new();

        for name in inner.simple_name_index.keys() {
            utf32_buf.clear();
            let haystack = Utf32Str::new(name.as_ref(), &mut utf32_buf);
            if let Some(score) = pattern.score(haystack, &mut matcher) {
                scored.push((Arc::clone(name), score));
            }
        }

        scored.sort_unstable_by(|(left_name, left_score), (right_name, right_score)| {
            right_score
                .cmp(left_score)
                .then_with(|| left_name.as_ref().cmp(right_name.as_ref()))
        });
        scored.truncate(limit);
        scored.into_iter().map(|(name, _)| name).collect()
    }

    pub fn fuzzy_search_classes(&self, query: &str, limit: usize) -> Vec<Arc<ClassMetadata>> {
        if query.is_empty() {
            return self.iter_all_classes().into_iter().take(limit).collect();
        }
        let simple_names = self.fuzzy_autocomplete(query, limit);
        simple_names
            .into_iter()
            .flat_map(|name| self.get_classes_by_simple_name(&name).into_iter())
            .collect()
    }

    pub fn exact_match_keys(&self) -> Vec<Arc<str>> {
        let inner = self.inner.read();
        let keys: Vec<Arc<str>> = inner.classes.keys().cloned().collect();
        tracing::debug!(
            key_count = keys.len(),
            sample_keys = ?keys.iter().take(5).map(|k| k.as_ref()).collect::<Vec<_>>(),
            "BucketIndex::exact_match_keys"
        );
        keys
    }

    pub fn class_count(&self) -> usize {
        let inner = self.inner.read();
        inner.classes.len()
    }

    pub fn iter_all_classes(&self) -> Vec<Arc<ClassMetadata>> {
        let inner = self.inner.read();
        inner.classes.values().cloned().collect()
    }

    pub fn build_name_table(&self) -> Arc<NameTable> {
        let inner = self.inner.read();
        Arc::clone(&inner.name_table)
    }

    pub fn remove_by_origin(&self, origin: &ClassOrigin) -> bool {
        let mut inner = self.inner.write();
        let internals = match inner.by_origin.remove(origin) {
            Some(v) => v,
            None => return false,
        };

        for internal in &internals {
            if let Some(meta) = inner.classes.remove(internal) {
                if let Some(v) = inner.simple_name_index.get_mut(&meta.name) {
                    v.retain(|meta: &Arc<ClassMetadata>| meta.internal_name != *internal);
                    if v.is_empty() {
                        inner.simple_name_index.remove(&meta.name);
                    }
                }
                if let Some(pkg) = &meta.package
                    && let Some(v) = inner.package_index.get_mut(pkg)
                {
                    v.retain(|meta: &Arc<ClassMetadata>| meta.internal_name != *internal);
                    if v.is_empty() {
                        inner.package_index.remove(pkg);
                    }
                }
                if let Some(owner_name) = &meta.inner_class_of {
                    if let Some(v) = inner.owner_index.get_mut(owner_name) {
                        v.retain(|meta: &Arc<ClassMetadata>| meta.internal_name != *internal);
                        if v.is_empty() {
                            inner.owner_index.remove(owner_name);
                        }
                    }
                }
            }
        }

        inner.mro_cache.clear();
        Self::rebuild_name_table_locked(&mut inner);
        true
    }

    fn same_classes_for_origin(&self, origin: &ClassOrigin, classes: &[ClassMetadata]) -> bool {
        let inner = self.inner.read();
        let Some(internals) = inner.by_origin.get(origin) else {
            return false;
        };
        if internals.len() != classes.len() {
            return false;
        }

        let mut existing = internals
            .iter()
            .filter_map(|internal| inner.classes.get(internal).map(|meta| (**meta).clone()))
            .collect::<Vec<_>>();
        drop(inner);

        existing.sort_unstable_by(|left, right| left.internal_name.cmp(&right.internal_name));

        let mut incoming = classes.to_vec();
        incoming.sort_unstable_by(|left, right| left.internal_name.cmp(&right.internal_name));

        existing == incoming
    }

    fn rebuild_name_table_locked(inner: &mut BucketState) {
        inner.name_table = Arc::new(NameTable(inner.classes.keys().cloned().collect()));
    }

    fn intern_class(class: &mut ClassMetadata) {
        class.name = intern_str(&class.name);
        class.internal_name = intern_str(&class.internal_name);
        if let Some(pkg) = &class.package {
            class.package = Some(intern_str(pkg));
        }
        for m in &mut class.methods {
            m.name = intern_str(&m.name);
            if let Some(rt) = &m.return_type {
                m.return_type = Some(intern_str(rt));
            }
            if let Some(gs) = &m.generic_signature {
                m.generic_signature = Some(intern_str(gs));
            }
        }
        for f in &mut class.fields {
            f.name = intern_str(&f.name);
            f.descriptor = intern_str(&f.descriptor);
        }

        match &class.origin {
            ClassOrigin::Jar(j) => {
                class.origin = ClassOrigin::Jar(intern_str(j));
            }
            ClassOrigin::SourceFile(s) => {
                class.origin = ClassOrigin::SourceFile(intern_str(s));
            }
            ClassOrigin::ZipSource {
                zip_path,
                entry_name,
            } => {
                class.origin = ClassOrigin::ZipSource {
                    zip_path: intern_str(zip_path),
                    entry_name: intern_str(entry_name),
                };
            }
            _ => {
                // tracing::error!("Unknown class source found for class {}", class.name);
            }
        }
    }
}

impl Default for BucketIndex {
    fn default() -> Self {
        Self::new()
    }
}
