use std::collections::VecDeque;
use std::sync::Arc;

use rustc_hash::FxHashSet;
use smallvec::SmallVec;

use crate::index::{BucketIndex, ClassMetadata, FieldSummary, MethodSummary, NameTable};

pub struct IndexView {
    layers: SmallVec<Arc<BucketIndex>, 8>,
}

impl IndexView {
    pub fn new(layers: SmallVec<Arc<BucketIndex>, 8>) -> Self {
        Self { layers }
    }

    pub fn get_class(&self, internal_name: &str) -> Option<Arc<ClassMetadata>> {
        for layer in &self.layers {
            if let Some(class) = layer.get_class(internal_name) {
                return Some(class);
            }
        }
        None
    }

    pub fn get_source_type_name(&self, internal: &str) -> Option<String> {
        self.get_class(internal).map(|meta| meta.source_name())
    }

    pub fn get_classes_by_simple_name(&self, simple_name: &str) -> Vec<Arc<ClassMetadata>> {
        let mut out = Vec::new();
        let mut seen: FxHashSet<Arc<str>> = Default::default();
        for layer in &self.layers {
            for class in layer.get_classes_by_simple_name(simple_name) {
                if seen.insert(Arc::clone(&class.internal_name)) {
                    out.push(class);
                }
            }
        }
        out
    }

    pub fn classes_in_package(&self, pkg: &str) -> Vec<Arc<ClassMetadata>> {
        let mut out = Vec::new();
        let mut seen: FxHashSet<Arc<str>> = Default::default();
        for layer in &self.layers {
            for class in layer.classes_in_package(pkg) {
                if seen.insert(Arc::clone(&class.internal_name)) {
                    out.push(class);
                }
            }
        }
        out
    }

    pub fn has_package(&self, pkg: &str) -> bool {
        self.layers.iter().any(|layer| layer.has_package(pkg))
    }

    pub fn has_classes_in_package(&self, pkg: &str) -> bool {
        self.layers
            .iter()
            .any(|layer| layer.has_classes_in_package(pkg))
    }

    pub fn resolve_imports(&self, imports: &[Arc<str>]) -> Vec<Arc<ClassMetadata>> {
        let mut result = Vec::new();
        let mut seen: FxHashSet<Arc<str>> = Default::default();
        for import in imports {
            if import.ends_with(".*") {
                let pkg = import.trim_end_matches(".*").replace('.', "/");
                for class in self.classes_in_package(&pkg) {
                    if seen.insert(Arc::clone(&class.internal_name)) {
                        result.push(class);
                    }
                }
            } else {
                let internal = import.replace('.', "/");
                if let Some(cls) = self.get_class(&internal) {
                    if seen.insert(Arc::clone(&cls.internal_name)) {
                        result.push(cls);
                    }
                }
            }
        }
        result
    }

    pub fn collect_inherited_members(
        &self,
        class_internal: &str,
    ) -> (Vec<Arc<MethodSummary>>, Vec<Arc<FieldSummary>>) {
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
        (methods, fields)
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
        let mut out = Vec::new();
        let mut seen: FxHashSet<Arc<str>> = Default::default();
        for layer in &self.layers {
            for name in layer.fuzzy_autocomplete(query, limit) {
                if seen.insert(Arc::clone(&name)) {
                    out.push(name);
                }
            }
        }
        out
    }

    pub fn fuzzy_search_classes(&self, query: &str, limit: usize) -> Vec<Arc<ClassMetadata>> {
        let mut out = Vec::new();
        let mut seen: FxHashSet<Arc<str>> = Default::default();
        for layer in &self.layers {
            for class in layer.fuzzy_search_classes(query, limit) {
                if seen.insert(Arc::clone(&class.internal_name)) {
                    out.push(class);
                }
            }
        }
        out
    }

    pub fn exact_match_keys(&self) -> Vec<Arc<str>> {
        let mut out = Vec::new();
        let mut seen: FxHashSet<Arc<str>> = Default::default();
        for layer in &self.layers {
            for key in layer.exact_match_keys() {
                if seen.insert(Arc::clone(&key)) {
                    out.push(key);
                }
            }
        }
        out
    }

    pub fn iter_all_classes(&self) -> Vec<Arc<ClassMetadata>> {
        let mut out = Vec::new();
        let mut seen: FxHashSet<Arc<str>> = Default::default();
        for layer in &self.layers {
            for class in layer.iter_all_classes() {
                if seen.insert(Arc::clone(&class.internal_name)) {
                    out.push(class);
                }
            }
        }
        out
    }

    pub fn build_name_table(&self) -> Arc<NameTable> {
        let names = self.exact_match_keys();
        NameTable::from_names(names)
    }
}
