use std::collections::HashMap;
use std::sync::Arc;

use parking_lot::RwLock;

use crate::build_integration::SourceRootId;
use crate::index::scope::{ClasspathId, ModuleId};
use crate::index::{ArtifactId, BucketIndex, ClassMetadata, ClassOrigin};

#[derive(Clone)]
pub enum ClasspathEntry {
    Artifact(ArtifactId),
    Overlay(Arc<BucketIndex>),
}

pub struct ClasspathIndex {
    pub jars: Vec<Arc<str>>,
    pub entries: Vec<ClasspathEntry>,
}

impl ClasspathIndex {
    fn new(jars: Vec<Arc<str>>, entries: Vec<ClasspathEntry>) -> Self {
        Self { jars, entries }
    }
}

pub struct ModuleQueryCache {
    by_internal: RwLock<HashMap<Arc<str>, Arc<ClassMetadata>>>,
}

impl ModuleQueryCache {
    fn new() -> Self {
        Self {
            by_internal: RwLock::new(HashMap::new()),
        }
    }

    #[allow(dead_code)]
    pub fn get(&self, internal: &str) -> Option<Arc<ClassMetadata>> {
        self.by_internal.read().get(internal).cloned()
    }

    #[allow(dead_code)]
    pub fn insert(&self, internal: Arc<str>, class: Arc<ClassMetadata>) {
        self.by_internal.write().insert(internal, class);
    }

    pub fn clear(&self) {
        self.by_internal.write().clear();
    }
}

struct SourceRootIndex {
    classpath: ClasspathId,
    bucket: Arc<BucketIndex>,
}

struct ModuleState {
    classpaths: HashMap<ClasspathId, ClasspathIndex>,
    active_classpath: ClasspathId,
    source_roots: HashMap<SourceRootId, SourceRootIndex>,
}

pub struct ModuleIndex {
    pub id: ModuleId,
    pub name: Arc<str>,
    pub source: Arc<BucketIndex>,
    state: RwLock<ModuleState>,
    pub query_cache: ModuleQueryCache,
}

impl ModuleIndex {
    pub fn new(id: ModuleId, name: Arc<str>) -> Self {
        let state = ModuleState {
            classpaths: HashMap::new(),
            active_classpath: ClasspathId::Main,
            source_roots: HashMap::new(),
        };
        Self {
            id,
            name,
            source: Arc::new(BucketIndex::new()),
            state: RwLock::new(state),
            query_cache: ModuleQueryCache::new(),
        }
    }

    pub fn update_source(&self, origin: ClassOrigin, classes: Vec<ClassMetadata>) {
        self.source.update_source(origin, classes);
    }

    pub fn set_source_roots(&self, roots: Vec<(SourceRootId, ClasspathId)>) {
        let mut state = self.state.write();
        state.source_roots = roots
            .into_iter()
            .map(|(id, classpath)| {
                (
                    id,
                    SourceRootIndex {
                        classpath,
                        bucket: Arc::new(BucketIndex::new()),
                    },
                )
            })
            .collect();
    }

    pub fn update_source_in_root(
        &self,
        source_root: Option<SourceRootId>,
        origin: ClassOrigin,
        classes: Vec<ClassMetadata>,
    ) -> bool {
        let state = self.state.read();
        if let Some(root_id) = source_root
            && let Some(root) = state.source_roots.get(&root_id)
        {
            tracing::debug!(
                module = self.id.0,
                source_root = root_id.0,
                class_count = classes.len(),
                "update_source_in_root: adding to source root bucket"
            );
            return root.bucket.update_source(origin, classes);
        }
        drop(state);
        tracing::debug!(
            module = self.id.0,
            source_root = ?source_root.map(|id| id.0),
            class_count = classes.len(),
            "update_source_in_root: adding to module-level source bucket (fallback)"
        );
        self.source.update_source(origin, classes)
    }

    pub fn remove_source_origin_in_root(
        &self,
        source_root: Option<SourceRootId>,
        origin: &ClassOrigin,
    ) -> bool {
        let state = self.state.read();
        if let Some(root_id) = source_root
            && let Some(root) = state.source_roots.get(&root_id)
        {
            return root.bucket.remove_by_origin(origin);
        }
        drop(state);
        self.source.remove_by_origin(origin)
    }

    pub fn set_classpath(
        &self,
        id: ClasspathId,
        jars: Vec<Arc<str>>,
        entries: Vec<ClasspathEntry>,
    ) {
        let mut state = self.state.write();
        state
            .classpaths
            .insert(id, ClasspathIndex::new(jars, entries));
    }

    pub fn add_classpath_bucket(&self, id: ClasspathId, bucket: Arc<BucketIndex>) {
        let mut state = self.state.write();
        let entry = state
            .classpaths
            .entry(id)
            .or_insert_with(|| ClasspathIndex::new(Vec::new(), Vec::new()));
        entry.entries.push(ClasspathEntry::Overlay(bucket));
    }

    pub fn set_active_classpath(&self, id: ClasspathId) {
        self.state.write().active_classpath = id;
    }

    pub fn active_classpath_entries(&self) -> Vec<ClasspathEntry> {
        let state = self.state.read();
        state
            .classpaths
            .get(&state.active_classpath)
            .map(|cp| cp.entries.clone())
            .unwrap_or_default()
    }

    pub fn visible_source_layers(
        &self,
        classpath_id: ClasspathId,
        preferred_root: Option<SourceRootId>,
    ) -> Vec<Arc<BucketIndex>> {
        let state = self.state.read();
        if state.source_roots.is_empty() {
            tracing::debug!(
                module = self.id.0,
                "visible_source_layers: no source roots, returning module-level source"
            );
            return vec![Arc::clone(&self.source)];
        }

        let mut layers = Vec::new();
        let mut seen = std::collections::HashSet::<SourceRootId>::new();

        if let Some(root_id) = preferred_root
            && let Some(root) = state.source_roots.get(&root_id)
        {
            tracing::debug!(
                module = self.id.0,
                preferred_root = root_id.0,
                "visible_source_layers: adding preferred root"
            );
            layers.push(Arc::clone(&root.bucket));
            seen.insert(root_id);
        }

        let mut push_matching = |requested: ClasspathId| {
            for (root_id, root) in &state.source_roots {
                if seen.contains(root_id) || root.classpath != requested {
                    continue;
                }
                tracing::debug!(
                    module = self.id.0,
                    root_id = root_id.0,
                    classpath = ?requested,
                    "visible_source_layers: adding matching root"
                );
                layers.push(Arc::clone(&root.bucket));
                seen.insert(*root_id);
            }
        };

        match classpath_id {
            ClasspathId::Main => push_matching(ClasspathId::Main),
            ClasspathId::Test => {
                push_matching(ClasspathId::Test);
                push_matching(ClasspathId::Main);
            }
        }

        if layers.is_empty() {
            tracing::debug!(
                module = self.id.0,
                "visible_source_layers: no matching roots, falling back to module-level source"
            );
            layers.push(Arc::clone(&self.source));
        }

        layers
    }

    pub fn visible_source_package_names(
        &self,
        classpath_id: ClasspathId,
        preferred_root: Option<SourceRootId>,
    ) -> Vec<Arc<str>> {
        let mut packages = std::collections::BTreeSet::new();
        for bucket in self.visible_source_layers(classpath_id, preferred_root) {
            for package in bucket.package_names() {
                packages.insert(package);
            }
        }
        packages.into_iter().collect()
    }

    pub fn classpath_entries(&self, id: ClasspathId) -> Vec<ClasspathEntry> {
        let state = self.state.read();
        state
            .classpaths
            .get(&id)
            .map(|cp| cp.entries.clone())
            .unwrap_or_default()
    }

    pub fn classpath_jars(&self, id: ClasspathId) -> Vec<Arc<str>> {
        let state = self.state.read();
        state
            .classpaths
            .get(&id)
            .map(|cp| cp.jars.clone())
            .unwrap_or_default()
    }

    pub fn classpath_summary(&self) -> Vec<(ClasspathId, Vec<Arc<str>>)> {
        let state = self.state.read();
        state
            .classpaths
            .iter()
            .map(|(id, cp)| (*id, cp.jars.clone()))
            .collect()
    }

    pub(crate) fn all_overlay_bucket_refs(&self) -> Vec<Arc<BucketIndex>> {
        let state = self.state.read();
        let mut buckets = Vec::with_capacity(
            1 + state.source_roots.len()
                + state
                    .classpaths
                    .values()
                    .map(|classpath| {
                        classpath
                            .entries
                            .iter()
                            .filter(|entry| matches!(entry, ClasspathEntry::Overlay(_)))
                            .count()
                    })
                    .sum::<usize>(),
        );
        buckets.push(Arc::clone(&self.source));
        buckets.extend(
            state
                .source_roots
                .values()
                .map(|root| Arc::clone(&root.bucket)),
        );
        for classpath in state.classpaths.values() {
            buckets.extend(classpath.entries.iter().filter_map(|entry| match entry {
                ClasspathEntry::Artifact(_) => None,
                ClasspathEntry::Overlay(bucket) => Some(Arc::clone(bucket)),
            }));
        }
        buckets
    }

    pub(crate) fn classpath_jar_count(&self) -> usize {
        let state = self.state.read();
        state
            .classpaths
            .values()
            .map(|classpath| classpath.jars.len())
            .sum()
    }

    pub(crate) fn clear_query_caches(&self) {
        self.query_cache.clear();
        self.source.clear_query_caches();

        let state = self.state.read();
        for root in state.source_roots.values() {
            root.bucket.clear_query_caches();
        }
        for classpath in state.classpaths.values() {
            for entry in &classpath.entries {
                if let ClasspathEntry::Overlay(bucket) = entry {
                    bucket.clear_query_caches();
                }
            }
        }
    }
}
