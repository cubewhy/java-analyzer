use std::collections::VecDeque;
use std::path::Path;
use std::sync::Arc;

use dashmap::DashMap;
use parking_lot::RwLock;
use rustc_hash::FxHashSet;
use smallvec::SmallVec;

use crate::build_integration::SourceRootId;
use crate::index::view::IndexView;
use crate::index::{
    BucketIndex, ClassMetadata, ClassOrigin, ClasspathId, IndexScope, ModuleGraph, ModuleId,
    ModuleIndex, NameTable, index_jar,
};

pub struct WorkspaceIndex {
    modules: DashMap<ModuleId, Arc<ModuleIndex>>,
    jdk: Arc<BucketIndex>,
    jar_cache: DashMap<Arc<str>, Arc<BucketIndex>>,
    graph: RwLock<ModuleGraph>,
}

impl WorkspaceIndex {
    pub fn new() -> Self {
        let modules = DashMap::new();
        let root = Arc::new(ModuleIndex::new(ModuleId::ROOT, Arc::from("root")));
        modules.insert(ModuleId::ROOT, root);
        Self {
            modules,
            jdk: Arc::new(BucketIndex::new()),
            jar_cache: DashMap::new(),
            graph: RwLock::new(ModuleGraph::new()),
        }
    }

    pub fn ensure_module(&self, id: ModuleId, name: Arc<str>) -> Arc<ModuleIndex> {
        match self.modules.entry(id) {
            dashmap::mapref::entry::Entry::Occupied(entry) => Arc::clone(entry.get()),
            dashmap::mapref::entry::Entry::Vacant(entry) => {
                let module = Arc::new(ModuleIndex::new(id, name));
                entry.insert(Arc::clone(&module));
                module
            }
        }
    }

    pub fn update_source(
        &self,
        scope: IndexScope,
        origin: ClassOrigin,
        classes: Vec<ClassMetadata>,
    ) {
        let module = self.ensure_module(scope.module, default_module_name(scope.module));
        module.update_source(origin, classes);
    }

    pub fn update_source_in_context(
        &self,
        module: ModuleId,
        source_root: Option<SourceRootId>,
        origin: ClassOrigin,
        classes: Vec<ClassMetadata>,
    ) {
        let module = self.ensure_module(module, default_module_name(module));
        module.update_source_in_root(source_root, origin, classes);
    }

    pub fn remove_source_origin(&self, scope: IndexScope, origin: &ClassOrigin) {
        let module = self.ensure_module(scope.module, default_module_name(scope.module));
        module.source.remove_by_origin(origin);
    }

    pub fn remove_source_origin_in_context(
        &self,
        module: ModuleId,
        source_root: Option<SourceRootId>,
        origin: &ClassOrigin,
    ) {
        let module = self.ensure_module(module, default_module_name(module));
        module.remove_source_origin_in_root(source_root, origin);
    }

    pub fn add_jdk_classes(&self, classes: Vec<ClassMetadata>) {
        self.jdk.add_classes(classes);
    }

    pub fn add_jar_classes(&self, scope: IndexScope, classes: Vec<ClassMetadata>) {
        let module = self.ensure_module(scope.module, default_module_name(scope.module));
        let bucket = Arc::new(BucketIndex::new());
        bucket.add_classes(classes);
        module.add_classpath_bucket(ClasspathId::Main, bucket);
    }

    pub fn get_or_index_jar(&self, path: Arc<str>) -> Arc<BucketIndex> {
        if let Some(existing) = self.jar_cache.get(&path) {
            return Arc::clone(existing.value());
        }

        let bucket = Arc::new(BucketIndex::new());
        match index_jar(Path::new(path.as_ref())) {
            Ok(classes) => bucket.add_classes(classes),
            Err(err) => {
                tracing::warn!(path = path.as_ref(), error = %err, "failed to index jar");
            }
        }

        self.jar_cache
            .insert(Arc::clone(&path), Arc::clone(&bucket));
        bucket
    }

    pub fn set_module_classpath(
        &self,
        module: ModuleId,
        classpath_id: ClasspathId,
        jar_paths: Vec<Arc<str>>,
    ) {
        let buckets = jar_paths
            .iter()
            .map(|p| self.get_or_index_jar(Arc::clone(p)))
            .collect();
        let module = self.ensure_module(module, default_module_name(module));
        module.set_classpath(classpath_id, jar_paths, buckets);
    }

    pub fn set_module_dependencies(&self, module: ModuleId, deps: Vec<ModuleId>) {
        let module_index = self.ensure_module(module, default_module_name(module));
        module_index.set_deps(deps.clone());
        self.graph.write().set_deps(module, deps);
    }

    pub fn register_module_source_roots(
        &self,
        module: ModuleId,
        roots: Vec<(SourceRootId, ClasspathId)>,
    ) {
        let module_index = self.ensure_module(module, default_module_name(module));
        module_index.set_source_roots(roots);
    }

    pub fn set_module_active_classpath(&self, module: ModuleId, classpath_id: ClasspathId) {
        let module_index = self.ensure_module(module, default_module_name(module));
        module_index.set_active_classpath(classpath_id);
    }

    pub fn module_classpath_jars(
        &self,
        module: ModuleId,
        classpath_id: ClasspathId,
    ) -> Vec<Arc<str>> {
        self.ensure_module(module, default_module_name(module))
            .classpath_jars(classpath_id)
    }

    pub fn module_classpath_summary(&self, module: ModuleId) -> Vec<(ClasspathId, Vec<Arc<str>>)> {
        self.ensure_module(module, default_module_name(module))
            .classpath_summary()
    }

    pub fn view(&self, scope: IndexScope) -> IndexView {
        self.view_for_classpath(scope, ClasspathId::Main)
    }

    pub fn view_for_classpath(&self, scope: IndexScope, classpath_id: ClasspathId) -> IndexView {
        self.view_for_analysis_context(scope.module, classpath_id, None)
    }

    pub fn view_for_analysis_context(
        &self,
        module_id: ModuleId,
        classpath_id: ClasspathId,
        source_root: Option<SourceRootId>,
    ) -> IndexView {
        let module = self.ensure_module(module_id, default_module_name(module_id));

        let mut layers: SmallVec<Arc<BucketIndex>, 8> = SmallVec::new();
        let module_jars = module.classpath_jars(classpath_id);
        for bucket in module.visible_source_layers(classpath_id, source_root) {
            layers.push(bucket);
        }
        for bucket in module.classpath_layers(classpath_id) {
            layers.push(bucket);
        }

        let graph = self.graph.read();
        let mut queue: VecDeque<ModuleId> = VecDeque::new();
        let mut seen: FxHashSet<ModuleId> = Default::default();
        seen.insert(module_id);
        queue.extend(graph.deps_of(module_id).iter().copied());

        while let Some(dep) = queue.pop_front() {
            if !seen.insert(dep) {
                continue;
            }
            let dep_module = self.ensure_module(dep, default_module_name(dep));
            for bucket in dep_module.visible_source_layers(ClasspathId::Main, None) {
                layers.push(bucket);
            }
            for bucket in dep_module.classpath_layers(ClasspathId::Main) {
                layers.push(bucket);
            }
            queue.extend(graph.deps_of(dep).iter().copied());
        }

        tracing::debug!(
            module = module_id.0,
            requested_classpath = ?classpath_id,
            source_root = ?source_root.map(|id| id.0),
            module_classpath_jars = module_jars.len(),
            dep_count = seen.len().saturating_sub(1),
            layer_count = layers.len() + 1,
            "building index view for analysis context"
        );
        layers.push(Arc::clone(&self.jdk));
        IndexView::new(layers)
    }

    pub fn build_name_table(&self, scope: IndexScope) -> Arc<NameTable> {
        self.view(scope).build_name_table()
    }

    pub fn build_name_table_for_classpath(
        &self,
        scope: IndexScope,
        classpath_id: ClasspathId,
    ) -> Arc<NameTable> {
        self.view_for_classpath(scope, classpath_id)
            .build_name_table()
    }

    pub fn build_name_table_for_analysis_context(
        &self,
        module: ModuleId,
        classpath_id: ClasspathId,
        source_root: Option<SourceRootId>,
    ) -> Arc<NameTable> {
        self.view_for_analysis_context(module, classpath_id, source_root)
            .build_name_table()
    }

    pub fn add_classes(&self, classes: Vec<ClassMetadata>) {
        self.add_jar_classes(
            IndexScope {
                module: ModuleId::ROOT,
            },
            classes,
        );
    }

    #[allow(dead_code)]
    pub fn graph_version(&self) -> u64 {
        self.graph.read().version()
    }
}

impl Default for WorkspaceIndex {
    fn default() -> Self {
        Self::new()
    }
}

fn default_module_name(id: ModuleId) -> Arc<str> {
    if id == ModuleId::ROOT {
        Arc::from("root")
    } else {
        Arc::from(format!("module-{}", id.0))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::build_integration::SourceRootId;
    use crate::index::{FieldSummary, MethodParams, MethodSummary};
    use rust_asm::constants::ACC_PUBLIC;

    fn make_class(internal: &str, origin: ClassOrigin) -> ClassMetadata {
        let (pkg, name) = internal
            .rsplit_once('/')
            .map(|(p, n)| (Some(Arc::from(p)), Arc::from(n)))
            .unwrap_or((None, Arc::from(internal)));
        ClassMetadata {
            package: pkg,
            name,
            internal_name: Arc::from(internal),
            super_name: None,
            interfaces: vec![],
            annotations: vec![],
            methods: vec![],
            fields: vec![],
            access_flags: ACC_PUBLIC,
            generic_signature: None,
            inner_class_of: None,
            origin,
        }
    }

    fn make_method(name: &str, desc: &str) -> MethodSummary {
        MethodSummary {
            name: Arc::from(name),
            params: MethodParams::from_method_descriptor(desc),
            annotations: vec![],
            access_flags: ACC_PUBLIC,
            is_synthetic: false,
            generic_signature: None,
            return_type: crate::semantic::types::parse_return_type_from_descriptor(desc),
        }
    }

    #[test]
    fn test_close_cleanup_removes_temp_source_overlay_and_falls_back() {
        let idx = WorkspaceIndex::new();
        let scope = IndexScope {
            module: ModuleId::ROOT,
        };

        let mut jdk_array = make_class(
            "java/util/ArrayList",
            ClassOrigin::Jar(Arc::from("jdk://builtin")),
        );
        jdk_array.interfaces.push(Arc::from("java/util/List"));
        jdk_array
            .methods
            .push(make_method("add", "(Ljava/lang/Object;)Z"));
        jdk_array
            .methods
            .push(make_method("add", "(ILjava/lang/Object;)V"));
        for method in &mut jdk_array.methods {
            if method.desc().as_ref() == "(Ljava/lang/Object;)Z" {
                method.generic_signature = Some(Arc::from("(TE;)Z"));
            } else if method.desc().as_ref() == "(ILjava/lang/Object;)V" {
                method.generic_signature = Some(Arc::from("(ITE;)V"));
            }
        }

        let mut jdk_list = make_class(
            "java/util/List",
            ClassOrigin::Jar(Arc::from("jdk://builtin")),
        );
        jdk_list
            .methods
            .push(make_method("add", "(Ljava/lang/Object;)Z"));
        jdk_list
            .methods
            .push(make_method("add", "(ILjava/lang/Object;)V"));
        for method in &mut jdk_list.methods {
            if method.desc().as_ref() == "(Ljava/lang/Object;)Z" {
                method.generic_signature = Some(Arc::from("(TE;)Z"));
            } else if method.desc().as_ref() == "(ILjava/lang/Object;)V" {
                method.generic_signature = Some(Arc::from("(ITE;)V"));
            }
        }

        idx.add_jdk_classes(vec![jdk_array, jdk_list]);

        let source_origin = ClassOrigin::SourceFile(Arc::from(
            "file:///tmp/java_analyzer_sources/java.base/java/util/ArrayList.java",
        ));
        let mut src_array = make_class("java/util/ArrayList", source_origin.clone());
        src_array.interfaces.push(Arc::from("java/util/List"));
        src_array.methods.push(make_method("add", "(LE;)Z"));
        src_array.methods.push(make_method("add", "(ILE;)V"));
        for method in &mut src_array.methods {
            if method.desc().as_ref() == "(LE;)Z" {
                method.generic_signature = Some(Arc::from("(TE;)Z"));
            } else if method.desc().as_ref() == "(ILE;)V" {
                method.generic_signature = Some(Arc::from("(ITE;)V"));
            }
        }
        idx.update_source(scope, source_origin.clone(), vec![src_array]);

        let view_before = idx.view(scope);
        let (methods_before, _): (Vec<Arc<MethodSummary>>, Vec<Arc<FieldSummary>>) =
            view_before.collect_inherited_members("java/util/ArrayList");
        let add_descs_before: Vec<_> = methods_before
            .iter()
            .filter(|m| m.name.as_ref() == "add")
            .map(|m| m.desc().to_string())
            .collect();
        assert!(
            add_descs_before.contains(&"(LE;)Z".to_string()),
            "expected source-shaped add family before cleanup: {:?}",
            add_descs_before
        );
        assert!(
            !add_descs_before.contains(&"(Ljava/lang/Object;)Z".to_string()),
            "view should not expose mixed add families simultaneously: {:?}",
            add_descs_before
        );

        idx.remove_source_origin(scope, &source_origin);

        let view_after = idx.view(scope);
        let (methods_after, _): (Vec<Arc<MethodSummary>>, Vec<Arc<FieldSummary>>) =
            view_after.collect_inherited_members("java/util/ArrayList");
        let add_descs_after: Vec<_> = methods_after
            .iter()
            .filter(|m| m.name.as_ref() == "add")
            .map(|m| m.desc().to_string())
            .collect();
        assert!(
            !add_descs_after.iter().any(|d| d.contains("LE;")),
            "source overlay should be removed on close, got {:?}",
            add_descs_after
        );
        assert!(
            add_descs_after.contains(&"(Ljava/lang/Object;)Z".to_string()),
            "bytecode fallback should remain after cleanup: {:?}",
            add_descs_after
        );
    }

    #[test]
    fn test_analysis_context_view_uses_selected_root_visibility() {
        let idx = WorkspaceIndex::new();
        let module = ModuleId(1);
        idx.ensure_module(module, Arc::from("app"));
        idx.register_module_source_roots(
            module,
            vec![
                (SourceRootId(10), ClasspathId::Main),
                (SourceRootId(20), ClasspathId::Test),
            ],
        );

        let main_origin = ClassOrigin::SourceFile(Arc::from("file:///main/Foo.java"));
        let test_origin = ClassOrigin::SourceFile(Arc::from("file:///test/FooTest.java"));
        idx.update_source_in_context(
            module,
            Some(SourceRootId(10)),
            main_origin.clone(),
            vec![make_class("pkg/Foo", main_origin)],
        );
        idx.update_source_in_context(
            module,
            Some(SourceRootId(20)),
            test_origin.clone(),
            vec![make_class("pkg/FooTest", test_origin)],
        );

        let main_view =
            idx.view_for_analysis_context(module, ClasspathId::Main, Some(SourceRootId(10)));
        assert!(main_view.get_class("pkg/Foo").is_some());
        assert!(main_view.get_class("pkg/FooTest").is_none());

        let test_view =
            idx.view_for_analysis_context(module, ClasspathId::Test, Some(SourceRootId(20)));
        assert!(test_view.get_class("pkg/Foo").is_some());
        assert!(test_view.get_class("pkg/FooTest").is_some());
    }
}
