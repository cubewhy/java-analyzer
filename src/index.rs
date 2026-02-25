use nucleo::Nucleo;
use nucleo::pattern::{CaseMatching, Normalization};
use rayon::prelude::*;
use rust_asm::class_reader::AttributeInfo;
use rust_asm::class_reader::ClassReader;
use rustc_hash::{FxBuildHasher, FxHashMap};
use std::io::Read;
use std::path::Path;
use std::sync::{Arc, Mutex, OnceLock};

use serde::{Deserialize, Serialize};
use zip::ZipArchive;

use crate::completion::type_resolver::parse_return_type_from_descriptor;

pub mod cache;
pub mod codebase;
pub mod jdk;
pub mod source;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ClassMetadata {
    pub package: Option<Arc<str>>,
    pub name: Arc<str>,
    pub internal_name: Arc<str>,
    pub super_name: Option<Arc<str>>,
    pub interfaces: Vec<Arc<str>>,
    pub methods: Vec<MethodSummary>,
    pub fields: Vec<FieldSummary>,
    pub access_flags: u16,
    pub inner_class_of: Option<Arc<str>>,
    pub origin: ClassOrigin,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum ClassOrigin {
    Jar(Arc<str>),
    SourceFile(Arc<str>),
    Unknown,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MethodSummary {
    pub name: Arc<str>,
    pub descriptor: Arc<str>,
    pub access_flags: u16,
    pub is_synthetic: bool,
    pub generic_signature: Option<Arc<str>>,
    pub return_type: Option<Arc<str>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FieldSummary {
    pub name: Arc<str>,
    pub descriptor: Arc<str>,
    pub access_flags: u16,
    pub is_synthetic: bool,
}

pub fn index_jar<P: AsRef<Path>>(path: P) -> anyhow::Result<Vec<ClassMetadata>> {
    let path = path.as_ref();
    // Try to load from cache
    if let Some(cached) = cache::load_cached(path) {
        return Ok(cached);
    }

    let classes = index_jar_uncached(path)?;

    // save cache
    let classes_clone = classes.clone();
    let path_buf = path.to_path_buf();
    std::thread::spawn(move || {
        cache::save_cache(&path_buf, &classes_clone);
    });

    Ok(classes)
}

fn index_jar_uncached(path: &Path) -> anyhow::Result<Vec<ClassMetadata>> {
    let jar_str = Arc::from(path.to_string_lossy().as_ref());
    let file = std::fs::File::open(path)?;
    let mut archive = ZipArchive::new(std::io::BufReader::new(file))?;

    // 收集所有文件内容（class、java、kt）
    let mut class_files: Vec<(String, Vec<u8>)> = Vec::new();
    let mut source_files: Vec<(String, Vec<u8>)> = Vec::new();

    for i in 0..archive.len() {
        let mut entry = archive.by_index(i)?;
        let name = entry.name().to_string();
        let mut buf = Vec::new();
        entry.read_to_end(&mut buf)?;

        if name.ends_with(".class") {
            class_files.push((name, buf));
        } else if name.ends_with(".java") || name.ends_with(".kt") {
            source_files.push((name, buf));
        }
    }

    // 先解析源文件（优先级高）
    let source_internal_names: std::collections::HashSet<Arc<str>> = {
        let parsed = parse_source_files_parallel(&source_files, &jar_str);
        parsed
            .iter()
            .map(|c| Arc::clone(&c.internal_name))
            .collect()
    };
    let mut source_results = parse_source_files_parallel(&source_files, &jar_str);

    // 解析字节码（跳过已有源文件的类）
    let bytecode_results: Vec<ClassMetadata> = class_files
        .into_par_iter()
        .filter_map(|(name, bytes)| {
            let meta = parse_class_data_bytes(&name, &bytes, Arc::clone(&jar_str))?;
            // 如果源文件已经解析了这个类，跳过字节码版本
            if source_internal_names.contains(&meta.internal_name) {
                None
            } else {
                Some(meta)
            }
        })
        .collect();

    source_results.extend(bytecode_results);
    Ok(source_results)
}

/// Parse class bytes with an explicit origin.
/// `file_name` is used only for debugging; the actual class name comes from the bytecode.
pub(crate) fn parse_class_data_bytes(
    _file_name: &str,
    bytes: &[u8],
    origin: Arc<str>,
) -> Option<ClassMetadata> {
    parse_class_data_with_origin(bytes, ClassOrigin::Jar(origin))
}

/// Internal: parse class bytes with arbitrary origin.
fn parse_class_data_with_origin(bytes: &[u8], origin: ClassOrigin) -> Option<ClassMetadata> {
    let cr = ClassReader::new(bytes);
    let cn = cr.to_class_node().ok()?;

    let internal_name: Arc<str> = Arc::from(cn.name.as_str());
    let rsp: Vec<_> = cn.name.rsplitn(2, '/').collect();
    let class_name = *rsp.first()?;
    let package = rsp.get(1).copied();

    let methods = cn
        .methods
        .iter()
        .map(|md| {
            let is_synthetic = md
                .attributes
                .iter()
                .any(|a| matches!(a, AttributeInfo::Synthetic));
            let generic_signature = md.attributes.iter().find_map(|a| {
                if let AttributeInfo::Signature { signature_index } = a {
                    cn.constant_pool
                        .get(*signature_index as usize)
                        .and_then(|cp| {
                            if let rust_asm::class_reader::CpInfo::Utf8(s) = cp {
                                Some(Arc::from(s.as_str()))
                            } else {
                                None
                            }
                        })
                } else {
                    None
                }
            });
            let return_type = parse_return_type_from_descriptor(&md.descriptor);
            MethodSummary {
                name: Arc::from(md.name.as_str()),
                descriptor: Arc::from(md.descriptor.as_str()),
                access_flags: md.access_flags,
                is_synthetic,
                generic_signature,
                return_type,
            }
        })
        .collect();

    let fields = cn
        .fields
        .iter()
        .map(|fd| {
            let is_synthetic = fd
                .attributes
                .iter()
                .any(|a| matches!(a, AttributeInfo::Synthetic));
            FieldSummary {
                name: Arc::from(fd.name.as_str()),
                descriptor: Arc::from(fd.descriptor.as_str()),
                access_flags: fd.access_flags,
                is_synthetic,
            }
        })
        .collect();

    let inner_class_of = if !cn.outer_class.is_empty() {
        let outer_simple = cn.outer_class.rsplit('/').next().unwrap_or(&cn.outer_class);
        Some(Arc::from(outer_simple))
    } else {
        class_name
            .find('$')
            .map(|pos| Arc::from(&class_name[..pos]))
    };

    Some(ClassMetadata {
        package: package.map(Arc::from),
        name: Arc::from(class_name),
        internal_name,
        super_name: cn.super_name.map(|str| intern_str(&str)),
        interfaces: cn
            .interfaces
            .iter()
            .map(|name| intern_str(name.as_str()))
            .collect(),
        methods,
        fields,
        access_flags: cn.access_flags,
        inner_class_of,
        origin,
    })
}

fn intern_str(s: &str) -> Arc<str> {
    static POOL: OnceLock<Mutex<std::collections::HashSet<Arc<str>>>> = OnceLock::new();
    let mut pool = POOL
        .get_or_init(|| Mutex::new(std::collections::HashSet::new()))
        .lock()
        .unwrap();
    if let Some(arc) = pool.get(s) {
        Arc::clone(arc)
    } else {
        let arc: Arc<str> = Arc::from(s);
        pool.insert(Arc::clone(&arc));
        arc
    }
}

pub struct GlobalIndex {
    /// 完整内部名 → ClassMetadata
    exact_match: FxHashMap<Arc<str>, Arc<ClassMetadata>>,
    /// 简单类名 → Vec<内部名>（不同包可能同名）
    simple_name_index: FxHashMap<Arc<str>, Vec<Arc<str>>>,
    /// 包名 → Vec<内部名>（用于通配符 import 展开）
    package_index: FxHashMap<Arc<str>, Vec<Arc<str>>>,
    /// 来源 → Vec<内部名>（用于按文件删除）
    origin_index: FxHashMap<ClassOrigin, Vec<Arc<str>>>,
    /// fuzzy 匹配器（用简单类名）
    fuzzy_matcher: Nucleo<Arc<str>>,
}

impl GlobalIndex {
    pub fn new() -> Self {
        let waker = Arc::new(|| {});

        Self {
            exact_match: FxHashMap::with_capacity_and_hasher(100_000, FxBuildHasher),
            simple_name_index: FxHashMap::with_capacity_and_hasher(100_000, FxBuildHasher),
            package_index: FxHashMap::with_capacity_and_hasher(10_000, FxBuildHasher),
            origin_index: FxHashMap::default(),
            fuzzy_matcher: Nucleo::new(nucleo::Config::DEFAULT, waker, None, 1),
        }
    }

    pub fn add_classes(&mut self, classes: Vec<ClassMetadata>) {
        let injector = self.fuzzy_matcher.injector();
        for mut class in classes {
            class.name = intern_str(&class.name);
            class.internal_name = intern_str(&class.internal_name);
            if let Some(pkg) = &class.package {
                class.package = Some(intern_str(pkg));
            }
            for m in &mut class.methods {
                m.name = intern_str(&m.name);
                m.descriptor = intern_str(&m.descriptor);
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
            if let ClassOrigin::Jar(j) = &class.origin {
                class.origin = ClassOrigin::Jar(intern_str(j));
            } else if let ClassOrigin::SourceFile(s) = &class.origin {
                class.origin = ClassOrigin::SourceFile(intern_str(s));
            }

            let internal = Arc::clone(&class.internal_name);
            let simple = Arc::clone(&class.name);
            let pkg = class.package.clone();
            let origin = class.origin.clone();
            let rc = Arc::new(class);

            self.exact_match
                .insert(Arc::clone(&internal), Arc::clone(&rc));
            self.simple_name_index
                .entry(Arc::clone(&simple))
                .or_default()
                .push(Arc::clone(&internal));

            if let Some(p) = pkg {
                self.package_index
                    .entry(Arc::clone(&p))
                    .or_default()
                    .push(Arc::clone(&internal));
            }

            self.origin_index
                .entry(origin)
                .or_default()
                .push(Arc::clone(&internal));

            injector.push(simple, |item, cols| {
                cols[0] = item.as_ref().into();
            });
        }
    }

    /// Parse multiple JARs in parallel and write them to the index in batches
    /// Faster than calling add_classes one by one because it only rebuilds the class once. (fuzzy)
    pub fn add_classes_bulk(&mut self, batch: Vec<Vec<ClassMetadata>>) {
        let flat: Vec<ClassMetadata> = batch.into_iter().flatten().collect();
        self.add_classes(flat);
    }

    pub fn remove_by_origin(&mut self, origin: &ClassOrigin) {
        let internals = match self.origin_index.remove(origin) {
            Some(v) => v,
            None => return,
        };

        for internal in &internals {
            if let Some(meta) = self.exact_match.remove(internal) {
                // remove from simple_name_index
                if let Some(v) = self.simple_name_index.get_mut(&meta.name) {
                    v.retain(|n| n != internal);
                    if v.is_empty() {
                        self.simple_name_index.remove(&meta.name);
                    }
                }
                // remove from package_index
                if let Some(pkg) = &meta.package
                    && let Some(v) = self.package_index.get_mut(pkg)
                {
                    v.retain(|n| n != internal);
                    if v.is_empty() {
                        self.package_index.remove(pkg);
                    }
                }
            }
        }

        // fuzzy_matcher does not support deleting or rebuilding individual records.
        self.rebuild_fuzzy();
    }

    pub fn update_source(&mut self, origin: ClassOrigin, classes: Vec<ClassMetadata>) {
        self.remove_by_origin(&origin);
        self.add_classes(classes);
    }

    pub fn get_class(&self, internal_name: &str) -> Option<Arc<ClassMetadata>> {
        self.exact_match.get(internal_name).cloned()
    }

    pub fn get_classes_by_simple_name(&self, simple_name: &str) -> Vec<Arc<ClassMetadata>> {
        let result: Vec<Arc<ClassMetadata>> = self
            .simple_name_index
            .get(simple_name)
            .map(|names| {
                names
                    .iter()
                    .filter_map(|n| self.exact_match.get(n.as_ref()).cloned())
                    .collect()
            })
            .unwrap_or_default();

        tracing::debug!(
            simple_name,
            count = result.len(),
            internals = ?result.iter().map(|c| c.internal_name.as_ref()).collect::<Vec<_>>(),
            "get_classes_by_simple_name"
        );

        result
    }

    pub fn classes_in_package(&self, pkg: &str) -> Vec<Arc<ClassMetadata>> {
        let normalized = Arc::from(pkg.replace('.', "/").as_str());
        let result: Vec<Arc<ClassMetadata>> = self
            .package_index
            .get(&normalized)
            .map(|v| {
                v.iter()
                    .filter_map(|n| self.exact_match.get(n).cloned())
                    .collect()
            })
            .unwrap_or_default();
        tracing::debug!(
            pkg,
            normalized = normalized.as_ref(),
            count = result.len(),
            "classes_in_package"
        );
        result
    }

    pub fn has_package(&self, pkg: &str) -> bool {
        let normalized = pkg.replace('.', "/");
        self.package_index.contains_key(normalized.as_str())
    }

    pub fn has_classes_in_package(&self, pkg: &str) -> bool {
        let normalized = pkg.replace('.', "/");
        self.package_index
            .get(normalized.as_str())
            .is_some_and(|v| !v.is_empty())
    }

    /// 先按内部名精确查，失败时按简单名回退（处理 source 解析时只有简单名的继承关系）
    pub fn resolve_class_name(&self, name: &str) -> Option<Arc<ClassMetadata>> {
        if let Some(c) = self.get_class(name) {
            return Some(c);
        }
        // 简单名回退：取最后一段（"extends Parent" 修复后不再需要，但保留防御）
        let simple = name.rsplit('/').next().unwrap_or(name);
        self.get_classes_by_simple_name(simple).into_iter().next()
    }

    pub fn resolve_imports(&self, imports: &[String]) -> Vec<Arc<ClassMetadata>> {
        let mut result = Vec::new();
        for import in imports {
            if import.ends_with(".*") {
                // 通配符：展开整个包
                let pkg = import.trim_end_matches(".*").replace('.', "/");
                let classes = self.classes_in_package(&pkg);
                tracing::debug!(
                    import,
                    pkg,
                    count = classes.len(),
                    "wildcard import expanded"
                );
                result.extend(self.classes_in_package(&pkg));
            } else {
                // 精确 import：按内部名查
                let internal = import.replace('.', "/");
                tracing::debug!(import, internal, "exact import lookup");
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

    /// Collect all methods and fields visible on `class_internal`,
    /// walking the inheritance chain (super_name + interfaces).
    /// Stops at classes not present in the index (e.g. java/lang/Object if not indexed).
    /// Returns (methods, fields) deduplicated by name — subclass members shadow superclass.
    pub fn collect_inherited_members(
        &self,
        class_internal: &str,
    ) -> (Vec<Arc<MethodSummary>>, Vec<Arc<FieldSummary>>) {
        let mut methods: Vec<Arc<MethodSummary>> = Vec::new();
        let mut fields: Vec<Arc<FieldSummary>> = Vec::new();
        // Track seen method signatures to implement shadowing
        let mut seen_methods: std::collections::HashSet<(Arc<str>, Arc<str>)> = Default::default(); // (name, descriptor)
        let mut seen_fields: std::collections::HashSet<Arc<str>> = Default::default();
        let mut queue: std::collections::VecDeque<Arc<str>> = Default::default();

        queue.push_back(Arc::from(class_internal));

        while let Some(internal) = queue.pop_front() {
            let meta = match self.resolve_class_name(&internal) {
                Some(m) => m,
                None => continue,
            };

            // Add methods not yet shadowed by a subclass
            for method in &meta.methods {
                let key = (Arc::clone(&method.name), Arc::clone(&method.descriptor));
                if seen_methods.insert(key) {
                    methods.push(Arc::new(MethodSummary {
                        name: Arc::clone(&method.name),
                        descriptor: Arc::clone(&method.descriptor),
                        access_flags: method.access_flags,
                        is_synthetic: method.is_synthetic,
                        generic_signature: method.generic_signature.clone(),
                        return_type: method.return_type.clone(),
                    }));
                }
            }
            for field in &meta.fields {
                if seen_fields.insert(Arc::clone(&field.name)) {
                    fields.push(Arc::new(FieldSummary {
                        name: Arc::clone(&field.name),
                        descriptor: Arc::clone(&field.descriptor),
                        access_flags: field.access_flags,
                        is_synthetic: field.is_synthetic,
                    }));
                }
            }
            // Enqueue super class
            if let Some(ref super_name) = meta.super_name
                && !super_name.is_empty()
            {
                queue.push_back(super_name.clone());
            }
            // Enqueue interfaces
            for iface in &meta.interfaces {
                if !iface.is_empty() {
                    queue.push_back(Arc::clone(iface));
                }
            }
        }
        (methods, fields)
    }

    /// Return all classes in the MRO (Method Resolution Order) of `class_internal`,
    /// starting from the class itself, walking super_name then interfaces.
    /// Classes not in the index are silently skipped.
    pub fn mro(&self, class_internal: &str) -> Vec<Arc<ClassMetadata>> {
        let mut result = Vec::new();
        let mut seen: std::collections::HashSet<Arc<str>> = std::collections::HashSet::new();
        let mut queue: std::collections::VecDeque<Arc<str>> = std::collections::VecDeque::new();

        queue.push_back(Arc::from(class_internal));
        while let Some(internal) = queue.pop_front() {
            if !seen.insert(internal.clone()) {
                continue; // avoid cycles (e.g. broken index)
            }
            let meta = match self.resolve_class_name(&internal) {
                Some(m) => m,
                None => continue,
            };
            // Enqueue super + interfaces before pushing meta,
            // so we process in BFS order (subclass first)
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

    pub fn fuzzy_autocomplete(&mut self, query: &str, limit: usize) -> Vec<Arc<str>> {
        self.fuzzy_matcher.pattern.reparse(
            0,
            query,
            CaseMatching::Smart,
            Normalization::Smart,
            false,
        );
        let _status = self.fuzzy_matcher.tick(10);
        let snapshot = self.fuzzy_matcher.snapshot();
        let count = snapshot.matched_item_count();
        let end_bound = (limit as u32).min(count);
        snapshot
            .matched_items(..end_bound)
            .map(|item| Arc::clone(item.data))
            .collect()
    }

    pub fn fuzzy_search_classes(&mut self, query: &str, limit: usize) -> Vec<Arc<ClassMetadata>> {
        if query.is_empty() {
            // Nucleo returns nothing for empty query; fall back to iter_all_classes
            return self.exact_match.values().take(limit).cloned().collect();
        }
        let simple_names = self.fuzzy_autocomplete(query, limit);
        simple_names
            .into_iter()
            .flat_map(|name| self.get_classes_by_simple_name(&name))
            .collect()
    }

    pub fn exact_match_keys(&self) -> impl Iterator<Item = &Arc<str>> {
        self.exact_match.keys()
    }

    pub fn class_count(&self) -> usize {
        self.exact_match.len()
    }

    fn rebuild_fuzzy(&mut self) {
        let waker = Arc::new(|| {});
        self.fuzzy_matcher = Nucleo::new(nucleo::Config::DEFAULT, waker, None, 1);
        let injector = self.fuzzy_matcher.injector();
        for name in self.simple_name_index.keys() {
            let n = Arc::clone(name);
            injector.push(n, |item, cols| {
                cols[0] = item.as_ref().into();
            });
        }
    }

    /// 遍历所有已索引的类（用于 import 补全等需要全量扫描的场景）
    pub fn iter_all_classes(&self) -> impl Iterator<Item = &Arc<ClassMetadata>> {
        self.exact_match.values()
    }
}

impl Default for GlobalIndex {
    fn default() -> Self {
        Self::new()
    }
}

fn parse_source_files_parallel(
    files: &[(String, Vec<u8>)],
    jar_origin: &Arc<str>,
) -> Vec<ClassMetadata> {
    files
        .par_iter()
        .flat_map(|(name, bytes)| {
            let content = match std::str::from_utf8(bytes) {
                Ok(s) => s,
                Err(_) => return vec![],
            };
            let lang = if name.ends_with(".kt") {
                "kotlin"
            } else {
                "java"
            };
            let origin = ClassOrigin::Jar(Arc::clone(jar_origin));
            source::parse_source_str(content, lang, origin)
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use rust_asm::constants::ACC_PUBLIC;

    use super::*;
    use crate::completion::type_resolver::count_params;

    fn make_class(pkg: &str, name: &str, origin: ClassOrigin) -> ClassMetadata {
        let internal = format!("{}/{}", pkg, name);
        ClassMetadata {
            package: Some(Arc::from(pkg)),
            name: Arc::from(name),
            internal_name: Arc::from(internal.as_str()),
            super_name: None,
            interfaces: vec![],
            methods: vec![],
            fields: vec![],
            access_flags: ACC_PUBLIC,
            inner_class_of: None,
            origin,
        }
    }

    fn make_method(name: &str, descriptor: &str) -> MethodSummary {
        MethodSummary {
            name: Arc::from(name),
            descriptor: Arc::from(descriptor),
            access_flags: ACC_PUBLIC,
            is_synthetic: false,
            generic_signature: None,
            return_type: parse_return_type_from_descriptor(descriptor),
        }
    }

    // TODO: field tests

    // fn make_field(name: &str, descriptor: &str) -> FieldSummary {
    //     FieldSummary {
    //         name: Arc::from(name),
    //         descriptor: Arc::from(descriptor),
    //         access_flags: ACC_PUBLIC,
    //         is_synthetic: false,
    //     }
    // }

    #[test]
    fn test_add_and_get_class() {
        let mut idx = GlobalIndex::new();
        idx.add_classes(vec![make_class("com/example", "Foo", ClassOrigin::Unknown)]);
        assert!(idx.get_class("com/example/Foo").is_some());
        assert!(idx.get_class("com/example/Bar").is_none());
    }

    #[test]
    fn test_class_count() {
        let mut idx = GlobalIndex::new();
        assert_eq!(idx.class_count(), 0);
        idx.add_classes(vec![
            make_class("a", "A", ClassOrigin::Unknown),
            make_class("b", "B", ClassOrigin::Unknown),
        ]);
        assert_eq!(idx.class_count(), 2);
    }

    #[test]
    fn test_simple_name_lookup_same_name_different_package() {
        let mut idx = GlobalIndex::new();
        idx.add_classes(vec![
            make_class("com/a", "Utils", ClassOrigin::Unknown),
            make_class("com/b", "Utils", ClassOrigin::Unknown),
        ]);
        let results = idx.get_classes_by_simple_name("Utils");
        assert_eq!(results.len(), 2);
        // 两个包都在
        let pkgs: Vec<_> = results
            .iter()
            .map(|c| c.package.as_deref().unwrap_or(""))
            .collect();
        assert!(pkgs.contains(&"com/a"));
        assert!(pkgs.contains(&"com/b"));
    }

    #[test]
    fn test_simple_name_lookup_missing() {
        let idx = GlobalIndex::new();
        assert!(idx.get_classes_by_simple_name("Missing").is_empty());
    }

    // ── package_index / 通配符 ────────────────────────────────────────────────

    #[test]
    fn test_package_index() {
        let mut idx = GlobalIndex::new();
        idx.add_classes(vec![
            make_class("java/util", "List", ClassOrigin::Unknown),
            make_class("java/util", "Map", ClassOrigin::Unknown),
            make_class("java/io", "File", ClassOrigin::Unknown),
        ]);
        let pkg = idx.classes_in_package("java/util");
        assert_eq!(pkg.len(), 2);
        assert!(pkg.iter().any(|c| c.name.as_ref() == "List"));
        assert!(pkg.iter().any(|c| c.name.as_ref() == "Map"));
        // java/io 不在里面
        assert!(pkg.iter().all(|c| c.name.as_ref() != "File"));
    }

    #[test]
    fn test_package_index_dot_notation() {
        // classes_in_package 应该同时接受 "java.util" 和 "java/util"
        let mut idx = GlobalIndex::new();
        idx.add_classes(vec![make_class("java/util", "List", ClassOrigin::Unknown)]);
        assert_eq!(idx.classes_in_package("java.util").len(), 1);
        assert_eq!(idx.classes_in_package("java/util").len(), 1);
    }

    #[test]
    fn test_wildcard_import_resolve() {
        let mut idx = GlobalIndex::new();
        idx.add_classes(vec![
            make_class("java/util", "List", ClassOrigin::Unknown),
            make_class("java/util", "ArrayList", ClassOrigin::Unknown),
            make_class("java/io", "File", ClassOrigin::Unknown),
        ]);

        // java.util.* → List + ArrayList
        let resolved = idx.resolve_imports(&["java.util.*".to_string()]);
        assert_eq!(resolved.len(), 2);
        assert!(resolved.iter().any(|c| c.name.as_ref() == "List"));
        assert!(resolved.iter().any(|c| c.name.as_ref() == "ArrayList"));
    }

    #[test]
    fn test_exact_import_resolve() {
        let mut idx = GlobalIndex::new();
        idx.add_classes(vec![make_class("java/io", "File", ClassOrigin::Unknown)]);
        let resolved = idx.resolve_imports(&["java.io.File".to_string()]);
        assert_eq!(resolved.len(), 1);
        assert_eq!(resolved[0].name.as_ref(), "File");
    }

    #[test]
    fn test_mixed_import_resolve() {
        let mut idx = GlobalIndex::new();
        idx.add_classes(vec![
            make_class("java/util", "List", ClassOrigin::Unknown),
            make_class("java/util", "ArrayList", ClassOrigin::Unknown),
            make_class("java/io", "File", ClassOrigin::Unknown),
        ]);
        let imports = vec!["java.util.*".to_string(), "java.io.File".to_string()];
        let resolved = idx.resolve_imports(&imports);
        assert_eq!(resolved.len(), 3);
    }

    #[test]
    fn test_unknown_import_returns_empty() {
        let idx = GlobalIndex::new();
        let resolved = idx.resolve_imports(&["com.nonexistent.Foo".to_string()]);
        assert!(resolved.is_empty());
    }

    // ── remove_by_origin ─────────────────────────────────────────────────────

    #[test]
    fn test_remove_by_origin_removes_exact_match() {
        let origin = ClassOrigin::SourceFile(Arc::from("file:///A.java"));
        let mut idx = GlobalIndex::new();
        idx.add_classes(vec![
            make_class("com/example", "A", origin.clone()),
            make_class("com/example", "B", ClassOrigin::Unknown),
        ]);

        idx.remove_by_origin(&origin);

        assert!(
            idx.get_class("com/example/A").is_none(),
            "A should be removed"
        );
        assert!(idx.get_class("com/example/B").is_some(), "B should remain");
    }

    #[test]
    fn test_remove_by_origin_updates_simple_name_index() {
        let origin = ClassOrigin::SourceFile(Arc::from("file:///A.java"));
        let mut idx = GlobalIndex::new();
        idx.add_classes(vec![
            make_class("com/a", "Util", origin.clone()),
            make_class("com/b", "Util", ClassOrigin::Unknown),
        ]);

        idx.remove_by_origin(&origin);

        // com/a の Util は消えるが com/b は残る
        let results = idx.get_classes_by_simple_name("Util");
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].package.as_deref(), Some("com/b"));
    }

    #[test]
    fn test_remove_by_origin_updates_package_index() {
        let origin = ClassOrigin::SourceFile(Arc::from("file:///A.java"));
        let mut idx = GlobalIndex::new();
        idx.add_classes(vec![
            make_class("com/example", "A", origin.clone()),
            make_class("com/example", "B", ClassOrigin::Unknown),
        ]);

        idx.remove_by_origin(&origin);

        let pkg = idx.classes_in_package("com/example");
        assert_eq!(pkg.len(), 1);
        assert_eq!(pkg[0].name.as_ref(), "B");
    }

    #[test]
    fn test_remove_by_origin_nonexistent_is_noop() {
        let mut idx = GlobalIndex::new();
        idx.add_classes(vec![make_class("com/example", "A", ClassOrigin::Unknown)]);
        // 删除不存在的 origin 不应 panic
        let fake_origin = ClassOrigin::SourceFile(Arc::from("file:///nonexistent.java"));
        idx.remove_by_origin(&fake_origin);
        assert_eq!(idx.class_count(), 1);
    }

    // ── update_source ─────────────────────────────────────────────────────────

    #[test]
    fn test_update_source_replaces_old_version() {
        let uri = Arc::from("file:///Service.java");
        let origin = ClassOrigin::SourceFile(Arc::clone(&uri));
        let mut idx = GlobalIndex::new();

        // v1: Service 有 oldMethod
        let mut v1 = make_class("com/example", "Service", origin.clone());
        v1.methods.push(make_method("oldMethod", "()V"));
        idx.add_classes(vec![v1]);

        assert!(
            idx.get_class("com/example/Service")
                .unwrap()
                .methods
                .iter()
                .any(|m| m.name.as_ref() == "oldMethod")
        );

        // v2: Service 有 newMethod
        let mut v2 = make_class("com/example", "Service", origin.clone());
        v2.methods.push(make_method("newMethod", "()V"));
        idx.update_source(origin, vec![v2]);

        let cls = idx.get_class("com/example/Service").unwrap();
        assert!(
            !cls.methods.iter().any(|m| m.name.as_ref() == "oldMethod"),
            "oldMethod should be gone"
        );
        assert!(
            cls.methods.iter().any(|m| m.name.as_ref() == "newMethod"),
            "newMethod should be present"
        );
    }

    #[test]
    fn test_update_source_can_add_new_class() {
        let uri = Arc::from("file:///Foo.java");
        let origin = ClassOrigin::SourceFile(Arc::clone(&uri));
        let mut idx = GlobalIndex::new();

        // 初始空
        assert_eq!(idx.class_count(), 0);

        // 第一次 update_source（相当于首次打开文件）
        idx.update_source(
            origin,
            vec![make_class(
                "com/example",
                "Foo",
                ClassOrigin::SourceFile(Arc::clone(&uri)),
            )],
        );
        assert_eq!(idx.class_count(), 1);
        assert!(idx.get_class("com/example/Foo").is_some());
    }

    #[test]
    fn test_update_source_can_remove_class_if_deleted_from_file() {
        let uri = Arc::from("file:///Multi.java");
        let origin = ClassOrigin::SourceFile(Arc::clone(&uri));
        let mut idx = GlobalIndex::new();

        // v1: 文件里有 A 和 B
        idx.add_classes(vec![
            make_class("com/example", "A", origin.clone()),
            make_class("com/example", "B", origin.clone()),
        ]);
        assert_eq!(idx.class_count(), 2);

        // v2: 文件里只剩 A（用户删除了 B）
        idx.update_source(origin.clone(), vec![make_class("com/example", "A", origin)]);
        assert_eq!(idx.class_count(), 1);
        assert!(idx.get_class("com/example/A").is_some());
        assert!(idx.get_class("com/example/B").is_none());
    }

    // ── 描述符工具 ────────────────────────────────────────────────────────────

    #[test]
    fn test_parse_return_type_object() {
        assert_eq!(
            parse_return_type_from_descriptor("()Ljava/util/List;").as_deref(),
            Some("java/util/List")
        );
    }

    #[test]
    fn test_parse_return_type_void() {
        assert_eq!(parse_return_type_from_descriptor("()V"), None);
    }

    #[test]
    fn test_parse_return_type_primitive() {
        assert_eq!(parse_return_type_from_descriptor("()I"), None);
        assert_eq!(parse_return_type_from_descriptor("()Z"), None);
    }

    #[test]
    fn test_parse_return_type_generic_erased() {
        assert_eq!(
            parse_return_type_from_descriptor("()Ljava/util/List<Ljava/lang/String;>;").as_deref(),
            Some("java/util/List")
        );
    }

    #[test]
    fn test_count_params_various() {
        assert_eq!(count_params("()V"), 0);
        assert_eq!(count_params("(I)V"), 1);
        assert_eq!(count_params("(IZ)V"), 2);
        assert_eq!(count_params("(ILjava/lang/String;[B)V"), 3);
        assert_eq!(count_params("([Ljava/lang/String;)V"), 1);
        assert_eq!(count_params("([[I)V"), 1);
    }

    // ── ClassOrigin ───────────────────────────────────────────────────────────

    #[test]
    fn test_origin_jar_vs_source() {
        let jar_origin = ClassOrigin::Jar(Arc::from("/path/to/lib.jar"));
        let src_origin = ClassOrigin::SourceFile(Arc::from("file:///Foo.java"));

        let mut idx = GlobalIndex::new();
        idx.add_classes(vec![
            make_class("com/lib", "Helper", jar_origin.clone()),
            make_class("com/app", "MyClass", src_origin.clone()),
        ]);

        // 删除 jar 来源的类，源文件类保留
        idx.remove_by_origin(&jar_origin);
        assert!(idx.get_class("com/lib/Helper").is_none());
        assert!(idx.get_class("com/app/MyClass").is_some());
    }

    #[test]
    fn test_multiple_jars_independent_origins() {
        let jar_a = ClassOrigin::Jar(Arc::from("a.jar"));
        let jar_b = ClassOrigin::Jar(Arc::from("b.jar"));

        let mut idx = GlobalIndex::new();
        idx.add_classes(vec![
            make_class("com/a", "A", jar_a.clone()),
            make_class("com/b", "B", jar_b.clone()),
        ]);

        idx.remove_by_origin(&jar_a);
        assert!(idx.get_class("com/a/A").is_none());
        assert!(idx.get_class("com/b/B").is_some());
    }

    // ── 与 source 解析集成 ────────────────────────────────────────────────────

    #[test]
    fn test_index_and_query_java_source() {
        use crate::index::codebase::index_source_text;

        let src = r#"
package com.example;
public class Calculator {
    private int result;
    public int add(int a, int b) { return a + b; }
    public int getResult() { return result; }
}
"#;
        let classes = index_source_text("file:///Calculator.java", src, "java");
        assert_eq!(classes.len(), 1);

        let mut idx = GlobalIndex::new();
        idx.add_classes(classes);

        let cls = idx.get_class("com/example/Calculator").unwrap();
        assert_eq!(cls.name.as_ref(), "Calculator");
        assert!(cls.methods.iter().any(|m| m.name.as_ref() == "add"));
        assert!(cls.methods.iter().any(|m| m.name.as_ref() == "getResult"));
        assert!(cls.fields.iter().any(|f| f.name.as_ref() == "result"));

        // 通过包查询
        let pkg_classes = idx.classes_in_package("com/example");
        assert_eq!(pkg_classes.len(), 1);
    }

    #[test]
    fn test_index_and_query_kotlin_source() {
        use crate::index::codebase::index_source_text;

        let src = r#"
package com.example
class UserService(val repo: String) {
    fun findUser(id: Int): String = ""
    fun deleteUser(id: Int) {}
}
object UserFactory {
    fun create(): UserService = UserService("")
}
"#;
        let classes = index_source_text("file:///UserService.kt", src, "kotlin");
        // UserService + UserFactory
        assert!(
            classes.iter().any(|c| c.name.as_ref() == "UserService"),
            "classes: {:?}",
            classes.iter().map(|c| c.name.as_ref()).collect::<Vec<_>>()
        );
        assert!(classes.iter().any(|c| c.name.as_ref() == "UserFactory"));

        let mut idx = GlobalIndex::new();
        idx.add_classes(classes);

        // 查 UserService
        let svc = idx.get_class("com/example/UserService").unwrap();
        assert!(svc.methods.iter().any(|m| m.name.as_ref() == "findUser"));
        assert!(svc.methods.iter().any(|m| m.name.as_ref() == "deleteUser"));

        // 通配符
        let pkg = idx.classes_in_package("com/example");
        assert_eq!(pkg.len(), 2);
    }

    #[test]
    fn test_source_overrides_bytecode_same_class() {
        // 模拟：jar 里有字节码，同时有源文件，源文件优先
        let jar_origin = ClassOrigin::Jar(Arc::from("mylib.jar"));
        let src_origin = ClassOrigin::SourceFile(Arc::from("file:///Foo.java"));

        let mut bytecode_class = make_class("com/example", "Foo", jar_origin);
        bytecode_class
            .methods
            .push(make_method("fromBytecode", "()V"));

        let mut source_class = make_class("com/example", "Foo", src_origin);
        source_class.methods.push(make_method("fromSource", "()V"));

        // 模拟 index_jar 的行为：source 先加入，bytecode 跳过同名
        let mut idx = GlobalIndex::new();
        idx.add_classes(vec![source_class]); // 源文件先

        // 字节码版本：如果 internal_name 已存在则不覆盖
        // （index_jar 里用 source_internal_names 集合过滤）
        if idx.get_class("com/example/Foo").is_none() {
            idx.add_classes(vec![bytecode_class]);
        }

        let cls = idx.get_class("com/example/Foo").unwrap();
        assert!(
            cls.methods.iter().any(|m| m.name.as_ref() == "fromSource"),
            "source version should win"
        );
        assert!(
            !cls.methods
                .iter()
                .any(|m| m.name.as_ref() == "fromBytecode")
        );
    }

    #[test]
    fn test_member_completion_across_files() {
        // 模拟两个文件的场景：Main.java 和 RandomClass.java
        use crate::index::codebase::index_source_text;

        let random_class_src = r#"
package org.cubewhy;
public class RandomClass {
    public void f() {}
}
"#;
        let main_src = r#"
package org.cubewhy.a;
import org.cubewhy.*;
class Main {
    public static void main() {
        RandomClass cl = new RandomClass();
        cl.f();
    }
}
"#;

        let mut idx = GlobalIndex::new();
        // 先 index RandomClass
        idx.add_classes(index_source_text(
            "file:///RandomClass.java",
            random_class_src,
            "java",
        ));
        // 再 index Main
        idx.add_classes(index_source_text("file:///Main.java", main_src, "java"));

        // 验证 RandomClass 在索引里
        let cls = idx.get_class("org/cubewhy/RandomClass");
        assert!(cls.is_some(), "RandomClass should be indexed");
        let cls = cls.unwrap();
        assert!(
            cls.methods.iter().any(|m| m.name.as_ref() == "f"),
            "f() should be in RandomClass"
        );

        // 验证通配符 import 展开能找到 RandomClass
        let resolved = idx.resolve_imports(&["org.cubewhy.*".to_string()]);
        assert!(
            resolved.iter().any(|c| c.name.as_ref() == "RandomClass"),
            "wildcard import should resolve RandomClass: {:?}",
            resolved.iter().map(|c| c.name.as_ref()).collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_mro_walks_superclass() {
        let mut idx = GlobalIndex::new();
        let mut parent = make_class("com/example", "Parent", ClassOrigin::Unknown);
        parent.methods.push(make_method("parentMethod", "()V"));
        let mut child = make_class("com/example", "Child", ClassOrigin::Unknown);
        child.super_name = Some("com/example/Parent".into());
        child.methods.push(make_method("childMethod", "()V"));
        idx.add_classes(vec![parent, child]);

        let (methods, _) = idx.collect_inherited_members("com/example/Child");
        assert!(methods.iter().any(|m| m.name.as_ref() == "childMethod"));
        assert!(
            methods.iter().any(|m| m.name.as_ref() == "parentMethod"),
            "parentMethod should be inherited"
        );
    }

    #[test]
    fn test_mro_simple_name_fallback_for_interface() {
        let mut idx = GlobalIndex::new();
        let mut iface = make_class("java/lang", "Runnable", ClassOrigin::Unknown);
        iface.methods.push(make_method("run", "()V"));
        let mut child = make_class("com/example", "MyThread", ClassOrigin::Unknown);
        // source 解析只有简单名
        child.interfaces = vec!["Runnable".into()];
        idx.add_classes(vec![iface, child]);

        let (methods, _) = idx.collect_inherited_members("com/example/MyThread");
        assert!(
            methods.iter().any(|m| m.name.as_ref() == "run"),
            "run() from Runnable should be inherited via simple-name fallback"
        );
    }

    #[test]
    fn test_has_classes_in_package() {
        let mut idx = GlobalIndex::new();
        idx.add_classes(vec![make_class(
            "java/lang",
            "String",
            ClassOrigin::Unknown,
        )]);
        assert!(!idx.has_classes_in_package("java"));
        assert!(idx.has_classes_in_package("java/lang"));
    }
}
