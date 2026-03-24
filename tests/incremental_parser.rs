/// Tests for incremental semantic analysis queries
use java_analyzer::salsa_db::{Database, FileId, SourceFile};
use java_analyzer::salsa_queries::{
    extract_class_members_incremental, extract_class_members_metadata,
    extract_method_locals_incremental, extract_method_locals_metadata, find_enclosing_class_bounds,
    find_enclosing_method_bounds,
};
use java_analyzer::semantic::context::CurrentClassMember;
use java_analyzer::workspace::Workspace;
use std::sync::Arc;
use tower_lsp::lsp_types::Url;

#[test]
fn test_find_enclosing_method_bounds() {
    let db = Database::default();

    let source = r#"
package com.example;

public class Test {
    public void method1() {
        String x = "hello";
        // cursor here at offset ~90
    }
    
    public void method2() {
        int y = 42;
    }
}
"#;

    let uri = Url::parse("file:///test/Test.java").unwrap();
    let file = SourceFile::new(&db, FileId::new(uri), source.to_string(), Arc::from("java"));

    // Cursor inside method1 (around "String x")
    let cursor_offset = 90;
    let bounds = find_enclosing_method_bounds(&db, file, cursor_offset);

    assert!(bounds.is_some(), "Should find enclosing method");

    let (start, end) = bounds.unwrap();
    let method_text = &source[start..end];

    // Should contain method1 but not method2
    assert!(method_text.contains("method1"), "Should be inside method1");
    assert!(
        method_text.contains("String x"),
        "Should contain the variable"
    );
    assert!(
        !method_text.contains("method2"),
        "Should not contain method2"
    );
}

#[test]
fn test_find_enclosing_class_bounds() {
    let db = Database::default();

    let source = r#"
package com.example;

public class OuterClass {
    private String field1;
    
    public void method() {
        // cursor here
    }
    
    class InnerClass {
        void innerMethod() {}
    }
}
"#;

    let uri = Url::parse("file:///test/Test.java").unwrap();
    let file = SourceFile::new(&db, FileId::new(uri), source.to_string(), Arc::from("java"));

    // Cursor inside OuterClass.method
    let cursor_offset = 100;
    let bounds = find_enclosing_class_bounds(&db, file, cursor_offset);

    assert!(bounds.is_some(), "Should find enclosing class");

    let (class_name, start, end) = bounds.unwrap();
    let class_text = &source[start..end];

    assert_eq!(class_name.as_ref(), "OuterClass", "Should find OuterClass");
    assert!(class_text.contains("field1"), "Should contain field1");
    assert!(class_text.contains("method"), "Should contain method");
    assert!(
        class_text.contains("InnerClass"),
        "Should contain InnerClass"
    );
}

#[test]
fn test_find_method_bounds_caching() {
    let db = Database::default();

    let source = r#"
public class Test {
    public void method() {
        int x = 1;
    }
}
"#;

    let uri = Url::parse("file:///test/Test.java").unwrap();
    let file = SourceFile::new(&db, FileId::new(uri), source.to_string(), Arc::from("java"));

    let cursor_offset = 50;

    // First call - cache miss
    let bounds1 = find_enclosing_method_bounds(&db, file, cursor_offset);

    // Second call - should be cached (same file, same offset)
    let bounds2 = find_enclosing_method_bounds(&db, file, cursor_offset);

    assert_eq!(bounds1, bounds2, "Should return same bounds from cache");
    assert!(bounds1.is_some(), "Should find method");
}

#[test]
fn test_find_method_bounds_no_method() {
    let db = Database::default();

    let source = r#"
public class Test {
    private String field = "value";
}
"#;

    let uri = Url::parse("file:///test/Test.java").unwrap();
    let file = SourceFile::new(&db, FileId::new(uri), source.to_string(), Arc::from("java"));

    // Cursor in field declaration (not in a method)
    let cursor_offset = 40;
    let bounds = find_enclosing_method_bounds(&db, file, cursor_offset);

    assert!(
        bounds.is_none(),
        "Should not find method in field declaration"
    );
}

#[test]
fn test_extract_method_locals_metadata() {
    let db = Database::default();

    let source = r#"
public class Test {
    public void method() {
        int x = 1;
        String y = "hello";
        double z = 3.14;
        
        for (int i = 0; i < 10; i++) {
            String temp = "loop";
        }
        
        try {
            int a = 5;
        } catch (Exception e) {
            int b = 6;
        }
    }
}
"#;

    let uri = Url::parse("file:///test/Test.java").unwrap();
    let file = SourceFile::new(&db, FileId::new(uri), source.to_string(), Arc::from("java"));

    // Find the method bounds first
    let cursor_offset = 50;
    let bounds = find_enclosing_method_bounds(&db, file, cursor_offset);
    assert!(bounds.is_some(), "Should find method");

    let (method_start, method_end) = bounds.unwrap();

    // Extract locals metadata
    let metadata = extract_method_locals_metadata(&db, file, method_start, method_end);

    // Should count: x, y, z, i, temp, a, e, b = 8 locals
    assert_eq!(metadata.local_count, 8, "Should count 8 local variables");
    assert_eq!(metadata.method_start, method_start);
    assert_eq!(metadata.method_end, method_end);
    assert!(
        metadata.content_hash > 0,
        "Should have non-zero content hash"
    );
}

#[test]
fn test_extract_class_members_metadata() {
    let db = Database::default();

    let source = r#"
public class Test {
    private int field1;
    private String field2, field3;
    
    public Test() {
        // constructor
    }
    
    public void method1() {
        // method 1
    }
    
    private int method2() {
        return 42;
    }
    
    public static void method3() {
        // static method
    }
}
"#;

    let uri = Url::parse("file:///test/Test.java").unwrap();
    let file = SourceFile::new(&db, FileId::new(uri), source.to_string(), Arc::from("java"));

    // Find the class bounds first
    let cursor_offset = 50;
    let bounds = find_enclosing_class_bounds(&db, file, cursor_offset);
    assert!(bounds.is_some(), "Should find class");

    let (_class_name, class_start, class_end) = bounds.unwrap();

    // Extract members metadata
    let metadata = extract_class_members_metadata(&db, file, class_start, class_end);

    // Should count: 1 constructor + 3 methods = 4 methods
    // Should count: field1 + field2 + field3 = 3 fields
    assert_eq!(
        metadata.method_count, 4,
        "Should count 4 methods (including constructor)"
    );
    assert_eq!(metadata.field_count, 3, "Should count 3 fields");
    assert_eq!(metadata.class_start, class_start);
    assert_eq!(metadata.class_end, class_end);
    assert!(
        metadata.content_hash > 0,
        "Should have non-zero content hash"
    );
}

#[test]
fn test_metadata_caching() {
    let db = Database::default();

    let source = r#"
public class Test {
    public void method() {
        int x = 1;
        int y = 2;
    }
}
"#;

    let uri = Url::parse("file:///test/Test.java").unwrap();
    let file = SourceFile::new(&db, FileId::new(uri), source.to_string(), Arc::from("java"));

    let cursor_offset = 50;
    let bounds = find_enclosing_method_bounds(&db, file, cursor_offset).unwrap();

    // First call - cache miss
    let metadata1 = extract_method_locals_metadata(&db, file, bounds.0, bounds.1);

    // Second call - should be cached (same file, same bounds)
    let metadata2 = extract_method_locals_metadata(&db, file, bounds.0, bounds.1);

    assert_eq!(metadata1.local_count, metadata2.local_count);
    assert_eq!(metadata1.content_hash, metadata2.content_hash);
    assert_eq!(metadata1.local_count, 2, "Should count 2 locals");
}

#[test]
fn test_metadata_invalidation_on_content_change() {
    let db = Database::default();

    let source1 = r#"
public class Test {
    public void method() {
        int x = 1;
    }
}
"#;

    let uri = Url::parse("file:///test/Test.java").unwrap();
    let file1 = SourceFile::new(
        &db,
        FileId::new(uri.clone()),
        source1.to_string(),
        Arc::from("java"),
    );

    let cursor_offset = 50;
    let bounds1 = find_enclosing_method_bounds(&db, file1, cursor_offset).unwrap();
    let metadata1 = extract_method_locals_metadata(&db, file1, bounds1.0, bounds1.1);

    // Change the file content (add another local)
    let source2 = r#"
public class Test {
    public void method() {
        int x = 1;
        int y = 2;
    }
}
"#;

    let file2 = SourceFile::new(
        &db,
        FileId::new(uri),
        source2.to_string(),
        Arc::from("java"),
    );

    let bounds2 = find_enclosing_method_bounds(&db, file2, cursor_offset).unwrap();
    let metadata2 = extract_method_locals_metadata(&db, file2, bounds2.0, bounds2.1);

    // Metadata should be different
    assert_ne!(metadata1.local_count, metadata2.local_count);
    assert_ne!(metadata1.content_hash, metadata2.content_hash);
    assert_eq!(
        metadata1.local_count, 1,
        "First version should have 1 local"
    );
    assert_eq!(
        metadata2.local_count, 2,
        "Second version should have 2 locals"
    );
}

#[test]
fn test_incremental_locals_cache_hit() {
    let workspace = Arc::new(Workspace::new());
    let db = workspace.salsa_db.lock();

    let source = r#"
public class Test {
    public void method() {
        int x = 1;
        String y = "hello";
    }
}
"#;

    let uri = Url::parse("file:///test/Test.java").unwrap();
    let file = SourceFile::new(
        &*db,
        FileId::new(uri),
        source.to_string(),
        Arc::from("java"),
    );

    let cursor_offset = 70; // Inside method

    // First check if we can find the method bounds
    let bounds = find_enclosing_method_bounds(&*db, file, cursor_offset);
    assert!(bounds.is_some(), "Should find method bounds");
    let (method_start, method_end) = bounds.unwrap();
    println!("Method bounds: {} to {}", method_start, method_end);
    println!("Method text: {}", &source[method_start..method_end]);

    // First call - cache miss
    let locals1 = extract_method_locals_incremental(&*db, file, cursor_offset, &workspace);
    println!("Extracted {} locals", locals1.len());
    for local in &locals1 {
        println!("  - {}", local.name);
    }
    assert_eq!(locals1.len(), 2, "Should extract 2 local variables");

    // Second call - should be cache hit
    let locals2 = extract_method_locals_incremental(&*db, file, cursor_offset, &workspace);
    assert_eq!(locals2.len(), 2, "Should still have 2 local variables");

    // Verify they're the same
    assert_eq!(locals1[0].name, locals2[0].name);
    assert_eq!(locals1[1].name, locals2[1].name);
}

#[test]
fn test_incremental_locals_cache_invalidation() {
    let workspace = Arc::new(Workspace::new());

    let source1 = r#"
public class Test {
    public void method() {
        int x = 1;
    }
}
"#;

    let uri = Url::parse("file:///test/Test.java").unwrap();
    let file1 = {
        let db = workspace.salsa_db.lock();
        SourceFile::new(
            &*db,
            FileId::new(uri.clone()),
            source1.to_string(),
            Arc::from("java"),
        )
    };

    let cursor_offset = 70;

    // First call
    let locals1 = {
        let db = workspace.salsa_db.lock();
        extract_method_locals_incremental(&*db, file1, cursor_offset, &workspace)
    };
    assert_eq!(locals1.len(), 1, "First version should have 1 local");

    // Change the file content (add another local)
    let source2 = r#"
public class Test {
    public void method() {
        int x = 1;
        int y = 2;
    }
}
"#;

    let file2 = {
        let db = workspace.salsa_db.lock();
        SourceFile::new(
            &*db,
            FileId::new(uri),
            source2.to_string(),
            Arc::from("java"),
        )
    };

    // Second call - should detect change and re-parse
    let locals2 = {
        let db = workspace.salsa_db.lock();
        extract_method_locals_incremental(&*db, file2, cursor_offset, &workspace)
    };
    assert_eq!(locals2.len(), 2, "Second version should have 2 locals");
}

#[test]
fn test_incremental_class_members_cache_hit() {
    let workspace = Arc::new(Workspace::new());
    let db = workspace.salsa_db.lock();

    let source = r#"
public class Test {
    private int field1;
    private String field2;
    
    public void method1() {}
    public void method2() {}
}
"#;

    let uri = Url::parse("file:///test/Test.java").unwrap();
    let file = SourceFile::new(
        &*db,
        FileId::new(uri),
        source.to_string(),
        Arc::from("java"),
    );

    let cursor_offset = 50; // Inside class

    // First call - cache miss
    let members1 = extract_class_members_incremental(&*db, file, cursor_offset, &workspace);
    assert!(
        members1.len() >= 2,
        "Should extract at least 2 methods (may include synthetic members)"
    );

    // Second call - should be cache hit
    let members2 = extract_class_members_incremental(&*db, file, cursor_offset, &workspace);
    assert_eq!(
        members1.len(),
        members2.len(),
        "Should return same number of members from cache"
    );
}

#[test]
fn test_incremental_class_members_cache_invalidation() {
    let workspace = Arc::new(Workspace::new());

    let source1 = r#"
public class Test {
    public void method1() {}
}
"#;

    let uri = Url::parse("file:///test/Test.java").unwrap();
    let file1 = {
        let db = workspace.salsa_db.lock();
        SourceFile::new(
            &*db,
            FileId::new(uri.clone()),
            source1.to_string(),
            Arc::from("java"),
        )
    };

    let cursor_offset = 30;

    // First call
    let members1 = {
        let db = workspace.salsa_db.lock();
        extract_class_members_incremental(&*db, file1, cursor_offset, &workspace)
    };
    let count1 = members1.len();

    // Change the file content (add another method)
    let source2 = r#"
public class Test {
    public void method1() {}
    public void method2() {}
}
"#;

    let file2 = {
        let db = workspace.salsa_db.lock();
        SourceFile::new(
            &*db,
            FileId::new(uri),
            source2.to_string(),
            Arc::from("java"),
        )
    };

    // Second call - should detect change and re-parse
    let members2 = {
        let db = workspace.salsa_db.lock();
        extract_class_members_incremental(&*db, file2, cursor_offset, &workspace)
    };
    let count2 = members2.len();

    assert!(
        count2 > count1,
        "Second version should have more members (added method2)"
    );
}

#[test]
fn test_incremental_enum_members_include_constants() {
    let workspace = Arc::new(Workspace::new());

    let source = r#"
package org.example;

public enum RandomEnum {
    A, B, C;

    public void test() {
        A
    }
}
"#;

    let uri = Url::parse("file:///test/RandomEnum.java").unwrap();
    let file = {
        let db = workspace.salsa_db.lock();
        SourceFile::new(
            &*db,
            FileId::new(uri),
            source.to_string(),
            Arc::from("java"),
        )
    };

    let cursor_offset = source
        .find("A\n    }")
        .expect("cursor marker inside enum method");
    let members = {
        let db = workspace.salsa_db.lock();
        extract_class_members_incremental(&*db, file, cursor_offset, &workspace)
    };

    for constant in ["A", "B", "C"] {
        let member = members
            .iter()
            .find(|member| member.name().as_ref() == constant)
            .unwrap_or_else(|| {
                panic!(
                    "missing enum constant {constant}, members={:?}",
                    members
                        .iter()
                        .map(|member| member.name().to_string())
                        .collect::<Vec<_>>()
                )
            });
        match member {
            CurrentClassMember::Field(field) => {
                assert!(
                    field.access_flags & rust_asm::constants::ACC_STATIC != 0,
                    "{constant} should be static"
                );
                assert_eq!(
                    field.descriptor.as_ref(),
                    "Lorg/example/RandomEnum;",
                    "{constant} should use enum owner descriptor"
                );
            }
            CurrentClassMember::Method(_) => panic!("{constant} should be a field"),
        }
    }
}
