//! Integration tests for Lombok support
//!
//! These tests verify that Lombok annotations are properly processed during
//! Java source parsing and that synthetic members are correctly generated.

use crate::index::ClassOrigin;
use crate::language::java::class_parser::parse_java_source;
use rust_asm::constants::{ACC_PUBLIC, ACC_STATIC};

/// Helper function to parse Java source and return the first class
fn parse_first_class(src: &str) -> crate::index::ClassMetadata {
    let classes = parse_java_source(src, ClassOrigin::Unknown, None);
    assert_eq!(classes.len(), 1, "Expected exactly one class");
    classes.into_iter().next().unwrap()
}

mod getter_tests {
    use super::*;

    #[test]
    fn field_level_getter_generates_method() {
        let src = r#"
            package org.example;
            
            import lombok.Getter;
            
            public class Main {
                @Getter
                private String name;
            }
        "#;

        let class = parse_first_class(src);

        assert!(
            class.methods.iter().any(|m| m.name.as_ref() == "getName"),
            "Should generate getName() method"
        );
    }

    #[test]
    fn class_level_getter_generates_methods_for_all_fields() {
        let src = r#"
            package org.example;
            
            import lombok.Getter;
            
            @Getter
            public class Person {
                private String name;
                private int age;
            }
        "#;

        let class = parse_first_class(src);

        assert!(
            class.methods.iter().any(|m| m.name.as_ref() == "getName"),
            "Should generate getName() method"
        );
        assert!(
            class.methods.iter().any(|m| m.name.as_ref() == "getAge"),
            "Should generate getAge() method"
        );
    }

    #[test]
    fn boolean_field_uses_is_prefix() {
        let src = r#"
            package org.example;
            
            import lombok.Getter;
            
            public class Main {
                @Getter
                private boolean active;
            }
        "#;

        let class = parse_first_class(src);

        assert!(
            class.methods.iter().any(|m| m.name.as_ref() == "isActive"),
            "Boolean field should generate isActive() method"
        );
    }

    #[test]
    fn getter_is_public_by_default() {
        let src = r#"
            package org.example;
            
            import lombok.Getter;
            
            public class Person {
                @Getter
                private String name;
            }
        "#;

        let class = parse_first_class(src);

        let getter = class
            .methods
            .iter()
            .find(|m| m.name.as_ref() == "getName")
            .expect("getName() should be generated");

        assert_eq!(
            getter.access_flags & 0x0001,
            0x0001,
            "getName() should be public"
        );
    }

    #[test]
    fn static_field_with_field_level_getter() {
        let src = r#"
            package org.example;
            
            import lombok.Getter;
            
            public class MyConfig {
                @Getter
                private static final String randomStringField = "Hello";
            }
        "#;

        let class = parse_first_class(src);

        // Should generate static getter
        let getter = class
            .methods
            .iter()
            .find(|m| m.name.as_ref() == "getRandomStringField");

        assert!(
            getter.is_some(),
            "Should generate getter for static field with field-level @Getter"
        );

        let method = getter.unwrap();

        // Verify it's static
        assert_eq!(
            method.access_flags & rust_asm::constants::ACC_STATIC,
            rust_asm::constants::ACC_STATIC,
            "Getter for static field should be static"
        );

        // Verify it's public
        assert_eq!(
            method.access_flags & rust_asm::constants::ACC_PUBLIC,
            rust_asm::constants::ACC_PUBLIC,
            "Getter should be public"
        );

        // Verify return type (accept both qualified and unqualified forms)
        let return_type = method.return_type.as_ref().map(|t| t.as_ref());
        assert!(
            return_type == Some("Ljava/lang/String;") || return_type == Some("LString;"),
            "Should return String, got: {:?}",
            return_type
        );
    }

    #[test]
    fn static_field_skipped_with_class_level_getter() {
        let src = r#"
            package org.example;
            
            import lombok.Getter;
            
            @Getter
            public class MyConfig {
                private String instanceField;
                private static String staticField = "Hello";
            }
        "#;

        let class = parse_first_class(src);

        // Should generate getter for instance field
        assert!(
            class
                .methods
                .iter()
                .any(|m| m.name.as_ref() == "getInstanceField"),
            "Should generate getter for instance field"
        );

        // Should NOT generate getter for static field
        assert!(
            !class
                .methods
                .iter()
                .any(|m| m.name.as_ref() == "getStaticField"),
            "Should NOT generate getter for static field with class-level @Getter"
        );
    }

    #[test]
    fn static_final_field_with_getter() {
        let src = r#"
            package org.example;
            
            import lombok.Getter;
            
            public class Constants {
                @Getter
                private static final int MAX_SIZE = 100;
                
                @Getter
                private static final String APP_NAME = "MyApp";
            }
        "#;

        let class = parse_first_class(src);

        // Should generate static getters for both fields
        // Note: Lombok preserves the exact field name for all-caps constants
        let max_size_getter = class
            .methods
            .iter()
            .find(|m| m.name.as_ref() == "getMAX_SIZE");

        let app_name_getter = class
            .methods
            .iter()
            .find(|m| m.name.as_ref() == "getAPP_NAME");

        assert!(max_size_getter.is_some(), "Should generate getMAX_SIZE()");
        assert!(app_name_getter.is_some(), "Should generate getAPP_NAME()");

        // Verify both are static
        assert_eq!(
            max_size_getter.unwrap().access_flags & ACC_STATIC,
            ACC_STATIC,
            "getMAX_SIZE() should be static"
        );

        assert_eq!(
            app_name_getter.unwrap().access_flags & ACC_STATIC,
            ACC_STATIC,
            "getAPP_NAME() should be static"
        );
    }

    #[test]
    fn getter_has_correct_return_type() {
        let src = r#"
            package org.example;
            
            import lombok.Getter;
            
            public class Main {
                @Getter
                private String name;
            }
        "#;

        let class = parse_first_class(src);
        let getter = class
            .methods
            .iter()
            .find(|m| m.name.as_ref() == "getName")
            .expect("getName() should exist");

        assert!(
            getter.return_type.is_some(),
            "Getter should have a return type"
        );
    }
}

mod setter_tests {
    use super::*;

    #[test]
    fn field_level_setter_generates_method() {
        let src = r#"
            package org.example;
            
            import lombok.Setter;
            
            public class Main {
                @Setter
                private String name;
            }
        "#;

        let class = parse_first_class(src);

        assert!(
            class.methods.iter().any(|m| m.name.as_ref() == "setName"),
            "Should generate setName() method"
        );
    }

    #[test]
    fn setter_has_one_parameter() {
        let src = r#"
            package org.example;
            
            import lombok.Setter;
            
            public class Main {
                @Setter
                private String name;
            }
        "#;

        let class = parse_first_class(src);
        let setter = class
            .methods
            .iter()
            .find(|m| m.name.as_ref() == "setName")
            .expect("setName() should exist");

        assert_eq!(
            setter.params.items.len(),
            1,
            "Setter should have exactly one parameter"
        );
    }

    #[test]
    fn setter_not_generated_for_final_field() {
        let src = r#"
            package org.example;
            
            import lombok.Setter;
            
            public class Main {
                @Setter
                private final String name = "John";
            }
        "#;

        let class = parse_first_class(src);

        assert!(
            !class.methods.iter().any(|m| m.name.as_ref() == "setName"),
            "Setter should not be generated for final field"
        );
    }

    #[test]
    fn class_level_setter_skips_final_fields() {
        let src = r#"
            package org.example;
            
            import lombok.Setter;
            
            @Setter
            public class Person {
                private String name;
                private final int age = 25;
            }
        "#;

        let class = parse_first_class(src);

        // Should generate setter for name
        assert!(
            class.methods.iter().any(|m| m.name.as_ref() == "setName"),
            "Should generate setName() for non-final field"
        );

        // Should NOT generate setter for age (final)
        assert!(
            !class.methods.iter().any(|m| m.name.as_ref() == "setAge"),
            "Should NOT generate setAge() for final field"
        );
    }

    #[test]
    fn static_field_with_field_level_setter() {
        let src = r#"
            package org.example;
            
            import lombok.Setter;
            
            public class Config {
                @Setter
                private static String configValue = "default";
            }
        "#;

        let class = parse_first_class(src);

        // Should generate static setter
        let setter = class
            .methods
            .iter()
            .find(|m| m.name.as_ref() == "setConfigValue");

        assert!(
            setter.is_some(),
            "Should generate setConfigValue() for static field with field-level @Setter"
        );

        let method = setter.unwrap();

        // Verify it's static
        assert_eq!(
            method.access_flags & ACC_STATIC,
            ACC_STATIC,
            "setConfigValue() should be static"
        );

        // Verify it's public
        assert_eq!(
            method.access_flags & ACC_PUBLIC,
            ACC_PUBLIC,
            "setConfigValue() should be public"
        );

        // Verify it has one parameter
        assert_eq!(method.params.len(), 1, "Setter should have one parameter");
    }

    #[test]
    fn static_field_skipped_with_class_level_setter() {
        let src = r#"
            package org.example;
            
            import lombok.Setter;
            
            @Setter
            public class Config {
                private String instanceField;
                private static String staticField = "default";
            }
        "#;

        let class = parse_first_class(src);

        // Should generate setter for instance field
        assert!(
            class
                .methods
                .iter()
                .any(|m| m.name.as_ref() == "setInstanceField"),
            "Should generate setter for instance field"
        );

        // Should NOT generate setter for static field
        assert!(
            !class
                .methods
                .iter()
                .any(|m| m.name.as_ref() == "setStaticField"),
            "Should NOT generate setter for static field with class-level @Setter"
        );
    }

    #[test]
    fn static_final_field_no_setter() {
        let src = r#"
            package org.example;
            
            import lombok.Setter;
            
            public class Constants {
                @Setter
                private static final String CONSTANT = "value";
            }
        "#;

        let class = parse_first_class(src);

        // Should NOT generate setter for static final field
        assert!(
            !class
                .methods
                .iter()
                .any(|m| m.name.as_ref() == "setCONSTANT"),
            "Should NOT generate setter for static final field"
        );
    }
}

mod annotation_resolution_tests {
    use super::*;

    #[test]
    fn simple_annotation_name_is_resolved() {
        let src = r#"
            package org.example;
            
            import lombok.Getter;
            
            public class Main {
                @Getter
                private String a;
            }
        "#;

        let class = parse_first_class(src);

        // Verify field has annotation
        assert_eq!(class.fields.len(), 1, "Should have one field");
        assert_eq!(
            class.fields[0].annotations.len(),
            1,
            "Field should have one annotation"
        );

        // Verify getter was generated
        assert!(
            class.methods.iter().any(|m| m.name.as_ref() == "getA"),
            "Getter should be generated from simple annotation name"
        );
    }

    #[test]
    fn qualified_annotation_name_works() {
        let src = r#"
            package org.example;
            
            public class Main {
                @lombok.Getter
                private String a;
            }
        "#;

        let class = parse_first_class(src);

        assert!(
            class.methods.iter().any(|m| m.name.as_ref() == "getA"),
            "Getter should be generated from qualified annotation name"
        );
    }
}

mod user_reported_issues {
    use super::*;

    #[test]
    fn original_user_example() {
        let src = r#"
            package org.example;
            
            import lombok.Getter;
            
            public class Main {
                @Getter
                private String a;
            }
        "#;

        let class = parse_first_class(src);

        // Verify getA() is generated
        assert!(
            class.methods.iter().any(|m| m.name.as_ref() == "getA"),
            "getA() should be generated"
        );

        // Verify it's accessible (public)
        let getter = class
            .methods
            .iter()
            .find(|m| m.name.as_ref() == "getA")
            .unwrap();
        assert_eq!(
            getter.access_flags & 0x0001,
            0x0001,
            "getA() should be public"
        );
    }

    #[test]
    fn static_field_getter_issue() {
        // User reported: static fields with @Getter should generate static getters
        let src = r#"
            package org.example;
            
            import lombok.Getter;
            
            public class MyConfig {
                @Getter
                private static final String randomStringField = "Hello";
            }
        "#;

        let class = parse_first_class(src);

        // Should generate static getter
        let getter = class
            .methods
            .iter()
            .find(|m| m.name.as_ref() == "getRandomStringField");

        assert!(
            getter.is_some(),
            "Should generate getRandomStringField() for static field with @Getter"
        );

        let method = getter.unwrap();

        // Verify it's static
        assert_eq!(
            method.access_flags & ACC_STATIC,
            ACC_STATIC,
            "getRandomStringField() should be static"
        );

        // Verify it's public
        assert_eq!(
            method.access_flags & ACC_PUBLIC,
            ACC_PUBLIC,
            "getRandomStringField() should be public"
        );
    }

    #[test]
    fn static_field_setter_issue() {
        // Static fields with @Setter should generate static setters
        let src = r#"
            package org.example;
            
            import lombok.Setter;
            
            public class MyConfig {
                @Setter
                private static String configValue = "default";
            }
        "#;

        let class = parse_first_class(src);

        // Should generate static setter
        let setter = class
            .methods
            .iter()
            .find(|m| m.name.as_ref() == "setConfigValue");

        assert!(
            setter.is_some(),
            "Should generate setConfigValue() for static field with @Setter"
        );

        let method = setter.unwrap();

        // Verify it's static
        assert_eq!(
            method.access_flags & ACC_STATIC,
            ACC_STATIC,
            "setConfigValue() should be static"
        );

        // Verify it's public
        assert_eq!(
            method.access_flags & ACC_PUBLIC,
            ACC_PUBLIC,
            "setConfigValue() should be public"
        );
    }

    #[test]
    fn static_field_getter_and_setter() {
        // Test both @Getter and @Setter on the same static field
        let src = r#"
            package org.example;
            
            import lombok.Getter;
            import lombok.Setter;
            
            public class MyConfig {
                @Getter
                @Setter
                private static String sharedConfig = "default";
            }
        "#;

        let class = parse_first_class(src);

        // Should generate static getter
        let getter = class
            .methods
            .iter()
            .find(|m| m.name.as_ref() == "getSharedConfig");

        assert!(getter.is_some(), "Should generate static getter");
        assert_eq!(
            getter.unwrap().access_flags & ACC_STATIC,
            ACC_STATIC,
            "Getter should be static"
        );

        // Should generate static setter
        let setter = class
            .methods
            .iter()
            .find(|m| m.name.as_ref() == "setSharedConfig");

        assert!(setter.is_some(), "Should generate static setter");
        assert_eq!(
            setter.unwrap().access_flags & ACC_STATIC,
            ACC_STATIC,
            "Setter should be static"
        );
    }
}

mod to_string_tests {
    use super::*;

    #[test]
    fn class_level_to_string_generates_method() {
        let src = r#"
            package org.example;
            
            import lombok.ToString;
            
            @ToString
            public class Person {
                private String name;
                private int age;
            }
        "#;

        let class = parse_first_class(src);

        let to_string = class.methods.iter().find(|m| m.name.as_ref() == "toString");

        assert!(to_string.is_some(), "Should generate toString() method");

        let method = to_string.unwrap();
        assert_eq!(
            method.return_type.as_ref().map(|t| t.as_ref()),
            Some("java/lang/String"),
            "toString() should return String"
        );
        assert!(
            method.params.is_empty(),
            "toString() should have no parameters"
        );
    }

    #[test]
    fn to_string_with_exclude() {
        let src = r#"
            package org.example;
            
            import lombok.ToString;
            
            @ToString(exclude = "password")
            public class User {
                private String username;
                private String password;
            }
        "#;

        let class = parse_first_class(src);

        assert!(
            class.methods.iter().any(|m| m.name.as_ref() == "toString"),
            "Should generate toString() method"
        );
    }

    #[test]
    fn to_string_with_of() {
        let src = r#"
            package org.example;
            
            import lombok.ToString;
            
            @ToString(of = {"name", "email"})
            public class User {
                private String name;
                private String email;
                private String password;
            }
        "#;

        let class = parse_first_class(src);

        assert!(
            class.methods.iter().any(|m| m.name.as_ref() == "toString"),
            "Should generate toString() method"
        );
    }

    #[test]
    fn to_string_does_not_override_existing() {
        let src = r#"
            package org.example;
            
            import lombok.ToString;
            
            @ToString
            public class Person {
                private String name;
                
                @Override
                public String toString() {
                    return "Custom: " + name;
                }
            }
        "#;

        let class = parse_first_class(src);

        // Should have exactly one toString method (the explicit one)
        let to_string_count = class
            .methods
            .iter()
            .filter(|m| m.name.as_ref() == "toString")
            .count();

        assert_eq!(
            to_string_count, 1,
            "Should not generate toString() when it already exists"
        );
    }

    #[test]
    fn to_string_skips_static_fields() {
        let src = r#"
            package org.example;
            
            import lombok.ToString;
            
            @ToString
            public class Config {
                private String name;
                private static String DEFAULT_NAME = "default";
            }
        "#;

        let class = parse_first_class(src);

        assert!(
            class.methods.iter().any(|m| m.name.as_ref() == "toString"),
            "Should generate toString() method"
        );
    }

    #[test]
    fn to_string_with_call_super() {
        let src = r#"
            package org.example;
            
            import lombok.ToString;
            
            @ToString(callSuper = true)
            public class Employee extends Person {
                private String employeeId;
            }
        "#;

        let class = parse_first_class(src);

        assert!(
            class.methods.iter().any(|m| m.name.as_ref() == "toString"),
            "Should generate toString() method with callSuper"
        );
    }

    #[test]
    fn to_string_is_public() {
        let src = r#"
            package org.example;
            
            import lombok.ToString;
            
            @ToString
            public class Person {
                private String name;
            }
        "#;

        let class = parse_first_class(src);

        let to_string = class
            .methods
            .iter()
            .find(|m| m.name.as_ref() == "toString")
            .expect("toString() should be generated");

        assert_eq!(
            to_string.access_flags & 0x0001,
            0x0001,
            "toString() should be public"
        );
    }

    #[test]
    fn to_string_comprehensive_example() {
        let src = r#"
            package com.example;
            
            import lombok.ToString;
            
            @ToString(exclude = {"password", "internalId"})
            public class User {
                private String username;
                private String email;
                private String password;
                private long internalId;
                private boolean active;
                private static String DEFAULT_ROLE = "user";
            }
        "#;

        let class = parse_first_class(src);

        // Should generate toString()
        let to_string = class.methods.iter().find(|m| m.name.as_ref() == "toString");

        assert!(to_string.is_some(), "Should generate toString() method");

        let method = to_string.unwrap();

        // Verify signature
        assert_eq!(
            method.return_type.as_ref().map(|t| t.as_ref()),
            Some("java/lang/String"),
            "toString() should return String"
        );
        assert!(
            method.params.is_empty(),
            "toString() should have no parameters"
        );
        assert_eq!(
            method.access_flags & 0x0001,
            0x0001,
            "toString() should be public"
        );
    }
}
