//! Integration tests for Lombok support
//!
//! These tests verify that Lombok annotations are properly processed during
//! Java source parsing and that synthetic members are correctly generated.

use crate::index::ClassOrigin;
use crate::language::java::class_parser::parse_java_source;

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

        assert_eq!(
            getter.access_flags & 0x0001,
            0x0001,
            "Getter should be public (ACC_PUBLIC flag set)"
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
                private final int id = 1;
            }
        "#;

        let class = parse_first_class(src);

        assert!(
            class.methods.iter().any(|m| m.name.as_ref() == "setName"),
            "Should generate setName() for non-final field"
        );
        assert!(
            !class.methods.iter().any(|m| m.name.as_ref() == "setId"),
            "Should not generate setId() for final field"
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

    /// Test case from user: https://github.com/user/issue
    /// Verifies that getA() is found when using @Getter annotation
    #[test]
    fn original_user_example() {
        let src = r#"
            package org.example;

            import lombok.Getter;

            public class Main {
                @Getter
                private String a;

                public Main(String str) {
                    this.a = str;
                }
            }
        "#;

        let class = parse_first_class(src);

        // Verify getA() method exists
        let has_getter = class.methods.iter().any(|m| m.name.as_ref() == "getA");

        assert!(
            has_getter,
            "getA() method should be generated. Found methods: {:?}",
            class
                .methods
                .iter()
                .map(|m| m.name.as_ref())
                .collect::<Vec<_>>()
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
