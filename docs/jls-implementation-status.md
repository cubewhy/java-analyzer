# JLS Implementation Status

This document tracks which parts of the Java Language Specification (JLS, Java SE 21 edition) are implemented, partially implemented, or not yet implemented in java-analyzer.

Legend:
- **Done** — implemented and tested
- **Partial** — implemented with known gaps or caveats
- **Planned** — design exists, not yet coded
- **Not implemented** — out of scope or not started

---

## Chapter 3 — Lexical Structure

| Rule | Status | Notes |
|---|---|---|
| Unicode source files | Done | tree-sitter handles UTF-8 |
| Integer / floating-point literals | Done | literal type inference in `TypeResolver` |
| String literals | Done | typed as `java/lang/String` |
| Text blocks (JLS 3.10.6) | Done | parsed by tree-sitter-java |
| `null` literal | Done | typed as `null` (reference-compatible) |

## Chapter 4 — Types, Values, Variables

| Rule | Status | Notes |
|---|---|---|
| Primitive types (§4.2) | Done | All 8 primitives recognized and typed |
| Reference types (§4.3) | Done | Class, interface, array types |
| Type variables (§4.4) | Partial | Recognized in generic signatures; full capture conversion not done |
| Parameterized types (§4.5) | Partial | `TypeName` preserves generic args; wildcard bounds are coarse |
| Raw types (§4.8) | Done | Raw type access falls back to erased member lookup |
| Intersection types (§4.9) | Partial | Implemented with minor issues. |
| Array types (§4.10.1) | Done | Array depth tracked in `TypeName`; `.length` and `clone()` as intrinsics |
| `null` type (§4.1) | Done | `null` expression returns null-compatible type |
| Boxing / unboxing (§4.3.2) | Done | `ConversionKind::Boxing` / `Unboxing` in overload scoring |
| Widening / narrowing conversions (§4.2.2) | Done | All primitive widening chains modeled in `ConversionKind` |

## Chapter 5 — Conversions and Contexts

| Rule | Status | Notes |
|---|---|---|
| Assignment contexts | Done | Expected type propagated from LHS into RHS for completion |
| Invocation contexts | Done | Method argument expected-type propagated per overload selection |
| Casting contexts | Partial | Cast target type recognized; no cast validity checking |
| String conversion (§5.4) | Not implemented | No diagnostics; string concatenation result typed as `String` |
| Numeric promotions (§5.6) | Done | Binary/unary/shift promotion results modeled in `expression_typing.rs` |

## Chapter 6 — Names

| Rule | Status | Notes |
|---|---|---|
| Simple name resolution | Done | Local vars, fields, params resolved via `LocalVar` list + `ThisMemberProvider` |
| Qualified name resolution | Done | Dotted chains resolved via `evaluate_chain()` in `TypeResolver` |
| Package name resolution | Done | Package prefix completion via `PackageProvider` |
| Ambiguous names | Done |  |
| Shadowing (§6.4.1) | Partial | Innermost local shadows field; lambda param shadowing handled |
| Obscuring | Not implemented | No diagnostics for obscured names |

## Chapter 7 — Packages and Modules

| Rule | Status | Notes |
|---|---|---|
| Package declarations | Done | Parsed from source; used for `NameTable` construction |
| Single-type-import (§7.5.1) | Done | Resolved to internal name in completion context |
| Type-import-on-demand (§7.5.2) | Done | Wildcard imports resolved against `IndexView` |
| Static-single-type import (§7.5.3) | Done | `import static Foo.bar` resolved |
| Static-import-on-demand (§7.5.4) | Done | `import static Foo.*` resolved |
| Module declarations (§7.7) | Not implemented | JPMS modules not modeled; JDK modules loaded as flat class pool |

## Chapter 8 — Classes

| Rule | Status | Notes |
|---|---|---|
| Class declarations | Done | Indexed from source and bytecode |
| Field declarations | Done | Fields in `FieldSummary`; type, access flags, annotations captured |
| Method declarations | Done | Methods in `MethodSummary`; params with names, descriptors, generics |
| Constructor declarations | Done | `<init>` mapped to constructor completion |
| Static initializers | Done | Parsed; `static` context detected for completion |
| Instance initializers | Done | Parsed |
| Nested classes (§8.1.3) | Done | `inner_class_of` tracked; `$`-name mapping in `source_name()` |
| Enums (§8.9) | Done | Constants indexed as fields; `values()` / `ordinal()` as intrinsics |
| Records (§8.10, Java 16+) | Done | Parsed; accessor methods not auto-generated in index |
| Sealed classes (§8.1.1.2, Java 17+) | Not implemented | `sealed`/`permits` parsed but not enforced |
| Generic classes (§8.1.2) | Partial | Type parameters parsed from `Signature` attribute; substitution partial |
| Method overloading resolution (§8.4.9) | Done | Full fixed-arity + varargs overload selection with scoring |
| Varargs (§8.4.1) | Done | `ACC_VARARGS` flag; varargs call sites handled in overload matching |
| `this()` / `super()` constructor calls | Partial | Parsed; completion context detects constructor body |
| Inheritance (§8.4.8) | Done | MRO walk via `collect_inherited_members`; interfaces included |
| Abstract methods | Done | `ACC_ABSTRACT` respected in member filtering |
| `@Override` | Done | `OverrideProvider` generates override stubs |

## Chapter 9 — Interfaces

| Rule | Status | Notes |
|---|---|---|
| Interface declarations | Done | Indexed; members available via MRO |
| Default methods (§9.4.3) | Done | Indexed; appear in member completion |
| Static interface methods | Done | Available via `StaticMemberProvider` |
| Functional interfaces (§9.8) | Done | SAM extraction implemented; used for lambda/method-ref completion |
| Annotation types (§9.6) | Done | `@Target`, `@Retention` parsed; annotation element completion via `AnnotationProvider` |
| `@interface` element defaults | Partial | Elements indexed; default values not tracked |

## Chapter 10 — Arrays

| Rule | Status | Notes |
|---|---|---|
| Array types | Done | Multi-dim arrays tracked in `TypeName` |
| Array access | Done | `expr[idx]` → component type in `resolve_expression_type` |
| Array creation | Done | `new T[n]` typed correctly |
| `.length` field | Done | Intrinsic in `IntrinsicMemberProvider` |
| `clone()` method | Done | Intrinsic producing same array type |
| Array covariance | Not implemented | No subtype checking |

## Chapter 14 — Blocks and Statements

| Rule | Status | Notes |
|---|---|---|
| Local variable declarations | Done | Indexed into `LocalVar` list by `locals.rs` |
| `var` type inference (§14.4, Java 10+) | Done | Initializer expression typed; inlay hint shows resolved type |
| Enhanced `for` loop variable | Done | Element type extracted from iterable |
| `try`-with-resources variable | Partial | Resource variable recognized; type inferred from initializer |
| `switch` expressions (§14.12, Java 14+) | Partial | Parsed; expression type not fully inferred |
| Pattern matching `instanceof` (§14.16.2, Java 16+) | Done | Narrowing facts extracted in `flow.rs`; type override applied in scope |
| `break` / `continue` with labels | Done | `StatementLabelProvider` tracks visible labels |
| `return` statement | Done | Return type context used in expected-type propagation |

## Chapter 15 — Expressions

| Rule | Status | Notes |
|---|---|---|
| Field access (§15.11) | Done | `expr.field` → field type via MRO |
| Method invocation (§15.12) | Done | Overload selected; return type substituted with generics |
| Constructor invocation `new` (§15.9) | Done | Constructor completion; type inferred as constructed type |
| Array access (§15.10.3) | Done | Component type propagation |
| Postfix/prefix operators | Done | Numeric promotion result typed |
| Binary operators (§15.17–15.24) | Done | Arithmetic, bitwise, shift results typed with promotion rules |
| String concatenation `+` | Done | Result typed as `java/lang/String` |
| Ternary operator (§15.25) | Partial | Branches not unified; first branch type used |
| Cast expressions (§15.16) | Done | Target type used as expression type |
| `instanceof` (§15.20.2) | Done | Result `boolean`; pattern variable captured by flow |
| Lambda expressions (§15.27) | Partial | SAM parameter types used; body type not fully inferred |
| Method references (§15.13) | Partial | Kind recognized (`Type::method`, `expr::method`, `Type::new`); constraint solving shallow |
| Class literals `Foo.class` (§15.8.2) | Done | Typed as `Class<Foo>` via intrinsic |
| `this` / `super` (§15.8.3–4) | Partial | `this`, bare `super`, `super.member`, and `super::method` resolve; qualified forms like `Outer.super` / `InterfaceName.super` are not modeled yet |
| Generic method invocation | Partial | Type arguments parsed; explicit type arg not forced on completion |

## Chapter 18 — Type Inference (Generics)

| Rule | Status | Notes |
|---|---|---|
| Class type parameter substitution | Done | `parse_class_type_parameters` + `substitute_type_vars` |
| Method type parameter substitution | Done | `parse_method_type_parameters` + `substitute_type` on return type |
| Wildcard types `? extends T` / `? super T` | Partial | Bounds preserved in `TypeName`; no full capture conversion |
| Diamond operator `<>` (§15.9.3) | Not implemented | Constructor type args not inferred |
| Full JLS §18 constraint solving | Not implemented | Poly expressions, inference variables (`α`) not modeled |
| Lambda / method-reference constraint solving | Not implemented | Shallow SAM compatibility only (parameter count + shape) |

---

## Java Version Feature Support

| Java Version | Feature | Status |
|---|---|---|
| Java 5 | Generics | Partial (see §18 above) |
| Java 5 | Annotations | Done |
| Java 5 | Enhanced for loop | Done |
| Java 5 | Varargs | Done |
| Java 7 | Diamond operator | Not implemented |
| Java 7 | try-with-resources | Partial |
| Java 8 | Lambda expressions | Partial |
| Java 8 | Method references | Partial |
| Java 8 | Default / static interface methods | Done |
| Java 8 | Stream API (library) | Done (via indexing) |
| Java 9 | Module system (JPMS) | Not implemented |
| Java 10 | `var` local type inference | Done |
| Java 14 | `switch` expressions | Partial |
| Java 14 | Records (preview) | Done |
| Java 15 | Text blocks | Done |
| Java 16 | Records (final) | Done |
| Java 16 | `instanceof` pattern matching | Done |
| Java 17 | Sealed classes | Not implemented |
| Java 21 | Pattern matching in `switch` | Not implemented |
| Java 21 | Virtual threads (library) | Done (via indexing) |
| Java 25 | (Previews) | Parsed; semantics not modeled |

---

## Summary

| Area | Coverage |
|---|---|
| Name resolution | High |
| Type representation | High |
| Overload resolution | High |
| Generic substitution | Medium |
| Type inference (full JLS §18) | Low |
| Lambda / method-ref constraint solving | Low |
| Diagnostics / error reporting | None |
| Data-flow analysis | Minimal (instanceof narrowing only) |
| JPMS module system | None |
