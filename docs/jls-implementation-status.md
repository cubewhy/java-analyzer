# Java Feature Coverage

This file is a maintainer-oriented coverage map, not a chapter-by-chapter JLS compliance claim.

## Strong Areas

These areas are central to the current server and should usually be treated as regressions if they break:

- source indexing for classes, fields, methods, constructors, imports, and packages
- JDK and JAR bytecode indexing
- visible-type and visible-member lookup through `IndexView`
- member access, method call, and constructor-call completion
- go to definition for source members, local variables, imported types, and many bytecode targets
- semantic tokens for Java
- inlay hints for `var` and parameter names
- synthetic members for records, enums, and the supported Lombok rule set
- workspace-model import for Gradle and Maven, plus fallback source scanning

## Partial Areas

These areas exist but are intentionally conservative or incomplete:

- deep generic inference and capture conversion
- lambda and method-reference inference
- some modern pattern-matching and switch semantics
- JPMS as a semantic access-control model
- decompiler-backed navigation quality for all bytecode edge cases
- Kotlin as a user-facing language experience

In these areas, "best effort with conservative fallback" is a more accurate mental model than "fully implemented semantics."

## Missing Product Areas

The following are not first-class features in the current backend:

- diagnostics
- references
- rename
- hover
- formatting
- code actions

If a user report sounds like a diagnostics or refactoring feature request, first check whether the feature exists at all before debugging the semantic core.

## Java Version Guidance

The parser and bytecode indexer are intended to work across modern Java versions, but semantic depth varies by feature.

The practical guidance is:

- everyday class/member/import/overload scenarios are the strongest path
- records, enums, and many common post-Java-8 features have targeted support
- newer inference-heavy or pattern-heavy language features are more likely to need follow-up work

## Where To Implement Missing Support

| Problem shape | Start here |
| --- | --- |
| parser or AST shape mismatch | `src/language/java/class_parser.rs`, `src/language/java/location.rs` |
| wrong visible symbol or member | `src/index/view.rs`, `src/semantic/types.rs` |
| wrong cursor semantics | `src/salsa_queries/java/`, `src/language/java/completion_context.rs` |
| wrong expression result type | `src/language/java/expression_typing.rs` |
| missing flow-sensitive behavior | `src/language/java/flow.rs` |
| missing generated member | `src/language/java/synthetic/` or `src/language/java/lombok/` |

## Maintenance Policy

Keep this document high signal:

- update it when a major capability moves from missing to real
- avoid long speculative checklists
- prefer describing the shape of support and the major gaps over trying to claim full JLS coverage
