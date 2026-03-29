# Java Inlay Hints Design

## Current Scope

Java inlay hints currently provide:

- inferred type hints for `var`
- parameter name hints for method and constructor calls

The implementation is intentionally thin. It reuses the same semantic helpers that completion and goto definition rely on instead of building a second type-inference path.

## Request Path

The LSP path is:

1. `src/lsp/handlers/inlay_hints.rs`
2. `JavaLanguage::collect_inlay_hints_with_tree` in `src/language/java.rs`
3. `collect_java_inlay_hints` in `src/language/java/inlay_hints.rs`
4. shared semantic lookup through `JavaSemanticRequestContext`

`JavaSemanticRequestContext` gives the inlay-hint code access to:

- cached semantic contexts
- request cancellation
- request metrics
- optional Salsa-backed context extraction

## Var Hint Pipeline

Type hints for `var` work like this:

1. walk the tree for `local_variable_declaration` nodes with type text `var`
2. build semantic context at the declarator boundary
3. find the resolved local variable in `ctx.local_variables`
4. render the resolved `TypeName` through `render_type_for_ui`

The key design choice is that the hint does not infer the type by re-reading the initializer directly in the inlay-hint code. It asks the shared semantic layer for the local variable that was already materialized for editor features.

## Parameter Hint Pipeline

Parameter hints work like this:

1. identify invocation syntax sites
2. collect named argument nodes
3. build semantic context at the call site
4. call `resolve_invocation` from `src/language/java/editor_semantics.rs`
5. map each argument position to the resolved parameter name
6. skip obviously redundant hints

This is important because parameter hints depend on the same overload-selection behavior that completion and expected-type propagation use.

## Shared Dependencies

The inlay-hint feature depends on these shared layers:

- `ContextEnricher` for expected-type and receiver enrichment
- `editor_semantics.rs` for call resolution and type rendering
- `TypeResolver` for overload matching and descriptor-to-type conversion
- `IndexView` for visible members and types
- `SourceTypeCtx` for import and package-aware source-type normalization

## Maintenance Rules

- Do not add a parallel expression-typing implementation just for hints.
- Prefer adding a new shared helper in `editor_semantics.rs` if both hints and another editor feature need it.
- Keep syntax collection local to `inlay_hints.rs`, but keep semantic decisions in shared helpers.
- Preserve cancellation checks in recursive tree walks and argument loops.
- Preserve byte-range filtering so range requests stay cheap on large files.

## Where To Extend

- new hint kind based on locals or enclosing type context: start in `inlay_hints.rs`
- new invocation-resolution capability: start in `editor_semantics.rs`
- new expression-type rule needed by hints and completion: start in `expression_typing.rs`
- new rendering rule: start in `render.rs`

## Refactor Hazard

The easy way to break this feature is to make inlay hints "work" from a local syntax heuristic while drifting away from the completion/goto semantics. When in doubt, make the shared semantic layer smarter instead of making the hint layer more ad hoc.
