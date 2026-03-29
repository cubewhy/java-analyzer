# Development Guide

## Prerequisites

- Rust toolchain from `rust-toolchain.toml`
- a JDK if you want JDK indexing or decompiler-backed navigation
- Gradle or Maven only if you are working on build integration
- Node.js and `pnpm` only if you are editing the VS Code extension in `editors/code/`

## Common Commands

```sh
cargo build
cargo test
cargo test --test lsp_integration
cargo bench
RUST_LOG=java_analyzer=debug cargo run
```

For the extension:

```sh
cd editors/code
pnpm install
pnpm compile
```

## Main Edit Workflows

### Add Or Change An LSP Feature

1. Start in `src/lsp/capabilities.rs` and `src/lsp/server.rs`.
2. Put transport mapping in a handler under `src/lsp/handlers/`.
3. Use `PreparedRequest` unless the request is purely administrative.
4. Keep language-specific behavior in `src/language/` or `src/semantic/`, not in the handler.
5. If the editor surface changes, update `editors/code/`.

### Add A Java Completion Case

1. Decide whether the issue is cursor classification, semantic context, or provider output.
2. If the cursor is classified incorrectly, change `src/language/java/location.rs` or related helpers.
3. If context is missing types or expected-type information, change `src/language/java/completion_context.rs` or `src/language/java/editor_semantics.rs`.
4. If the provider is missing candidates, update or add a provider in `src/language/java/completion/providers/`.
5. Register new providers in `src/language/java.rs`.

### Add A Java Semantic Rule

Use this routing table:

- new syntax shape: `src/language/java/location.rs`, `class_parser.rs`, or `scope.rs`
- expression typing: `src/language/java/expression_typing.rs`
- overload or member resolution: `src/semantic/types.rs`
- flow-sensitive narrowing: `src/language/java/flow.rs`
- synthetic members: `src/language/java/synthetic/` and possibly `lombok/`
- request-time shared editor semantics: `src/language/java/editor_semantics.rs`

### Change Workspace Or Build Behavior

1. Build-tool detection and import live in `src/build_integration/`.
2. Full model application and fallback indexing live in `src/workspace.rs`.
3. Source rescans and closed-file updates live in `src/fs_watcher.rs`.
4. Keep the invariant that open documents override imported or scanned disk content.

## Testing Strategy

The repository uses a mix of targeted tests:

- `tests/` for end-to-end and regression coverage
- unit tests inside modules for parser, semantic, and indexing behavior
- `insta` snapshots under `src/**/snapshots/` for semantic and completion stability
- `benches/` for completion and Salsa/index performance experiments

When changing behavior, prefer the smallest test that protects the new invariant.

## Debugging Tools

### Logging

Use `RUST_LOG=java_analyzer=debug cargo run` for broad tracing.

Useful narrower filters include:

```sh
RUST_LOG=java_analyzer::lsp=debug cargo run
RUST_LOG=java_analyzer::salsa_queries=debug cargo run
RUST_LOG=java_analyzer::language::java=trace cargo run
```

### Server Commands

The backend exposes two useful maintenance commands:

- `java-analyzer.server.showMemoryStatus`
- `java-analyzer.server.clearCaches`

The VS Code extension wires user commands to these backend commands.

### Metrics And Hot Paths

`RequestMetrics` is already threaded through prepared requests. If a request is unexpectedly slow, inspect:

- repeated semantic-context extraction
- repeated `IndexView` acquisition
- artifact class/materialization counts
- parse snapshot origin counts

## Refactor Checklist

Before landing a non-trivial refactor, verify these points:

- unsaved file overlays still win over disk and imported model state
- the request still goes through `PreparedRequest` if it needs semantic data
- blocking work did not move onto async code paths
- internal JVM names and source-facing names were not mixed together
- synthetic members still participate in the same semantic paths as explicit members
- the relevant document in `docs/` still matches the new architecture
