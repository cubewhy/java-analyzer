# Project Overview

## What This Repository Is

`java-analyzer` is a Rust language server backend that targets Java first and ships with a VS Code extension in `editors/code/`.

The server is built around four ideas:

- keep transport and background orchestration in `src/lsp/` and `src/workspace.rs`
- keep Java parsing and editor semantics in `src/language/java/`
- keep workspace-wide lookup in `src/index/`
- use Salsa in `src/salsa_db.rs` and `src/salsa_queries/` for file-scoped incremental work

## Current Product Surface

The backend currently exposes these LSP features:

- text document sync with incremental edits
- completion
- go to definition
- document symbols
- semantic tokens
- inlay hints
- execute commands for memory status and cache clearing

These are not implemented as first-class user features today:

- diagnostics
- references
- rename
- code actions
- formatting
- hover

## Language Scope

### Java

Java is the main product surface. The server has dedicated support for:

- source indexing
- JDK and JAR indexing
- completion providers
- symbol resolution
- semantic tokens
- inlay hints
- decompiler-backed navigation into bytecode
- synthetic member support for records, enums, and several Lombok annotations

### Kotlin

Kotlin exists in the parser and indexing pipeline, but the user-facing experience is intentionally much thinner than Java today.

Current Kotlin support is best understood as:

- source parsing and source-to-index extraction exist
- Salsa query plumbing exists for package/import/symbol-style operations
- no registered completion providers
- no language-specific semantic-token, symbol, or inlay-hint implementation in `Language`

## Workspace Model

The server can operate in two modes:

- managed mode: Gradle or Maven import builds a `WorkspaceModelSnapshot` and the server indexes modules, source roots, and classpaths from that model
- fallback mode: if no supported build is detected, the workspace root is scanned directly for source files

In both modes, open documents are treated as the highest-priority overlay over disk state.

## Storage And Performance Model

The important performance choices are:

- open documents live in memory as immutable `SourceFile` snapshots
- JDK and dependency artifacts are cached in an LMDB-backed store under `src/index/store/`
- dependency classes are lazily materialized through `ArtifactScopeReader`
- request-time lookups run through `IndexView`, which merges visible source buckets, dependency artifacts, and the active JDK
- file-scoped parsing and semantic extraction are memoized through Salsa

## Repository Map

| Path | Purpose |
| --- | --- |
| `src/main.rs` | stdio server entry point |
| `src/lsp/` | LSP transport, capabilities, handlers, request cancellation, commands |
| `src/workspace.rs` | central state hub for documents, Salsa files, indexing, model application, cache management |
| `src/workspace/` | open-document data structures and LSP-side caches |
| `src/language/` | language registry and language-neutral contracts |
| `src/language/java/` | Java parser integration, cursor classification, typing, members, synthetics, inlay hints |
| `src/completion/` | provider trait, engine, scoring, dedup, fuzzy ranking |
| `src/semantic/` | runtime semantic model used by completion and goto |
| `src/salsa_db.rs` | Salsa inputs and side caches for parse trees and extracted classes |
| `src/salsa_queries/` | incremental query layer |
| `src/index/` | workspace index, scope snapshots, artifact cache, source and bytecode indexing |
| `src/build_integration/` | Gradle/Maven detection and workspace-model import |
| `src/decompiler/` | Vineflower/CFR integration and on-disk cache |
| `tests/` | integration and regression tests |
| `benches/` | Criterion benchmarks |
| `editors/code/` | VS Code extension |

## Where To Start For Common Problems

- completion bug: `src/lsp/handlers/completion.rs`, `src/lsp/request_context.rs`, `src/language/java/completion_context.rs`, provider files under `src/language/java/completion/providers/`
- goto-definition bug: `src/lsp/handlers/goto_definition.rs`, `src/semantic/types/symbol_resolver.rs`, `src/language/java/class_parser.rs`
- wrong source/member indexing: `src/workspace.rs`, `src/index/codebase.rs`, `src/language/java/class_parser.rs`, `src/salsa_queries/java/indexing.rs`
- build import issue: `src/build_integration/reload.rs`, `src/build_integration/gradle.rs`, `src/build_integration/maven.rs`, `src/build_integration/model.rs`
- stale or expensive request path: `src/lsp/request_context.rs`, `src/request_metrics.rs`, `src/salsa_queries/`, `src/index/view.rs`
