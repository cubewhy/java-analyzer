# Architecture

## System At A Glance

```text
editor / VS Code extension
        |
        v
tower-lsp Backend
        |
        v
Workspace
  |- DocumentStore           open document snapshots + LSP caches
  |- Salsa database          file-scoped incremental queries
  |- WorkspaceIndexHandle    published workspace index + optional model snapshot
  |- build services          Gradle / Maven import loop
  |- source watch service    filesystem updates for closed files
  |- decompiler cache        decompiled source files on disk
```

The core architectural split is:

- `src/lsp/` owns protocol, request lifecycle, and background orchestration
- `src/workspace.rs` owns mutable application state and index publication
- `src/index/` owns read-side lookup across source, dependencies, and JDK
- `src/language/java/`, `src/semantic/`, and `src/salsa_queries/` own Java analysis

## Major State Holders

| Type | Owns | Notes |
| --- | --- | --- |
| `Backend` | client handle, config, services, request-cancellation manager | created by `tower-lsp` in `src/main.rs` |
| `Workspace` | documents, Salsa DB, published index, current workspace root, semantic caches | the main state hub |
| `Document` | current `SourceFile` plus semantic-token and semantic-context caches | only for open files |
| `salsa_db::SourceFile` | Salsa input for content and language id | separate from `workspace::SourceFile` |
| `WorkspaceIndex` | source buckets, classpaths, JDK state, scope cache | request-time read model |
| `WorkspaceModelSnapshot` | imported Gradle/Maven module graph | optional, published with the index |
| `IndexView` | request-scoped lookup view over visible layers | cheap clone, heavy lazy caches inside |

## Runtime Flows

### 1. Startup

`src/main.rs` starts `tower-lsp`, which constructs `Backend`.

`Backend::new` initializes:

- an empty `Workspace`
- the completion engine
- the language registry
- request cancellation
- the decompiler cache root

### 2. Initialization And Workspace Discovery

During initialization the backend:

- records the workspace root
- optionally indexes the configured JDK
- starts build integration
- starts source watching

Build integration is optimistic:

- if Gradle or Maven is detected, `BuildIntegrationService` imports a `WorkspaceModelSnapshot`
- if no supported build is detected, `Workspace::index_fallback_root` scans the workspace directly

### 3. Document Write Path

Open/change/save events update the open-document view first.

The important write-path rules are:

- `DocumentStore` is the source of truth for open files
- `Workspace` mirrors open files into Salsa inputs
- parse trees from the LSP document path are seeded into the Salsa side cache when available
- the current file is re-extracted and overlaid on top of the published index for read requests

This is why requests can see unsaved edits without rebuilding the whole workspace index.

### 4. Read Request Path

Most read requests go through `PreparedRequest` in `src/lsp/request_context.rs`.

That preparation step:

- finds the language implementation
- ensures the document has a tree
- resolves module, classpath, and source-root context from the current model
- obtains an `IndexView` for that analysis context through Salsa-backed helpers
- extracts classes for the current open file and prepends them as an overlay layer
- caches semantic contexts per document version and workspace version

After that, feature code operates almost entirely on:

- `SourceFile`
- `IndexView`
- `SemanticContext`
- request cancellation and metrics

### 5. Full Workspace Rebuild Path

`Workspace::apply_workspace_model` is the main rebuild path.

It:

- builds a fresh `WorkspaceIndex`
- replays imported modules, classpaths, and dependencies into the new index
- indexes source roots
- reapplies open-document overlays
- swaps the whole published index and model together through `WorkspaceIndexHandle`

The swap is important: requests always read from a coherent snapshot instead of watching the index mutate in place.

## Index Layering Model

`IndexView` reads through a `ScopeSnapshot`.

The visible layers, in priority order, are effectively:

1. current-request source overlay for the open file
2. source-root buckets for the active module and visible classpath
3. dependency artifacts or in-memory classpath overlays
4. active JDK artifact

Two consequences matter during maintenance:

- source should win over dependency bytecode for the same internal name
- request code should prefer `IndexView` instead of directly reaching into `WorkspaceIndex`

## Persistence And Lazy Materialization

The persistent artifact path is:

- JDK/JAR/source archive indexed into `IndexedArchiveData`
- data stored in LMDB through `src/index/store/`
- lightweight archive stubs loaded back
- full `ClassMetadata` materialized only on demand through `ArtifactScopeReader`

This lets the server keep broad dependency coverage without holding every class as a fully materialized Rust object at all times.

## Background Services

| Service | Responsibility |
| --- | --- |
| `BuildIntegrationService` | detect build tool, debounce reloads, import workspace model |
| `SourceWatchService` | apply filesystem changes for closed files and trigger rescans on watcher failure |
| `RequestCancellationManager` | cancel superseded requests per `(family, uri)` |
| `DecompilerCache` | store decompiled source by backend and class byte content |
| `RequestMetrics` | capture request-time hot spots and parse/index reuse |

## Concurrency Model

The codebase deliberately mixes async orchestration with blocking analysis:

- async: LSP server, background services, config, watcher coordination
- blocking: parse-heavy or CPU-heavy request work, JDK indexing, workspace import, decompilation setup

The rule is simple:

- protocol and coordination stay on `tokio`
- expensive CPU or filesystem work moves to `spawn_blocking`

## Architectural Invariants

- Open-document text is authoritative over disk text.
- `PreparedRequest` is the normal entry point for feature handlers; bypassing it usually loses overlay classes, metrics, cancellation, or cache reuse.
- `IndexView` is read-only and request-scoped. Do not store it in long-lived state.
- `WorkspaceIndex` is the shared mutable source of lookup state, but requests should read snapshots, not mutate it.
- Use `TypeName` and internal JVM names consistently; do not invent alternate type currencies.
- Synthetic members are part of the semantic view of a source type and must be considered before feature code claims a member is missing.
