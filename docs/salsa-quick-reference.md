# Salsa Quick Reference

## What Salsa Owns In This Repository

Salsa is used for file-scoped incremental computation. It is not the global source of truth for the workspace.

In this codebase, Salsa owns:

- source-file inputs
- memoized parse and extraction metadata
- file-scoped semantic queries
- invalidation across content changes

Salsa does not own:

- the mutable workspace index
- open-document LSP caches
- filesystem or subprocess work
- tree-sitter `Tree` values as tracked outputs
- full `ClassMetadata` graphs as tracked outputs

## The Two-Level Cache Model

The repo deliberately combines Salsa with side caches.

### In Salsa

Tracked queries hold small, hashable values such as:

- parse metadata
- extracted-package/import metadata
- completion-context metadata
- workspace-analysis metadata keyed by module/classpath/source root/version

### Outside Salsa

Side caches hold values that are expensive or awkward to hash:

- tree-sitter parse trees
- materialized class extraction snapshots
- open-document semantic-context cache
- request-local `IndexView` caches

This pattern is visible in `src/salsa_db.rs`, `src/salsa_queries/parse.rs`, and `src/salsa_queries/index.rs`.

## Query Families

| Area | Files | Role |
| --- | --- | --- |
| parse | `src/salsa_queries/parse.rs` | package/import extraction and parse-tree reuse |
| index | `src/salsa_queries/index.rs`, `src/salsa_queries/java/indexing.rs` | source class extraction and analysis-context lookup |
| context | `src/salsa_queries/context.rs`, `src/salsa_queries/java/completion.rs` | cursor classification and semantic-context extraction |
| semantic | `src/salsa_queries/semantic.rs` | locals, class members, flow overrides, enclosing structure |
| hints | `src/salsa_queries/hints.rs`, `src/salsa_queries/java/hints.rs` | inlay-hint support data |
| resolve | `src/salsa_queries/resolve.rs`, `src/salsa_queries/java/resolve.rs` | symbol and type lookup |

## Request Path With Salsa

The normal request path is:

1. `Workspace` keeps the Salsa input synchronized with the open document.
2. `PreparedRequest` asks Salsa for an `IndexView` matching the current analysis context.
3. Salsa-backed class extraction produces the current-file overlay.
4. Java Salsa queries produce semantic context or feature-specific data.
5. Runtime feature code consumes that data together with `IndexView`.

The key point is that Salsa participates in request preparation, not only in leaf queries.

## Rules For Adding Queries

- Use `#[salsa::tracked]` only for outputs with stable equality and small surface area.
- Keep actual `Tree` values and other non-hashable heavy data in side caches.
- Thread `module`, `classpath`, `source_root`, and workspace version into queries when visibility depends on them.
- Do not perform filesystem I/O, network I/O, or subprocess work inside a Salsa query.
- Do not store `IndexView` or `WorkspaceIndex` inside tracked values.
- If a query needs the current open-file parse tree, prefer reusing the seeded parse snapshot instead of reparsing from scratch.

## When To Use A Plain Function Instead Of A Tracked Query

Use a plain function when:

- you need to read a side cache
- the result is large and you only need Salsa to invalidate a smaller metadata wrapper
- the function is just an adapter around tracked queries and runtime objects

This is why `get_index_view_for_context` and `get_extracted_classes` are plain functions even though they depend on tracked invalidation inputs.

## Common Invalidation Inputs

These are the inputs most often forgotten in refactors:

- `SourceFile.content`
- `SourceFile.language_id`
- workspace index version
- module/classpath/source-root tuple
- current-file overlay class count

If a query is unexpectedly stale, check that the invalidation key actually includes all of the above factors it depends on.

## Practical Advice

- Prefer adding a small tracked metadata query plus a side-cache materialization path over forcing a large result into Salsa.
- Keep Java-specific logic in `src/salsa_queries/java/` when the query needs Java AST knowledge.
- If a request handler starts doing repeated direct parsing, stop and look for the equivalent Salsa path first.
