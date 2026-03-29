# Semantic Pipeline

## Purpose

This document explains how Java source becomes the semantic objects used by completion, goto definition, and inlay hints.

The important distinction is:

- parsing and indexing discover structure
- semantic context explains the cursor and local scope
- type resolution explains what expressions and members mean in that scope

## Core Types

| Type | Meaning |
| --- | --- |
| `CursorLocation` | syntax-level classification of the cursor position |
| `SemanticContext` | request-scoped semantic snapshot used by editor features |
| `SourceTypeCtx` | source-facing type-resolution context built from package, imports, and visible types |
| `TypeName` | canonical semantic type currency |
| `IndexView` | request-scoped read view over visible source, dependencies, and JDK |
| `TypeResolver` | member lookup, compatibility checks, and overload resolution |
| `CurrentClassMember` | explicit or synthetic member visible on the current type |

## End-To-End Flow

### 1. Prepare The Request

`PreparedRequest::prepare` in `src/lsp/request_context.rs` does the shared setup:

- ensure the document has a tree
- resolve module/classpath/source-root analysis context
- obtain an `IndexView`
- extract the current file into classes and prepend them as an overlay

That gives every semantic feature a coherent view of unsaved code plus the rest of the workspace.

### 2. Extract Cursor Context

Java cursor context is extracted through Salsa queries under `src/salsa_queries/java/`.

The extracted data includes:

- package and imports
- enclosing class and internal name
- visible locals
- flow overrides
- cursor location and query text
- Java-module context when inside `module-info.java`

`Document` then caches the resulting `SemanticContext` by document version, workspace version, analysis context, offset, and trigger character.

### 3. Enrich The Context

`ContextEnricher` in `src/language/java/completion_context.rs` upgrades the raw cursor context.

Important enrichment steps:

- materialize local-variable types
- resolve receiver types for member access
- compute expected types for assignments and method arguments
- bind functional-interface context for lambdas and method references
- normalize method-reference and constructor-call cases into shared forms

This stage is where many "completion knows where I am but not what types are involved" bugs actually live.

### 4. Resolve Expressions And Members

`TypeResolver` in `src/semantic/types.rs` and helpers in `src/language/java/expression_typing.rs` do the heavy semantic work:

- resolve simple identifiers from locals, enclosing members, and visible types
- evaluate chains such as `foo.bar().baz`
- compute expression result types
- rank overload candidates
- substitute generic type parameters conservatively

### 5. Add Synthetic Members

Current-type member extraction runs through `src/language/java/synthetic/common.rs`.

That layer merges:

- explicit members parsed from the source body
- record and enum synthetic members
- Lombok-generated synthetic members

Editor features should reason about the merged view, not about explicit members alone.

### 6. Feature-Specific Consumers

- completion: providers read `SemanticContext` and `IndexView`, then `post_processor.rs` scores and deduplicates the final list
- goto definition: `SymbolResolver` resolves the target symbol, then the handler maps it back to source, synthetic origins, or decompiled bytecode
- inlay hints: `editor_semantics.rs` and `inlay_hints.rs` reuse the same semantic context and call-resolution helpers

## Important Invariants

- `TypeName` is the semantic type currency. Do not pass raw descriptors or ad hoc strings through new semantic APIs.
- Use erased internal names for class lookup. Generic detail belongs in `TypeName`, not in index keys.
- Keep source names and internal names separate. Source names are for UI; internal names are for lookup.
- The open document overlay must remain above workspace and dependency state.
- Synthetic members are not a side channel. They are part of the semantic model of the current type.
- Request handlers should not reimplement Java semantics. They should prepare the request, then delegate.

## Where To Change What

- bad cursor classification: `src/language/java/location.rs`
- missing locals or enclosing context: `src/salsa_queries/semantic.rs` and Java scope helpers
- wrong receiver typing or expected type: `src/language/java/completion_context.rs`
- wrong expression result: `src/language/java/expression_typing.rs`
- wrong overload choice: `src/semantic/types.rs`
- missing synthetic member: `src/language/java/synthetic/` or `src/language/java/lombok/`

## Common Refactor Mistakes

- skipping `PreparedRequest` and losing the current-file overlay
- adding a parallel type representation instead of extending `TypeName`
- looking up classes by pretty-printed Java names instead of internal JVM names
- fixing a completion symptom inside a provider when the real problem is earlier enrichment
- forgetting that a source type may expose members through records, enums, or Lombok even when the source body is empty
