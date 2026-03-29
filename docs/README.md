# Documentation Guide

This folder is the maintainer-facing map for `java-analyzer`. It is written from the current code, not from aspirational design notes.

## Reading order

1. [Project Overview](./project-overview.md) for the product boundary and repository map.
2. [Architecture](./architecture.md) for runtime structure, state ownership, and request flow.
3. [Development Guide](./development-guide.md) for build, test, debug, and common edit workflows.
4. [Semantic Pipeline](./semantic-pipeline.md) for the Java analysis model and invariants.
5. [Salsa Quick Reference](./salsa-quick-reference.md) for incremental query rules.

## Focused guides

- [Java Inlay Hints Design](./java-inlay-hints-design.md) explains the current inlay-hint pipeline and the shared helpers it depends on.
- [Lombok Implementation](./lombok-implementation.md) explains synthetic-member generation and where Lombok support plugs in.
- [JLS Implementation Status](./jls-implementation-status.md) summarizes strong areas, partial areas, and current gaps in Java support.

## Documentation rules

- Prefer naming the concrete module or function that owns behavior.
- Prefer describing invariants and extension seams over long feature checklists.
- When behavior changes, update the relevant doc in the same change if the maintainer-facing mental model also changed.
- Treat this set as the canonical docs. Older one-off notes should not silently become the source of truth.
