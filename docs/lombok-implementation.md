# Lombok Implementation

## Design Goal

Lombok support is implemented as source-level synthetic-member generation. The server does not run Lombok as an annotation processor and does not try to mirror Lombok's full compilation pipeline.

The goal is narrower and pragmatic:

- expose generated members to completion, goto definition, and semantic lookups
- preserve source navigation back to the field or class that caused the member to exist
- stay conservative when behavior is uncertain

## Where Lombok Support Lives

| Path | Role |
| --- | --- |
| `src/language/java/lombok/types.rs` | annotation names, access levels, small shared enums |
| `src/language/java/lombok/utils.rs` | annotation/config parsing helpers and naming rules |
| `src/language/java/lombok/rules/` | per-annotation synthetic-member rules |
| `src/language/java/synthetic/common.rs` | rule registry and synthetic merge path |
| `src/salsa_queries/semantic.rs` | current-class member extraction that consumes synthetics |

## Supported Rule Families

The rule set currently covers these groups:

- getter and setter generation
- constructors
- `@Data`
- `@Value`
- `@ToString`
- `@EqualsAndHashCode`
- `@Builder`
- `@With` and `@Wither`
- log annotations
- delegate support

Rules are registered in the `SYNTHETIC_RULES` array in `src/language/java/synthetic/common.rs`.

## Integration Model

Lombok is not a separate late-stage pass. It participates in the same synthetic-member framework that also handles record and enum synthetic members.

The flow is:

1. parse explicit fields and methods from the class body
2. run synthetic rules with access to the explicit members
3. merge synthetic and explicit members into the current type view
4. use that merged member set in semantic queries and editor features

This means a Lombok-generated getter should look like a normal member to feature code.

## Synthetic Origins

Every generated Lombok member carries a `SyntheticOrigin`.

That origin is used to map navigation back to a meaningful source node, such as:

- the field that produced a getter, setter, or `with` method
- the class declaration that produced `toString`, constructor, builder, or log members

If you add a new Lombok rule, add a corresponding origin when the generated member needs source navigation.

## Important Constraint

The synthetic framework does not magically deduplicate after generation. Each rule receives `explicit_methods` and `explicit_fields`, and the rule itself is responsible for not emitting duplicates that already exist in source.

When extending the rule set:

- check explicit members first
- be conservative if Lombok semantics are ambiguous
- prefer missing one synthetic over emitting a wrong one that pollutes completion and goto

## How To Add A New Lombok Annotation

1. Add or reuse the annotation constant in `types.rs`.
2. Add parsing helpers in `utils.rs` if new parameters or config rules are needed.
3. Implement a `SyntheticMemberRule` in `src/language/java/lombok/rules/`.
4. Register the rule in `SYNTHETIC_RULES`.
5. Add a `SyntheticOrigin` variant if navigation needs to land on a source node.
6. Add tests in `src/language/java/lombok/tests.rs`.

## Testing

`src/language/java/lombok/tests.rs` is the main safety net. Treat it as an integration suite, not as a place for tiny helper-only tests.

When adding support, cover:

- positive generation
- opt-out or exclusion behavior
- duplicate suppression with explicit members
- source navigation for at least one generated member

## Known Boundaries

- this is source-level support, not bytecode or compilation support
- diagnostics for invalid Lombok usage are not a goal here
- rule behavior may intentionally be less exhaustive than real Lombok when the tradeoff favors predictable editor behavior
