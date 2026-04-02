# Java Analyzer

[![Ask DeepWiki](https://deepwiki.com/badge.svg)](https://deepwiki.com/cubewhy/java-analyzer)

[![CI Status](https://github.com/cubewhy/java-analyzer/actions/workflows/rust-ci.yaml/badge.svg)](https://github.com/cubewhy/java-analyzer/actions/workflows/rust-ci.yaml)
![GitHub License](https://img.shields.io/github/license/cubewhy/java-analyzer)

Extreme fast Java LSP, built in Rust

## Archived

The current architecture design is completely flawed. I believe using a handwritten parser + rowan + salsa would be a better approach. Tree-sitter imposes too many limitations. I simply don't have the bandwidth to refactor it, so I'm archiving the repository.

If you got interest, please contact me, we could restart the project together.

My contacts can be found at my GitHub profile.

## Feature Matrix

[JLS Implement Status (AI generated)](docs/jls-implementation-status.md)

- Analyze Jar, Codebase and JDK builtins
- Code completion
- Symbols List (Outline)
- Goto definition
- Inlay hints
  - Inferred type on `var`
  - Parameter names
- Decompiler support (Vineflower, cfr)
- Treesitter based syntax highlight (semantic_tokens handler)
- Java 8 to 25 support
- Gradle 4.0 to 9.x support
- Maven 3.0+ support
- Lombok support (Partial) [Implement Status (AI generated)](docs/lombok-implementation.md)

## FAQ

- Is this a real LSP?
  YES
- Is this production ready?
  Probably yes, but not everything in JLS implemented perfectly yet.

## Development

- [The Treesitter inspector script](https://gist.github.com/cubewhy/7a43196d323488db4c4053f1c5126f9f)
- [Treesitter playground](https://tree-sitter.github.io/tree-sitter/7-playground.html)

## License

This work is licensed under GPL-3.0 license.

You're allowed to

- use
- share
- modify
