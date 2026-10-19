# v2 Editor Haskell Library

It exposes v2 modules written in Haskell which are meant to be used in the web environment via WebAssembly. The library's responsibility is primarily rich text representation conversion and diffing.

## Build

### Prerequisites

- Install GHC and [cabal](https://www.haskell.org/cabal/), e.g. with [GHCup](https://www.haskell.org/ghcup/). Use the GHC version that `with-compiler` names in `cabal.project`.
- Check out `automerge-pandoc`, `pandoc-diff` and `pandoc-tree` next to this repository.

### Build Natively

Run `cabal build all`. The native build is used for development and tests.

### Build for WebAssembly

There are instructions for building the project for [WebAssembly](https://webassembly.org/) [here](https://github.com/oktana-coop/v2-hs-lib/blob/main/WASM_BUILD.md). This is the build the consuming app uses.

### Dependency Versions

Both builds solve the dependencies from the same state of the package index, the `index-state` in `cabal.project`. Each build has a freeze file that pins the versions of its dependencies: `cabal.project.freeze` for the native build and `cabal.project.wasm-wasi.freeze` for the WebAssembly build. Update them together, so that both builds keep using the same versions, and commit them.

After adding or removing a dependency, run `cabal freeze`, and freeze the WebAssembly build too.

To upgrade the dependencies:

1. Run `cabal update`, and `wasm32-wasi-cabal update` in the shell with the WASM tools. Both print the state they updated the index to.
2. Set `index-state` in `cabal.project` to the earlier of the two times.
3. Delete both freeze files, then run `cabal freeze` and freeze the WebAssembly build too.
4. Build both, run the tests, and commit `cabal.project` together with both freeze files.

## Testing

The primary framework used for testing is [tasty](https://hackage.haskell.org/package/tasty). It is combined with [tasty-golden](https://hackage.haskell.org/package/tasty-golden) for golden (aka snapshot) testing and [tasty-hspec](https://hackage.haskell.org/package/tasty-hspec) to leverage [HSpec](https://hspec.github.io)'s friendly DSL for defining tests.

### Running Tests

To run the tests run `cabal test v2-hs-lib`.

#### Updating Golden Files

Golden tests test the output against a file committed to the repo (aka golden file). There are cases where the golden must be updated (the difference with the snapshot is expected). To do this we must pass the `accept` argument like this:

```
cabal test v2-hs-lib --test-options=--accept
```

#### Failed Round Trip Tests Logs

Round trip tests are included in the testing suite, in an attempt to gradually achieve isomorphic conversions:

- Markdown → ProseMirror → Markdown
- ProseMirror → Markdown → ProseMirror

When these round trip tests fail, it's difficult to understand what failed from what's logged into the console (stdout). This is why we also write the artifacts to a folder in the repository (`failed-round-trip-tests/`)
