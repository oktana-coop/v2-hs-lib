# v2 Editor Haskell Library

It exposes v2 modules written in Haskell which are meant to be used in the web environment via WebAssembly. The library's responsibility is primarily rich text representation conversion and diffing.

## Build for WebAssembly

There are instructions for building the project for [WebAssembly](https://webassembly.org/) [here](https://github.com/oktana-coop/v2-hs-lib/blob/main/WASM_BUILD.md).

## Testing

The primary framework used for testing is [tasty](https://hackage.haskell.org/package/tasty). It is combined with [tasty-golden](https://hackage.haskell.org/package/tasty-golden) for golden (aka snapshot) testing and [tasty-hspec](https://hackage.haskell.org/package/tasty-hspec) to leverage [HSpec](https://hspec.github.io)'s friendly DSL for defining tests.

### Running Tests

To run the tests run `stack test` or `stack build --test`.

#### Updating Golden Files

Golden tests test the output against a file committed to the repo (aka golden file). There are cases where the golden must be updated (the difference with the snapshot is expected). To do this we must past the `accept` argument like this:

```
stack test --ta --accept
```
