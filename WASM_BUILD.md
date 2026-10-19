# Build Project for WebAssembly

The project can be built for [WebAssembly](https://webassembly.org/) in order to be more easily consumed by browsers or a Node.js app.

More specifically, it is also compiled as a [WASI](https://wasi.dev/) module so that the consuming app can leverage I/O and consume the CLI tool using standard output (stdout).

## Prerequisites

- Install [Nix](https://nixos.org/) because it's the easiest way to use the [Haskell Wasm tools](https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta).
- Check out `automerge-pandoc`, `pandoc-diff` and `pandoc-tree` next to this repository.

## WASM Tools

The WASM tools come from the [ghc-wasm-meta](https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta) flake, with the GHC `9.10` flavour (`all_9_10`), not the latest one. `flake.nix` declares that flake as the only input and `flake.lock` pins it to a revision, so every build uses the same compiler.

All the commands below run in a shell with the WASM tools. Open it from the repository's root with:

```
nix --extra-experimental-features 'nix-command flakes' shell --inputs-from . 'ghc-wasm-meta#all_9_10'
```

and exit it with `exit`.

## Build for WASM

Build whenever the consuming app needs a newer version of this library, e.g. after changing its code or the code of the packages next to it:

```
wasm32-wasi-cabal build --project-file=cabal.project.wasm-wasi --builddir=dist-wasm v2-hs-lib:exe:v2-hs-lib
```

This uses the `cabal.project.wasm-wasi` overrides that are needed for the project to be built for WebAssembly. The WASM module's path includes the GHC version, so print it with:

```
wasm32-wasi-cabal list-bin --project-file=cabal.project.wasm-wasi --builddir=dist-wasm v2-hs-lib:exe:v2-hs-lib
```

Then copy the module where it's needed, or optimize it with `wasm-opt` and write the result there. Optimizing makes the module much smaller, at the cost of a slightly longer build:

```
wasm-opt -Oz WASM_MODULE_PATH -o ../v2/src/modules/infrastructure/wasm/files/v2-hs-lib.wasm
```

## Dependency Versions

`cabal.project.wasm-wasi.freeze` pins the versions of the dependencies. Update it before building whenever the dependencies change, and commit it together with the WASM module it produced.

After adding or removing a dependency, or changing `cabal.project.wasm-wasi`, run:

```
wasm32-wasi-cabal freeze --project-file=cabal.project.wasm-wasi --builddir=dist-wasm --enable-tests
```

To upgrade all the dependencies, or when a dependency needs a newer package than the freeze file allows, run:

```
rm cabal.project.wasm-wasi.freeze
wasm32-wasi-cabal update
wasm32-wasi-cabal freeze --project-file=cabal.project.wasm-wasi --builddir=dist-wasm --enable-tests
```

## Updating the WASM Tools

To move to a newer revision of ghc-wasm-meta, run `nix --extra-experimental-features 'nix-command flakes' flake update ghc-wasm-meta` outside the shell with the WASM tools. A new GHC comes with new versions of `base` and the other packages it ships with, which the freeze file pins, so then open the shell again, upgrade the dependencies and build. Commit `flake.lock` together with the freeze file.

## Run using wasmtime

You can confirm that the CLI is working by using the [wasmtime](https://wasmtime.dev/) runtime for WebAssembly, which the WASM tools include:

```
wasmtime WASM_MODULE_PATH convertToText --from markdown --to html '# Hello'
```

## Notes on the Overrides

- The overrides in `cabal.project.wasm-wasi` are based mostly on https://github.com/haskell-wasm/pandoc/blob/wasm/cabal.project, which is a Pandoc build for wasm.
- Pandoc was downgraded to version `3.5` because this was the version that was working with the wasm build at the time of the initial implementation.
- [haskell-wasm](https://github.com/orgs/haskell-wasm/repositories) GitHub repositories is probably the best resource for packages that will work for WASM & WASI.

## More Resources

Haskell's [official docs](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html) for WebAssembly were the starting point of this implementation and are a useful resource.
