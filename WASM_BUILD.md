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

`cabal.project.wasm-wasi.freeze` pins the versions of the dependencies. Update it together with the native build's freeze file (see the [README](README.md#dependency-versions)), before building, and commit it together with the WASM module it produced.

After adding or removing a dependency, or changing `cabal.project.wasm-wasi`, run:

```
wasm32-wasi-cabal freeze --project-file=cabal.project.wasm-wasi --builddir=dist-wasm
```

To upgrade the dependencies, follow the steps in the [README](README.md#dependency-versions). For the WebAssembly build, first update the package index:

```
wasm32-wasi-cabal update
```

Then, after setting `index-state` in `cabal.project`, freeze it again:

```
rm cabal.project.wasm-wasi.freeze
wasm32-wasi-cabal freeze --project-file=cabal.project.wasm-wasi --builddir=dist-wasm
```

## Updating the WASM Tools

To move to a newer revision of ghc-wasm-meta, run `nix --extra-experimental-features 'nix-command flakes' flake update ghc-wasm-meta` outside the shell with the WASM tools. A new GHC comes with new versions of `base` and the other packages it ships with, which the freeze files pin. So then move the native build to the same GHC version (`with-compiler` in `cabal.project`), delete both freeze files, freeze both builds again and build. Commit `flake.lock` together with the freeze files.

## Run using wasmtime

You can confirm that the CLI is working by using the [wasmtime](https://wasmtime.dev/) runtime for WebAssembly, which the WASM tools include:

```
wasmtime WASM_MODULE_PATH convertToText --from markdown --to html '# Hello'
```

## WASM Overrides

`cabal.project.wasm-wasi` imports `cabal.project`, so both builds share the same packages and constraints. It only adds what WebAssembly needs: a few package options, and patched versions of some dependencies.

### Patched Dependencies

A few dependencies don't build for WebAssembly as published on Hackage, e.g. because they rely on features WASI doesn't provide, such as networking. `cabal.project.wasm-wasi` takes each of them from its git repository at a fixed revision instead, with a patch from `wasm/patches/` that makes it build. The patches come from [Pandoc's own WebAssembly build](https://github.com/jgm/pandoc/tree/main/wasm), which keeps them working for the versions Pandoc uses.

For running the build, this needs nothing extra: cabal clones the repositories and applies the patches by itself. The build reads the patches from this repository, so they are committed with the rest of the source.

When upgrading Pandoc or one of the patched dependencies:

- Compare `cabal.project.wasm-wasi` and `wasm/patches/` with Pandoc's files at the new release, and update the revisions and the patches.
- Keep each revision at the version the native build uses, so that both builds run the same code.
- Some patches are adjusted from Pandoc's, e.g. so that macOS's `patch` applies them. Keep these adjustments, or check whether they are still needed.
- When a dependency publishes a version that builds for WebAssembly, remove its revision and its patch.

## More Resources

Haskell's [official docs](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html) for WebAssembly were the starting point of this implementation and are a useful resource.
