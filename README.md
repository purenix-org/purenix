# cabal2nixWithoutIFD

This is a proof-of-concept for a
[`cabal2nix`](https://github.com/NixOS/cabal2nix) written in Nix. The benefit
of writting it in Nix instead of Haskell is that is can parse a `.cabal` file
directly.  It does not rely on
[Import From Derivation (IFD)](https://blog.hercules-ci.com/2019/08/30/native-support-for-import-for-derivation/)
to work.

Internally we have a PureScript backend that compiles to Nix called
[`purenix`](./purenix).  We have written a `.cabal` parser in
PureScript in [`purescript-cabal-parser`](./purescript-cabal-parser) and
compiled it to Nix.  We can then directly use this from Nix to parse
a `.cabal` file without IFD.

## Running

Compiling `purescript-cabal-parser` to Nix can be done with the following
steps.

First, get into a Nix devShell:

```console
$ nix develop
```

The, change to `./purescript-cabal-parser` directory and run `spago build`:

```console
$ cd ./purescript-cabal-parser
$ spago build --verbose
```

`spago build` does the following things:

1.  Looks for all `.purs` files in `./purescript-cabal-parser/src/`.

    The location to search for `.purs` files can be changed in
    `./purescript-cabal-parser/spago.dhall`.

2.  Compile the `.purs` files to corefn and output in
    `./purescript-cabal-parser/output/`.

    For instance, `./purescript-cabal-parser/src/Main.purs` will be compiled to
    `./purescript-cabal-parser/output/Main/corefn.json`.

3.  `spago` will try to run the `backend` command in
    `./purescript-cabal-parser/spago.dhall`.

    This is set to `cd ../purenix && cabal run purenix`, so the `purenix`
    executable will be built and run.

4.  `purenix` needs to look for all `corefn.json` files in
    `./purescript-cabal-parser/output/`, translate them to JSON, and then
    output them somewhere.
