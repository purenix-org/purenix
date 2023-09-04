# Quick Start

Here's a quick-start guide for seting up a PureNix project.

If something is unclear, you can always take a look at some of our libraries to see exactly how they are packaged, and copy what they do.
For instance, [`purescript-foldable-traversable`](https://github.com/purenix-org/purescript-foldable-traversable) or [`purescript-maybe`](https://github.com/purenix-org/purescript-maybe).

## Creating a development shell

The PureNix flake exposes a development shell with tools like [PureScript](https://github.com/purescript/purescript), [Spago](https://github.com/purescript/spago), and PureNix.
If you have flakes enabled, you can drop into the PureNix development shell with a command like this:

```console
$ nix develop github:purenix-org/purenix#use-purenix
```

From here, use `spago` to create your PureNix project:

```console
$ mkdir my-cool-project
$ cd my-cool-project/
$ spago init
```

This creates a template project.  You'll need to edit some of these files in order to work with PureNix.

## Edit files

This section explains exactly what you'll have to change from `spago`'s defaults in order to use PureNix.

### `packages.dhall`

This file defines the upstream package set you want to use.
It is somewhat similar to a `stack.yaml` file for Haskell packages.

It currently looks like this:

```dhall
let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.4-20211028/packages.dhall sha256:df6486e7fad6dbe724c4e2ee5eac65454843dce1f6e10dc35e0b1a8aa9720b26

in  upstream
```

This is pointing to the official PureScript package set, but you'll need to change it point to the PureNix package set:

```dhall
let upstream =
      https://raw.githubusercontent.com/purenix-org/temp-package-set/a14eb4db17fa84ed4d41aee16f6a18018f23ec64/packages.dhall

in  upstream
  with metadata.version = "v0.15.4"
```

You may want to change the commit to be the [most recent commit from `temp-package-set`](https://github.com/purenix-org/temp-package-set/commits/main).
See the section at the bottom of this document for more information about package sets.

(Note that `spago` will automatically add the `sha256:XXX` integrity check the first time you run `spago build`.)

### `spago.dhall`

This file defines your current package.
It is similar to a `.cabal` file for Haskell projects, or a `Cargo.toml` file for Rust projects.

It currently looks like the following:

```dhall
{ name = "my-project"
, dependencies = [ "console", "effect", "prelude", "psci-support" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
```

You'll need to make the following changes:

-   The temporary PureNix package set doesn't include some of the above dependencies, so you need to remove them.
    You can add other packages as long as they are defined in the package set.
-   There isn't any testing support in PureNix yet, so remove the test sources.
-   You need to specify that `spago` should use the `purenix` executable as the backend.

The new `spago.dhall` should look something like this:

```dhall
{ name = "my-project"
, dependencies = [ "foldable-traversable", "maybe", "prelude" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, backend = "purenix"
}
```

### `src/Main.purs`

Now you're ready to actually write some PureScript code.

Edit the `src/Main.purs` file to look like this:

```purescript
module Main where

import Prelude

import Data.Foldable (foldr)
import Data.Maybe (Maybe(..))

myNames :: String
myNames =
  foldr (\name accum -> name <> " " <> accum) "" ["Eelco", "Jon", "Graham"]
```

## Building

You can use `spago` to compile the code:

```console
$ spago build
```

This creates the directory `output/` with all the built code.
For instance, your `src/Main.purs` module is built as `output/Main/default.nix`.
I recommend you take a quick look at it.
The conversion between PureScript and Nix is relatively straight-forward (except for pattern matching and type class dictionaries).

## Using the built Nix code

You can use the built Nix code as you'd expect:

```console
$ nix repl
nix-repl> main = import ./output/Main/default.nix
nix-repl> main.myNames
"Eelco Jon Graham "
```

## Setting up a `flake.nix`

It may be convenient to create a `flake.nix` for your project.
This can make it easy to quickly jump into a dev shell.

A simple `flake.nix` may look like the following:

```nix
{
  description = "my-cool-project";

  inputs.purenix.url = "github:purenix-org/purenix";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, flake-utils, purenix }:
    flake-utils.lib.eachDefaultSystem (system: {
      devShell = purenix.devShells.${system}.use-purenix;
    });
}
```

With this in place, you can enter the dev shell just by running `nix develop`.

## A Note about Package Sets

A PureScript package set is a set of PureScript packages that are known to all compile and be useable together.
PureScript package sets are very similar to Haskell [Stackage](https://www.stackage.org/) resolvers.

The main [PureScript Package Sets repo](https://github.com/purescript/package-sets) is for the JavaScript backend.
All the packages in this package set are meant to compile to JavaScript code.
The PureScript Package Sets repo has a workflow and CI actions that make it easy for users to add new packages to the package set, upgrade existing packages, and make new releases of the package set itself.

In order to use a package set when compiling with PureNix, you need to specify a PureNix-compatible package set.
Currently, we only have a [temporary package set](https://github.com/purenix-org/temp-package-set) for PureNix.
This package set pins all packages to their `master` branch.
There are no workflows or CI actions for adding packages, upgrading existing packages, etc.
This is sufficient for getting started with PureNix, but not ideal for large-scale, serious use of PureNix.
There is an [open issue](https://github.com/purenix-org/purenix/issues/36) about creating an actual package set if you'd like to help with getting this off the ground.
Please open an issue or PR on <https://github.com/purenix-org/temp-package-set> if something is not working or you'd like to add one of your own packages.

You can find more info about PureScript package sets in the following places:

- The [PureScript Package Sets repo](https://github.com/purescript/package-sets) for the JavaScript backend
- The [Spago README](https://github.com/purescript/spago)
- The [PureScript Registry README](https://github.com/purescript/registry/) (which is supposed to be the eventual successor to PureScript package sets)

## Problems / Questions / PRs

Feel free to open an issue or send a PR on <https://github.com/purenix-org/purenix>.

The PureNix compiler is currently very usable, but the surrounding package set still needs some work.
We'd love to get more people to help out with improving both PureNix and the surrounding ecosystem!
