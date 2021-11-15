<p align="center">
  <img src="img/purenix-icon.svg" width="150" height="150" />
</ p>
<h1 align="center">PureNix</h1>

PureNix is a Nix backend for PureScript.

Sometimes, you find yourself having to write Nix code that's more complicated than what the language was designed for.
PureNix allows you to write that code in a fully-featured, strongly-typed language instead, and then compile to Nix.
A typical example is parsing of configuration files, like [the port of cabal2nix that inspired PureNix](https://github.com/cdepillabout/cabal2nixWithoutIFD).

PureNix has full support for all of PureScript's features, including data types, type classes, and calling back into Nix using the FFI.

On the [organization page for PureNix](https://github.com/purenix-org) you will find a number of packages intended to be used with PureNix, including ports of libraries like [purescript-prelude](https://github.com/purenix-org/purescript-prelude).

## Usage

The easiest way to use PureNix is through Spago.
Simply set `backend = "purenix"`, make sure `purenix` is available in the `PATH`, and build as normal.

When you run `purenix`, manually or through Spago, it will look for the Purescript output directory `./output` in the current working directory.
It then traverses this directory structure, looks for Purescript's intermediate `corefn.json` files, transpiles the `corefn.json` files to the equivalent Nix code, and writes the output Nix code to `default.nix`.

See [this post](https://discourse.nixos.org/t/purenix-nix-backend-for-purescript/15756/3) on the NixOS Discourse for more in-depth instructions.


## Code sample

PureScript source, `Main.purs`:

```purescript
module Main where

import Data.A as A
import Data.B as B

greeting :: String
greeting = "Hello, world!"

data Maybe a = Nothing | Just a

fromMaybe :: forall a. a -> Maybe a -> a
fromMaybe a Nothing = a
fromMaybe _ (Just a) = a

foreign import add :: Int -> Int -> Int

foo :: Int
foo = add A.bar B.baz
```

Nix FFI file, `Main.nix`:

```nix
{ add = a: b: a + b; }
```

Generated Nix:

```nix
let
  module =
    { "Data.A" = import ../Data.A;
      "Data.B" = import ../Data.B;
    };
  foreign = import ./foreign.nix;
  add = foreign.add;
  Nothing = {__tag = "Nothing";};
  Just = value0:
    { __tag = "Just";
      __field0 = value0;
    };
  greeting = "Hello, world!";
  fromMaybe = v: v1:
    let
      __pattern0 = __fail: if v1.__tag == "Nothing" then let a = v; in a else __fail;
      __pattern1 = __fail: if v1.__tag == "Just" then let a = v1.__field0; in a else __fail;
      __patternFail = builtins.throw "Pattern match failure in src/Main.purs at 11:1 - 11:41";
    in
      __pattern0 (__pattern1 __patternFail);
  foo = add module."Data.A".bar module."Data.B".baz;
in
  {inherit greeting Nothing Just fromMaybe add foo;}
```

There are a couple things to notice here:

- PureScript built-in types like `String`, `Int`, objects, and lists are converted to their corresponding Nix types, as in `greeting`.
- Data constructors from sum types are available to easily work with in the output Nix file, like `Just` and `Nothing`, although you might want to define named field accessors.
- Foreign imports are straightforward to define and use, like in `add` and `foo`. The FFI file gets copied into the module's output directory as `foreign.nix`.

## Development

You can launch a development shell with the command `nix develop` (as long as you have flakes support enabled in Nix).
This puts you in a Nix shell with `cabal-install` and GHC setup to compile PureNix, as well as other helpful tools like HLint, HLS, PureScript, Spago, etc.
From here you should be able to run commands like `cabal build` in order to build PureNix.

## Warnings

### What PureNix is and is not

PureNix allows you to write code in PureScript, and then compile to Nix.
The degree to which you can replace existing Nix code depends on how well you can express that code in PureScript.
For some things, that's pretty easy, but there are many things in Nix and nixpkgs that are much harder to provide (useful) types for.
As such, PureNix is _not_ a complete typed replacement for Nix.
The goal for now is simply to allow you to take code that's tricky to write in an untyped language, and write it in a typed language instead.

[The ecosystem around PureNix](https://github.com/purenix-org) is currently focused on providing PureNix ports of existing PureScript libraries.
Over time, we hope to expand in the other direction as well, with libraries that provide typed versions of Nix-only constructs, thereby expanding the amount of Nix you can feasibly replace with PureNix, but there's still a lot to be done.
Any help is welcome!

### Laziness and memory management

PureScript generally assumes that its backends perform strict evaluation, and some degree of memory management.
Nix is a lazy language however, and is happy to leak memory.
For most use cases this doesn't cause any issues, and in fact the laziness allows you to write more Haskell-like code than you usually would in PureScript.

Still, it's good to keep these things in mind:

- Like in every lazy language, you need to watch out for space leaks caused by accidentally building up large thunks.
  PureScript does not natively have tools to deal with laziness, like bang patterns or `seq`, but you can define them yourself by e.g. pulling in `builtins.seq` through the FFI.

- Long-running programs may run out of memory due to the lack of garbage collection and tail recursion.
  If you end up writing long-running programs, like a parser that needs to process very large files, you might have to rewrite it in a way that minimizes recursion and allocation.
