<p align="center">
  <img src="img/purenix-icon.svg" width="150" height="150" />
</ p>
<h1 align="center">purenix</h1>

Purenix is a Nix backend for Purescript

Sometimes, you find yourself having to write Nix code that's more complicated than what the language was designed for.
Purenix allows you to write that code in a fully-featured, strongly-typed language instead, and then compile to Nix.
A typical example is handling of configuration files, like [the port of cabal2nix that inspired Purenix](https://github.com/cdepillabout/cabal2nixWithoutIFD).

Purenix has full support for all of Purescript's features, including data types, type classes, and calling back into Nix using the FFI.

On the [organization page for `purenix`](https://github.com/purenix-org) you will find a number of packages intended to be used with `purenix`, including Nix ports of several popular Purescript libraries like the Prelude.

#### Code sample

Purescript source:

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

## Usage

### Compiling with purenix

The easiest way to use `purenix` is through Spago.
Simply set the backend to `purenix`, make sure it's available in the `PATH`, and build as normal.

When you run `purenix`, manually or through Spago, it will look for the Purescript output directory `./output` in the current working directory.
It then traverses this directory structure, look for Purescript's intermediate `corefn.json` files, and produce the equivalent Nix code and write it to `default.nix`.

### Laziness

Purescript is a strict language, or more specifically, assumes that its backends perform strict evaluation.
Nix is a lazy language however, and we make no attempt to reconcile this in any way.
You might expect this to cause issues, but in general, Purescript makes for a fine lazy language.
In fact, because of laziness you can use more of Haskell's idioms than you typically would in Purescript.

There are two things to watch out for:

The first thing to watch out for is space leaks, in the same way you would in Haskell (slightly more so because there is no strictness analysis phase).
Nix has a `builtins.seq` that you can use to force evaluation of thunks, but you have to go through the FFI to call it, since Purescript has no native support for it.

The second thing is that Haskell often relies on tail call optimization to make long-running lazy programs use constant memory, which Nix does not support.
This shouldn't be an issue since you probably don't want to use Nix for long-running programs in the first place, but it's a good thing to be aware of.
