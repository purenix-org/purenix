# Changelog

Notable changes to this project are documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Breaking changes:

New features:

Bugfixes:

Other improvements:
- Changed `unit`'s FFI representation from `{}` to `undefined` (#267 by @JordanMartinez)

## [v5.0.1](https://github.com/purescript/purescript-prelude/releases/tag/v5.0.1) - 2021-05-11

Other improvements:
- Fix warnings revealed by `v0.14.1` PS release (#262 by @JordanMartinez)

## [v5.0.0](https://github.com/purescript/purescript-prelude/releases/tag/v5.0.0) - 2021-02-26

Breaking changes:
- Support compiler version `v0.14.0`, and drop support for previous versions (#206, #226)
- `purescript-proxy` was ported to this repo (#230)
- `purescript-generics-rep` was ported to this repo (#235)
- Move the `Applicative` Superclass law from `Monad` to `Bind` and rename it
to the `Apply` Superclass law (#229)
- Removed `unsafeCompare` (#257)

New features:
- Added `Bounded` instance for records (#208)
- Added `Show` instances to `Data.Generics.Rep` types (#250)
- Added `toRep` (#238)

Bugfixes:
- No longer use reference equality check in `Array`'s `Eq` instance because it breaks referential transparency (#187).
- Fix Ring laws (#228)

Documentation improvements:
- Added `lift2` example using `Maybe` (#213)
- Added `const` example (#214)
- Added `power` example (#253)
- Clarify `Array`'s do notation and the purposes of `Monoid` and `Semigroup` newtypes (#217)
- Clarify `Unit` representation in FFI code (#223)
- Fix typo: 'ommitted' -> 'omitted' (#220)

Other improvements:
- Migrated to GitHub Actions (#234)
- Added a `CHANGELOG.md` file and PR template (#254)

## [v4.1.1](https://github.com/purescript/purescript-prelude/releases/tag/v4.1.1) - 2019-04-20

- Added documentation fixes/improvements to `Data.Monoid.Conj` and other `Data.Monoid` newtypes (#191, #192)
- Made `Eq` and `Ord` instances for primitive types less JavaScript-specific (#183)

## [v4.1.0](https://github.com/purescript/purescript-prelude/releases/tag/v4.1.0) - 2018-07-17

- Added `Ord` instance for records

## [v4.0.1](https://github.com/purescript/purescript-prelude/releases/tag/v4.0.1) - 2018-06-07

- Performance improvement for `Array` equality, now performs a reference check before comparing contents (@jazmit)

## [v4.0.0](https://github.com/purescript/purescript-prelude/releases/tag/v4.0.0) - 2018-05-22

This version is for PureScript v0.12.x.

**Breaking changes**

- `id` has been renamed to `identity`
- The modulo and division behaviour for integers is now based on Euclidean division. Functions implementing the old `div`/`mod` behaviour are available as `quot`/`rem` in [`purescript-integers`](https://github.com/purescript/purescript-integers). (#161, #168)
- The definition of `Field` has been altered. It is no longer necessary to provide an instance for this, there is a single instance now for every type that implements `EuclideanRing` and `DivisionRing` .

**Other changes**

- `Monoid` has been moved into the prelude
- `Data.Symbol` has been moved into the prelude
- `RProxy` and `RLProxy` have been moved into the prelude
- There are now various instances available for records: `Eq`, `Show`, `Semigroup`, `Monoid`, `Semiring`... etc. (@i-am-tom)

## [v3.3.0](https://github.com/purescript/purescript-prelude/releases/tag/v3.3.0) - 2018-04-13

- Added `Data.Function.applyN` for repeatedly applying a function to an initial value (@matthewleon)

## [v3.2.0](https://github.com/purescript/purescript-prelude/releases/tag/v3.2.0) - 2018-04-07

- Added `Bounded` instance for `Number` (@erisco)

## [v3.1.1](https://github.com/purescript/purescript-prelude/releases/tag/v3.1.1) - 2017-11-01

Remove unused `refIneq` function (@metaleap)

## [v3.1.0](https://github.com/purescript/purescript-prelude/releases/tag/v3.1.0) - 2017-06-27

* Fixes for out-of-date documentation (@joneshf, @matthewleon)
* Add a new `DivisionRing` class

## [v3.0.0](https://github.com/purescript/purescript-prelude/releases/tag/v3.0.0) - 2017-03-25

- Updated for PureScript 0.11
- Removed incorrect `EuclideanRing Unit` and `Field Unit` instances

## [v2.5.0](https://github.com/purescript/purescript-prelude/releases/tag/v2.5.0) - 2017-03-02

- Added `Eq1` and `Ord1` classes

## [v2.4.0](https://github.com/purescript/purescript-prelude/releases/tag/v2.4.0) - 2017-02-13

- Add the new `Discard` class to support https://github.com/purescript/purescript/pull/2653
- Fixes a corner case in the `degree` function for `Int`.
- Documentation fixes

## [v2.3.0](https://github.com/purescript/purescript-prelude/releases/tag/v2.3.0) - 2017-01-29

- Added `gcd` and `lcm` for any `EuclideanRing`.
- Fixed some issues with the `EuclideanRing` laws. The `Unit` instance is no longer valid, but is being kept around temporarily for the sake of backwards compatibility.

## [v2.2.0](https://github.com/purescript/purescript-prelude/releases/tag/v2.2.0) - 2017-01-26

- Added instances of the form `C b => C (a -> b)` for `Semiring`, `Ring`, `CommutativeRing`, `BooleanAlgebra`
- Added some documentation for the `Void` type (@chexxor)
- Fixed documentation typos (@mlang, @thoradam)

## [v2.1.0](https://github.com/purescript/purescript-prelude/releases/tag/v2.1.0) - 2016-09-30

- Added `whenM` and `unlessM` variants of `when` and `unless` for situations where the conditional is also in `m`

## [v2.0.0](https://github.com/purescript/purescript-prelude/releases/tag/v2.0.0) - 2016-09-26

**Breaking changes**
- Fixed `=<<` to be right associative, as originally intended
- Changed behaviour of `Eq` and `Ord` for `NaN` numeric value #91 (@berdario)

**Enhancements**
- Updated docs to include notes about the non-law-abiding numeric instances
- Semigroup `append` for `Array` will no longer shallow copy the array if either side is empty #63 (suggested by @natefaubion)

## [v1.1.0](https://github.com/purescript/purescript-prelude/releases/tag/v1.1.0) - 2016-09-26

- Added `flap` and corresponding `<@>` operator (@joneshf)

## [v1.0.1](https://github.com/purescript/purescript-prelude/releases/tag/v1.0.1) - 2016-06-07

- A redundant `CommutativeRing` constraint was removed from `Field`, the `EuclideanRing` constraint already implies `CommutativeRing`.

## [v1.0.0](https://github.com/purescript/purescript-prelude/releases/tag/v1.0.0) - 2016-06-01

This release is intended for the PureScript 0.9.1 compiler and newer.

**Note**: The v1.0.0 tag is not meant to indicate the library is “finished”, the core libraries are all being bumped to this for the 0.9 compiler release so as to use semver more correctly.

## [v1.0.0-rc.6](https://github.com/purescript/purescript-prelude/releases/tag/v1.0.0-rc.6) - 2016-05-23

- Added `Data.NaturalTransformation` with a synonym and type operator alias for natural transformations

## [v1.0.0-rc.5](https://github.com/purescript/purescript-prelude/releases/tag/v1.0.0-rc.5) - 2016-05-22

- Added general `abs` and `signum` for types that with `Ord` and `Ring` instances (@hdgarrood)

## [v1.0.0-rc.4](https://github.com/purescript/purescript-prelude/releases/tag/v1.0.0-rc.4) - 2016-05-19

- Various fixes

## [v0.1.5](https://github.com/purescript/purescript-prelude/releases/tag/v0.1.5) - 2016-03-30

- Fixes invalid escape sequence in `Show` (@michaelficarra, #65)

## [v1.0.0-rc.3](https://github.com/purescript/purescript-prelude/releases/tag/v1.0.0-rc.3) - 2016-03-30

- Fixed escaping in `Show` #65

## [v1.0.0-rc.2](https://github.com/purescript/purescript-prelude/releases/tag/v1.0.0-rc.2) - 2016-03-20

- Reworked the hierarchy for number classes to resolve some issues with the laws and admit new instances #61
- Introduced `HeytingAlgebra` as a superclass of `BooleanAlgebra` #62

## [v1.0.0-rc.1](https://github.com/purescript/purescript-prelude/releases/tag/v1.0.0-rc.1) - 2016-02-29

- Release candidate for the psc 0.8+ prelude

## [v0.1.4](https://github.com/purescript/purescript-prelude/releases/tag/v0.1.4) - 2016-01-27

- Fixed the escaping used in `Show` for `Char` and `String` (@michaelficarra)

## [v0.1.3](https://github.com/purescript/purescript-prelude/releases/tag/v0.1.3) - 2015-10-13

Move `Int` bounds to foreign module (@andyarvanitis)

## [v0.1.2](https://github.com/purescript/purescript-prelude/releases/tag/v0.1.2) - 2015-08-13

Add `Bounded Char` instance (@garyb)

## [v0.1.1](https://github.com/purescript/purescript-prelude/releases/tag/v0.1.1) - 2015-07-26

Export `unsafeCompare`

## [v0.1.0](https://github.com/purescript/purescript-prelude/releases/tag/v0.1.0) - 2015-06-30

This release works with versions 0.7.\* of the PureScript compiler. It will not work with older versions. If you are using an older version, you should require an older, compatible version of this library.

## [v0.1.0-rc.1](https://github.com/purescript/purescript-prelude/releases/tag/v0.1.0-rc.1) - 2015-06-06

Initial release candidate.
