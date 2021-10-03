# Changelog

Notable changes to this project are documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Breaking changes:

New features:

Bugfixes:

Other improvements:

## [v5.0.0](https://github.com/purescript/purescript-control/releases/tag/v5.0.0) - 2021-02-26

Breaking changes:
  - Added support for PureScript 0.14 and dropped support for all previous versions (#61)
  - Deprecated `MonadZero` (#64, #66, #68, #70)

New features:

Bugfixes:

Other improvements:
  - Migrated CI to GitHub Actions and updated installation instructions to use Spago (#65)
  - Added a CHANGELOG.md file and pull request template (#72, #73)
  - Added examples for `Alt` (#58) 

## [v4.2.0](https://github.com/purescript/purescript-control/releases/tag/v4.2.0) - 2019-09-05

Added `Newtype` instance for `Alternate` (@masaeedu)

## [v4.1.0](https://github.com/purescript/purescript-control/releases/tag/v4.1.0) - 2018-07-17

- Restored `Alternate` newtype that used to exist in `-monoid` (@Thimoteus)

## [v4.0.0](https://github.com/purescript/purescript-control/releases/tag/v4.0.0) - 2018-05-22

- Updated for PureScript 0.12 
- The definition of `fix` was fixed to allow sharing

## [v3.3.1](https://github.com/purescript/purescript-control/releases/tag/v3.3.1) - 2017-09-21

Docs fix for `MonadZero` (@spicydonuts)

## [v3.3.0](https://github.com/purescript/purescript-control/releases/tag/v3.3.0) - 2017-06-04

`Lazy` instance for `Unit` (@matthewleon)

## [v3.2.0](https://github.com/purescript/purescript-control/releases/tag/v3.2.0) - 2017-05-29

- Added `Extend` instance for `Array` (@i-am-tom)

## [v3.1.0](https://github.com/purescript/purescript-control/releases/tag/v3.1.0) - 2017-05-28

`Lazy` instance for functions (@matthewleon)

## [v3.0.0](https://github.com/purescript/purescript-control/releases/tag/v3.0.0) - 2017-03-25

- Updated for PureScript 0.11

## [v2.0.0](https://github.com/purescript/purescript-control/releases/tag/v2.0.0) - 2016-10-01

- Updated dependencies

## [v1.0.0](https://github.com/purescript/purescript-control/releases/tag/v1.0.0) - 2016-06-01

This release is intended for the PureScript 0.9.1 compiler and newer.

**Note**: The v1.0.0 tag is not meant to indicate the library is “finished”, the core libraries are all being bumped to this for the 0.9 compiler release so as to use semver more correctly.

## [v1.0.0-rc.2](https://github.com/purescript/purescript-control/releases/tag/v1.0.0-rc.2) - 2016-05-19

- Fix warning

## [v1.0.0-rc.1](https://github.com/purescript/purescript-control/releases/tag/v1.0.0-rc.1) - 2016-03-01

- Release candidate for the psc 0.8+ core libraries

## [v0.3.2](https://github.com/purescript/purescript-control/releases/tag/v0.3.2) - 2015-11-02

- Removed unused imports

## [v0.3.1](https://github.com/purescript/purescript-control/releases/tag/v0.3.1) - 2015-09-16

- Fixed unused type variable warning (@zudov)

## [v0.3.0](https://github.com/purescript/purescript-control/releases/tag/v0.3.0) - 2015-06-30

This release works with versions 0.7.\* of the PureScript compiler. It will not work with older versions. If you are using an older version, you should require an older, compatible version of this library.

## [v0.3.0-rc.2](https://github.com/purescript/purescript-control/releases/tag/v0.3.0-rc.2) - 2015-06-06

- Added instances for `Array` that were orphans previously.

## [v0.3.0-rc.1](https://github.com/purescript/purescript-control/releases/tag/v0.3.0-rc.1) - 2015-06-06

Initial release candidate of the library intended for the 0.7 compiler.

## [v0.2.6](https://github.com/purescript/purescript-control/releases/tag/v0.2.6) - 2015-03-24

Add `extend`.

## [v0.2.5](https://github.com/purescript/purescript-control/releases/tag/v0.2.5) - 2015-03-22

Updated docs

## [v0.2.4](https://github.com/purescript/purescript-control/releases/tag/v0.2.4) - 2015-03-19

Add `filterM` (@pseudonom)

## [v0.2.3](https://github.com/purescript/purescript-control/releases/tag/v0.2.3) - 2015-03-18

Update docs (@hdgarrood)

## [v0.2.2](https://github.com/purescript/purescript-control/releases/tag/v0.2.2) - 2015-02-26

- Added `<$` and `$>` (@joneshf)

## [v0.2.1](https://github.com/purescript/purescript-control/releases/tag/v0.2.1) - 2014-08-26

- Added `Extend` and `Comonad` (@joneshf)

## [v0.2.0](https://github.com/purescript/purescript-control/releases/tag/v0.2.0) - 2014-08-11

- Added `Alternative`, `Alt`, `Plus` (@garyb)
- Added `MonadPlus`, `guard` (@garyb)
- Added `Lazy`, `Lazy1`, `Lazy2`, `fix`, `fix1`, `fix2` (@garyb & @paf31)

## [v0.1.1](https://github.com/purescript/purescript-control/releases/tag/v0.1.1) - 2014-06-14

- Now uses "proper" `Unit` type instead of `{}` (garyb)

## [v0.1.0](https://github.com/purescript/purescript-control/releases/tag/v0.1.0) - 2014-04-25



