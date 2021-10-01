# Changelog

Notable changes to this project are documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Breaking changes:

New features:

Bugfixes:

Other improvements:

## [v4.0.0](https://github.com/purescript/purescript-newtype/releases/tag/v4.0.0) - 2021-02-26

Breaking changes:
- Added support for PureScript 0.14 and dropped support for all previous versions (#24)
- Removed `op`, which was deprecated in v1.1.0 in favor of `un` (#24)
- Replaced explicit wrapping and unwrapping of newtypes by coercions (#22)
- Added `Coercible` as a superclass of `Newtype` and removed the `wrap` and `unwrap` class members (#22)
  The `wrap` and `unwrap` functions still exist, but they are no longer members of the `Newtype` class and are simple aliases for `coerce`.

New features:

Bugfixes:

Other improvements:
- Migrated CI to GitHub Actions and updated installation instructions to use Spago (#25)
- Added a changelog and pull request template (#26)

## [v3.0.0](https://github.com/purescript/purescript-newtype/releases/tag/v3.0.0) - 2018-05-22

- Updated for PureScript 0.12

## [v2.0.0](https://github.com/purescript/purescript-newtype/releases/tag/v2.0.0) - 2017-03-25

- Update for PureScript 0.11

## [v1.3.0](https://github.com/purescript/purescript-newtype/releases/tag/v1.3.0) - 2017-02-03

- Added `over` and `under` variants for binary operations (@mlang)

## [v1.2.0](https://github.com/purescript/purescript-newtype/releases/tag/v1.2.0) - 2016-12-31

- Added `traverse` and `collect` functions

## [v1.1.0](https://github.com/purescript/purescript-newtype/releases/tag/v1.1.0) - 2016-10-28

- Introduced `un` as a replacement for `op` - `op` is still present but will be removed in the next major release (#4)
- Documented the class law (#3)
- Fixed a code block in the documentation

## [v1.0.0](https://github.com/purescript/purescript-newtype/releases/tag/v1.0.0) - 2016-10-02

- Initial release for the 0.10 compiler.

## [v0.1.0](https://github.com/purescript/purescript-newtype/releases/tag/v0.1.0) - 2016-09-23

- Initial versioned release.
