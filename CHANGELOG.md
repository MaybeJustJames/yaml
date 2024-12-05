# ChangeLog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [2.1.5]
### Fixed
- Added support for literal multi-line strings ([#36](https://github.com/MaybeJustJames/yaml/pull/36) by [lovebug356](https://github.com/lovebug356))

## [2.1.4]
### Changed
- Bumped Github action dependency versions ([#34](https://github.com/MaybeJustJames/yaml/pull/34))

### Fixed
- Fix list of record encoding bug #29 ([#35](https://github.com/MaybeJustJames/yaml/pull/35))

## [2.1.3]
### Fixed
- Updated `elm-explorations/test` to version 2 ([#30](https://github.com/MaybeJustJames/yaml/pull/30)
- Incorrect encoding of empty collection values in records ([#33](https://github.com/MaybeJustJames/yaml/pull/33))

## [2.1.2]
### Changed
- Error message for duplicated record keys now lists the duplicated keys ([#27](https://github.com/MaybeJustJames/yaml/pull/27)).

## [2.1.1]
### Fixed
- Throw error on records with duplicate keys ([#25](https://github.com/MaybeJustJames/yaml/pull/25) by [@ypyl](https://github.com/ypyl))

## [2.1.0]
### Fixed
- Examples in the `Yaml.Decode` module are not executable by `elm-verify-examples`

### Added
- `Yaml.Decode.andMap`, `Yaml.Decode.fromResult`, and `Yaml.Decode.fromMaybe` functions

## [2.0.2]
### Fixed
- A bug causing references in nested structures to break parsing
- A bug resolving aliases

## [2.0.1]
### Changed
- `Yaml.Decode` is now able to resolve [anchors and aliases](https://yaml.org/spec/1.2/spec.html#id2785586).

## [2.0.0]
### Changed
- The confusing `Yaml.Encode.Value` has been renamed `Yaml.Encode.Encoder` to distinguish it
  from `Yaml.Decode.Value`.

### Added
- A function to encode `Yaml.Decode.Value`.

## [1.1.1]
### Fixed
- A bug causing strings containing a colon to be interpreted as a mapping.
- A bug causing the list decoder to fail on empty strings.

## [1.1.0]
### Added
- A new `Yaml.Encode` module to encode Elm values into YAML formatted strings

### Fixed
- Some small documentation issues and typos

[Unreleased]: https://github.com/MaybeJustJames/yaml/compare/2.1.5...HEAD
[2.1.5]: https://github.com/MaybeJustJames/yaml/compare/2.1.4...2.1.5
[2.1.4]: https://github.com/MaybeJustJames/yaml/compare/2.1.3...2.1.4
[2.1.3]: https://github.com/MaybeJustJames/yaml/compare/2.1.2...2.1.3
[2.1.2]: https://github.com/MaybeJustJames/yaml/compare/2.1.1...2.1.2
[2.1.1]: https://github.com/MaybeJustJames/yaml/compare/2.1.0...2.1.1
[2.1.0]: https://github.com/MaybeJustJames/yaml/compare/2.0.2...2.1.0
[2.0.2]: https://github.com/MaybeJustJames/yaml/compare/2.0.1...2.0.2
[2.0.1]: https://github.com/MaybeJustJames/yaml/compare/2.0.0...2.0.1
[2.0.0]: https://github.com/MaybeJustJames/yaml/compare/1.1.1...2.0.0
[1.1.1]: https://github.com/MaybeJustJames/yaml/compare/1.1.0...1.1.1
[1.1.0]: https://github.com/MaybeJustJames/yaml/compare/1.0.0...1.1.0
