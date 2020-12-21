# ChangeLog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]
## Fixed
- A bug causing strings containing a colon to be interpreted as a mapping.
- A bug causing the list decoder to fail on empty strings.

## [1.1.0]
### Added
- A new `Yaml.Encode` module to encode Elm values into YAML formatted strings

### Fixed
- Some small documentation issues and typos

[Unreleased]: https://github.com/MaybeJustJames/yaml/compare/1.0.0...HEAD
[1.1.0]: https://github.com/MaybeJustJames/yaml/compare/1.0.0...1.1.0
