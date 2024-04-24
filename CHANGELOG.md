# Changelog

All notable changes of this package are documented in this file using the [Keep a Changelog] principles.

[Keep a Changelog]: https://keepachangelog.com/

<!-- ## Unreleased -->

## [0.0.5]

### Added

* [wiz] Add `wiz-pkg` macro for direct use.

### Changed

* [wiz] `:init` and `:config` now accept multiple lines without needing to be enclosed in `lambda`.
* [wiz] Make delayed require when use `:package`, `:load` and `:load-if-exists` keyword.

### Fixed

* [wiz] Fixed the list returned by `wiz--form-to-alist` being reversed.
* [wiz-kwd] Fixed runtime error due to undefined variable.

## [0.0.4]

### Added

* [wiz-pkgs] Add `wiz-pkgs` function for package installation.
* [wiz] Add `wiz-map` macro to help expression expansion at compile time.

## [0.0.3]

### Added

* [wiz-key] Add `wiz-keys` for key binding settings.

### Changed

* [wiz] Make the `:init` keyword's lambda always return NIL for optimization.

## [0.0.2]

### Added

* Add `wiz` macro to compile [feature] settings.

[feature]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Named-Features.html

## [0.0.1]

### Added

 * Add `wiz-env` macro to import environment variables at compile time.
