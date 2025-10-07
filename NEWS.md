# metaMultiverse 0.2.0 (Development)

## New Features

* Added comprehensive "Getting Started" vignette with progressive examples
* Added in-depth "Theory and Practice" vignette covering E/U/N framework
* Added 24 integration tests covering complete pipeline workflows

## Improvements

* Deprecated legacy API functions now issue helpful `.Deprecated()` warnings:
  - `setup_which_factors()` → use `define_factors()`
  - `check_data_multiverse_enhanced()` → use `check_data_multiverse()`
  - `general_multiverse_enhanced()` → use `general_multiverse()`
  - `get_display_labels()` and `get_original_names()` → use factor_setup directly

## Bug Fixes

* Fixed test assertions in `check_data_multiverse()` tests to match actual return values
* Removed 6 outdated test files that tested non-existent or deprecated functions

## Test Suite

* **Test coverage**: 446 tests passing, 0 failing
* **New integration tests**: Full pipeline, multiple factors, custom groupings, visualizations
* **Removed**: Outdated tests for removed/deprecated functions

## Documentation

* New vignettes with fully executable code examples
* Improved error messages and deprecation warnings
* Better examples of E/U/N decision types

---

# metaMultiverse 0.1.0

* Initial CRAN release
* Core multiverse meta-analysis pipeline
* Support for multiple meta-analytic methods
* E/U/N decision type framework
* Specification curve and VoE plots
