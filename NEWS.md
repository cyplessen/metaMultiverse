# metaMultiverse 0.2.3 (Development)

## New Features

* Added N-type decision support for custom factors - custom factors with `decision = "N"` now properly create separate multiverses without adding "total_" option

## Bug Fixes

* Fixed `create_multiverse_specifications()` to properly handle N-type decisions for custom factor groups
* Custom factors no longer bypass decision type logic

## Test Suite

* Added test for N-type simple factors verifying separate multiverses without total option
* Added test for N-type custom factors verifying proper multiverse separation

---

# metaMultiverse 0.2.2 (Development)

## New Features

* Added bidirectional format compatibility between metafor (`yi`/`vi`) and metaPsyTools (`.g`/`.g_se`)
* Added auto-generation of `es_id` column when missing - uses row numbers as unique identifiers

## Improvements

* `check_data_multiverse()` now accepts data in either metafor or metaPsyTools format
* After validation, data contains both formats for cross-package compatibility
* Improved error messages for missing effect size columns

## Test Suite

* Added 11 tests for bidirectional format compatibility
* Added 3 tests for auto-generated `es_id`
* Updated integration tests for new error messages

---

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
