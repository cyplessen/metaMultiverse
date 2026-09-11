# metaMultiverse 0.3.1 (2026-09-11)

## New Features

* `plot_spec_curve()` and `plot_voe()` now also accept the output of
  `run_pre_post_multiverse()` (new class `pre_post_multiverse`, with a
  `factors` element and a `print()` method) and plain data frames of
  results. A new `factors` argument names the factor columns to draw
  (optionally with labels as names); `plot_spec_curve()` shows `es_metric`
  and `imputed_post_sd` as how factors, and `plot_voe()` appends the given
  factors to its hover tooltip. Behaviour for `multiverse_result` input is
  unchanged.

## Test Suite

* Tests for plotting the stacked results of `run_pre_post_multiverse()`,
  the data-frame input path with labelled `factors`, and the input
  validation of both plot functions.

---

# metaMultiverse 0.3.0 (2026-09-11)

## New Features

* Pre/post (change-score) designs. `compute_pre_post_es()` turns arm-level
  pre/post means and SDs into `yi`/`vi` under a chosen metric:
  `"post_smd"` (post-test SMD), `"change_smd_r<r>"` (SMD of change scores,
  change SD imputed from the pre and post SDs under pre-post correlation `r`
  following Cochrane Handbook 6.5.2.8 unless reported), `"smc_r<r>"`
  (difference in standardized mean change, Becker 1988 variance) and
  `"adjusted_post_smd"` (reported adjusted between-group difference where
  available, post-test SMD otherwise). `imputed_post_sd` (`"borrow"`,
  `"exclude"`, `"change_smd"`) governs rows flagged `post_sd_imputed = TRUE`
  in the post-test metrics.
* `run_pre_post_multiverse()` runs the standard `define_factors()` ->
  `create_multiverse_specifications()` -> `run_multiverse_analysis()`
  pipeline once per row of an `es_grid` (metric x imputed-post-SD rule) and
  stacks the results, so that the effect-size metric becomes a "how" factor.
  Results carry `k_studies` and `studies_in_set` (`k` counts effect sizes,
  not studies) and `wf_*` columns are renamed to the factor labels.
  `which_factors` may be a function of the per-variant data, and
  `spec_filter` can prune the specification grid per variant.
* `register_metafor_estimators()` adds `dl`, `dl_hksj`, `reml_hksj` and
  `pm_hksj` (metafor, Hartung-Knapp-Sidik-Jonkman via `test = "knha"`) to
  the estimator registry. It is user-called, not run at load, so the default
  method set of `create_multiverse_specifications()` is unchanged.

## Test Suite

* New tests for every metric against `metafor::escalc()`, for the imputed
  post SD rules, for `run_pre_post_multiverse()` (stacking, function-valued
  `which_factors`, `spec_filter`, all-failing variants) and a regression test
  reproducing the Luo et al. (2020) corrigendum Table 1 post-test analysis
  (DerSimonian-Laird, g = 0.10 [-0.13, 0.33]).

---

# metaMultiverse 0.2.3 (Development)

## New Features

* Added N-type decision support for custom factors - custom factors with `decision = "N"` now properly create separate multiverses without adding "total_" option

## Bug Fixes

* `create_multiverse_specifications()` now returns the `wf_*`, `dependency` and `ma_method` columns as character rather than factor. Indexing a named label vector with a factor column (e.g. `labels[results$wf_1]`) silently used the integer codes and mislabelled results; the same columns in `run_multiverse_analysis()` output are now character as well.
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
