# metaMultiverse 0.4.0 (development)

This release merges the estimand-aware architecture that was developed on a
separate line since 0.2.3. Everything from 0.3.0 and 0.3.1 is kept, and the
existing `define_factors()` pipeline is unchanged.

## New Features

* Decisions as forks. `fork()` declares a named decision with a
  plain-language ruling (`equivalent`, `uncertain`, `changes_question`,
  `indefensible`; letter aliases E/U/N/X), a clinical or analytic family and
  a required machine-readable justification. `domain_config()` and
  `validate_domain_config()` bundle forks, eligibility rules and the estimand
  declaration, and validate them against the data at load time
  (`fork_availability_matrix()`).
* Estimand cells. `build_specification_grid()` and `run_multiverse_grid()`
  cross the forks into a grid in which question-changing forks define
  estimand cells. Effects are summarized per cell only
  (`summarize_effects()`, guarded by `assert_single_estimand()`); across
  cells there is labeled divergence (`summarize_divergence()`), never a
  pooled effect. Indefensible options run in a separate bias layer.
* Nothing is lost silently. An attrition ledger records every specification
  that was not estimated, with a machine-readable reason
  (`attrition_report()`, `attrition_summary()`); `audit_universes()` and
  `lookup_spec()` give per-specification diagnostics and traceability.
* Plausibility flags (`flag_effect_sizes()`, `add_plausibility_flags()`,
  `plausibility_fork()`), fragility metrics (`fragility_metrics()`, `m1` to
  `m6`, `check_fork_tolerances()`), comparability helpers
  (`common_core_specs()`), reporting (`report_grid_result()`,
  `warrant_table()`, `export_prereg_config()`), plots
  (`plot_grid_spec_curve()`, `plot_two_layer()`) and
  `simulate_multiverse_data()` for prototyping.
* New vignette `eunx-workflow` walks through the fork workflow on simulated
  data.

## Improvements

* Estimator results carry `se`, `tau2`, `i2`, `k`, `convergence` and `notes`
  in addition to the estimate, interval and p-value, including the four
  estimators from `register_metafor_estimators()`.
* Defaults that come from the Metapsy multiverse analyses (`min_studies = 5`,
  `mid = 0.24`, `i2_high = 50`, `dominance_share = 0.5`,
  `within_study_correlation = 0.6`) are documented as conventions of that
  domain rather than universals.

## Infrastructure

* The package now declares `R (>= 4.1.0)`: the code has used the native
  pipe since 0.2.x, so the previous `R (>= 3.5)` was wrong.
* GitHub Actions workflow running `R CMD check` on macOS, Windows and
  Ubuntu (release, devel, oldrel).
* New tests assert that every registered estimator fills `se`, `tau2`,
  `i2`, `k` and `convergence`, checked against metafor and meta.

## Bug Fixes

* `run_multiverse_analysis()`: `full_set` was computed against row order and
  was wrong whenever `es_id` was not `1..n`; it now compares against the
  actual ids.
* `create_multiverse_specifications()` warns when a requested method drops
  out of the grid because none of the requested dependencies is compatible
  with it, instead of dropping it silently.
* `check_data_multiverse()` only treats columns matching `wf_<number>` as
  which factors, not every column starting with `wf`.
* `generate_multiverse_report()`: the share of significant analyses uses all
  analyses as the denominator and reports how many carry a p-value; removed
  deprecated ggplot2 usage.
* `generate_multiverse_report_text()`: publication-bias and consistency
  bullets appear only when their statistics were supplied; two quoting
  errors fixed.

---

