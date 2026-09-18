# Contributing to metaMultiverse

Bug reports, questions and pull requests are welcome.

## Reporting a problem or asking a question

Open an issue at <https://github.com/cyplessen/metaMultiverse/issues> and
pick the matching template. For a bug, include a minimal reproducible
example (a few lines of data and the call that fails), what you expected,
what happened, and the output of `sessionInfo()`.

## Proposing a change

1. Open an issue first if the change is more than a small fix, so the
   approach can be agreed before you write code.
2. Fork the repository and create a branch from `main`.
3. Make the change, add or update tests, and add a line to `NEWS.md`.
4. Open one pull request against `main`. The R CMD check workflow runs on
   every pull request.

## Development setup

```r
# install.packages("pak")
pak::local_install_dev_deps()

pkgload::load_all()
testthat::test_dir("tests/testthat")   # full suite
roxygen2::roxygenise()                 # regenerate man/ and NAMESPACE
```

```bash
R CMD build .
R CMD check --as-cran metaMultiverse_*.tar.gz
```

Set `options(metaMultiverse.skip_slow = TRUE)` to skip the slow Bayesian
estimator while iterating.

## Conventions

* R, `snake_case`, base pipe `|>`, R >= 4.1.
* Documentation with roxygen2; never edit `man/` or `NAMESPACE` by hand.
* Keep roxygen comments ASCII-only so the PDF manual builds: write
  `\eqn{\tau^2}` instead of the symbol and `>=` instead of the sign.
* Tests with testthat (edition 3). New behaviour needs a test; a bug fix
  needs a test that fails without the fix. Where a reference
  implementation exists (metafor, meta), test against it directly.
* Changes that alter numerical results must say so in the pull request and
  in `NEWS.md`.

## Adding an estimator

Estimators live in `R/helpers_estimators.R`. An estimator is a function of
a data frame with `yi` and `vi` (and `study`, `es_id` for models of
dependency) that returns `new_universe_result()` with the estimate, its
confidence limits and p-value, and, where defined, `se`, `tau2`, `i2` and
`k`. Register it in `R/register_defaults.R` with the dependency strategies
it supports, and add it to `tests/testthat/test-estimator-fields.R`.

## Code of conduct

By participating you agree to the [code of conduct](CODE_OF_CONDUCT.md).
