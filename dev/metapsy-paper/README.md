# Variance decomposition: parked for the Metapsy multiverse paper

Decision (2026-09-17): the package goes to JOSS first with a finished
surface. The variance decomposition (`decompose_variance()`,
`family_shares()`, `marginal_fork_means()`) is developed with the Metapsy
multiverse paper and is **not** part of the package until that work settles.

This folder is excluded from the package build (`dev/` is in
`.Rbuildignore`). It holds the last package version of the code and its
tests so nothing is lost:

* `decompose_variance.R`: the dev-line implementation plus one change made
  during the 0.4.0 merge: the fallback from the lme4 variance-component model
  to method-of-moments components is announced with a message and recorded in
  `attr(x, "estimator")`. With lme4 installed the numbers are identical to
  the dev line.
* `test-decompose-variance.R`: the five tests that covered it.

To use it: `pkgload::load_all()` (or `library(metaMultiverse)`), then
`source("dev/metapsy-paper/decompose_variance.R")`. It needs `%||%` from the
package namespace or rlang, and lme4 for the variance-component model.

`report_grid_result()` no longer prints a "Variance decomposition" section.
When the method returns to the package, restore that section in
`R/reporting.R` and the two lines in `vignettes/eunx-workflow.Rmd`.
