# Merge brief: bringing the dev architecture into the live package

Target: merge `metaMultiverse-dev` (0.9.0) into this repo (0.3.0) without
changing any result. Four stages, a human gate after each. Written
2026-09-16 from a full static comparison of both trees; the comparison
itself is at `../../metaMultiverse-dev-vs-live.md`.

## Amendments agreed 2026-09-17 (override the text below where they conflict)

1. The live line is **0.3.1**, not 0.3.0. When this brief was written the
   local checkout was four commits behind `origin/main` (plot support for
   `run_pre_post_multiverse()` output, releases v0.3.0 and v0.3.1 with a
   Zenodo DOI, `Authors@R`, dynamic `inst/CITATION`). Stage 0 starts with
   `git pull --ff-only` on `main`.
2. Rollback tag is `v0.3.1-premerge`. The checkpoint commit is made on
   `feature/fork-architecture`, not on `main`, so `main` stays identical
   to `origin/main`.
3. Stage 0 does not `git add -A`. The untracked `metaMultiverse_pr/`
   sandbox folder and the local `.claude/settings.local.json` edits stay
   out of the commit (excluded locally via `.git/info/exclude` and
   `skip-worktree`).
4. **Fourth live-only invariant:** the 0.3.1 plotting work
   (`R/helpers_plot_input.R`, the `factors` argument and
   `pre_post_multiverse` class handling in `R/plot_spec_curve.R`,
   `R/plot_VoE.R`, `R/helpers_voe.R`). Diff these against dev before
   Stage 2; never take dev's copies wholesale.
5. `3b9cc47` is dev's root commit (a snapshot of 0.2.3). The repos share
   no git history; this is a file-level port, as the stages already assume.
6. Stage 0b runs dev's suite on a `git archive` export of dev HEAD in a
   scratch directory, so nothing is written into the dev folder.
8. **The variance decomposition is not merged** (decided 2026-09-17,
   after Stage 2). JOSS comes first with a finished surface; the
   decomposition is developed with the Metapsy multiverse paper.
   `R/decompose_variance.R` and its tests are parked in
   `dev/metapsy-paper/` (outside the build), the "Variance decomposition"
   section is removed from `report_grid_result()`,
   `test-reporting-decomposition.R` is split into `test-reporting.R`, and the
   vignette chunk is cut. This overrides "take all twelve files" and trap 3
   below: the call in `reporting.R` was removed together with the file, so
   nothing fails quietly. Stage 4 is unaffected: it compares grid results and
   the attrition ledger only.
7. Wider goal (see project memory): combine features, make the package
   submittable to JOSS, and develop the variance decomposition for the
   Metapsy multiverse paper. Ambiguous merge choices are made with those
   in mind.

---

**Read this whole file before touching anything.** The dangerous failures
here are silent, not loud.

---

## Rules of engagement

- Work **in this repo only**. `../../metaMultiverse-dev` is a **read-only
  source**. Never write, commit, checkout, stash or clean anything there.
  Its `analysis/output/` is the regression baseline and its
  `materials-code-data/` is the input data; both are tracked in git and
  must stay untouched.
- Never push. Never force anything. Never work on `main`.
- **Stop at every gate.** Do not start the next stage because the previous
  one looked fine. A gate is passed when a human has read the output.
- If a gate fails, stop and report. Do not repair forward into the next
  stage.
- Never delete `tests/testthat/test-create_multiverse_specifications.R`
  or `R/pre_post_effect_sizes.R`.

---

## Stage 0: pre-flight

This repo currently has uncommitted work. Settle it first.

```bash
git status                 # expect: .claude/settings.local.json,
                           # dev/metaMultiverseDevelopment.Rmd, untracked metaMultiverse_pr/
git add -A && git commit -m "chore: checkpoint before fork-architecture merge"
git tag v0.3.0-premerge
git switch -c feature/fork-architecture
```

`v0.3.0-premerge` is the rollback point. Rollback is
`git switch main && git branch -D feature/fork-architecture`.

**Gate 0.** Working tree clean, tag exists, branch is
`feature/fork-architecture`.

---

## Stage 0b: capture the regression baseline

Do this **before** any code moves. The 12-domain outputs in dev are what
prove the merge changed no numbers, and they are worth confirming
reproducible on this machine first.

```bash
cd ../../metaMultiverse-dev
Rscript -e 'pkgload::load_all("."); testthat::test_dir("tests/testthat")'
```

Expect roughly 715 passing, 0 failing. If dev's own suite does not pass on
this machine, stop: the baseline is not trustworthy and nothing below
means anything.

**Gate 0b.** Dev suite green. Record the actual pass count here: **715 passed, 0 failed, 0 errors, 1 skipped** (157 tests, dev HEAD `165696c`, run 2026-09-17 on a `git archive` export; dev folder left untouched)

---

## Stage 1: bug fixes and enriched results

Nothing in this stage adds public API. Every change is a defect fix.

### Take whole files from dev

- `R/universe-result-class.R` (4 scalars becomes `b`, `ci.lb`, `ci.ub`,
  `pval`, `se`, `tau2`, `i2`, `k`, `convergence`, `notes`)
- `R/helpers_estimators.R` (every estimator populates the new fields)

Both need their UTF-8 roxygen converted to ASCII on the way in (invariant
2 in `CLAUDE.md`). `helpers_estimators.R` is where most of it lives.

### Take single hunks, not whole files

Copying these files wholesale reverts live-only fixes.

| file | take only |
|---|---|
| `R/check_data_multiverse.R` | the anchored regex `^wf_[0-9]+$` |
| `R/create_multiverse_specifications.R` | the `dropped_methods` warning block. **Keep `stringsAsFactors = FALSE`.** |
| `R/run_multiverse_analysis.R` | `full_set_val <- paste(data$es_id, collapse = ",")` |
| `R/generate_multiverse_report.R` | `n_with_pval` + the `prop_significant` denominator, `aes_string` to `.data`, `size` to `linewidth` |
| `R/generate_report.R` | the optional pubbias/consistency sections and the two glue quoting fixes |
| `R/globals.R` | drop `aes_string`, add `@importFrom rlang .data` |
| `DESCRIPTION` | add `rlang` to Imports |

### One new edit, not a copy

The four estimators registered by `register_metafor_estimators()` in
`R/pre_post_effect_sizes.R` (line ~320) call `new_universe_result()` with
four positional arguments. After this stage every other estimator fills
`se`, `tau2`, `i2`, `k`. Bring these into line:

```r
out <- new_universe_result(mod$b, mod$ci.lb, mod$ci.ub, mod$pval,
                           se = mod$se, tau2 = mod$tau2, i2 = mod$I2,
                           k = mod$k)
```

Add a test asserting these four estimators return non-NA `se` and `tau2`.

### Take the new test file

- `tests/testthat/test-phase0-regressions.R`

One of its cases, "full_set is correct when es_id is not 1..n row order",
is the direct guard on the `run_multiverse_analysis.R` change.

### Gate 1

```bash
Rscript -e 'pkgload::load_all("."); testthat::test_dir("tests/testthat")'
```

All green, and specifically confirm by eye in the output that
`test-pre_post.R` and `test-create_multiverse_specifications.R` both ran
and passed. Then:

```bash
git add -A && git commit -m "fix: phase 0 defects + enriched universe_result"
```

---

## Stage 2: the new architecture

### Take all twelve files. Do not cherry-pick.

```
R/vocabulary.R          R/fork.R              R/domain_config.R
R/build_grid.R          R/run_grid.R          R/flag_effect_sizes.R
R/fragility_metrics.R   R/comparability.R     R/decompose_variance.R
R/reporting.R           R/plot_grid.R         R/simulate_multiverse_data.R
```

Taking a subset is tempting and wrong: `R/reporting.R` calls
`decompose_variance()` (line ~138) and `family_shares()` (line ~157)
inside a `tryCatch`. Drop `decompose_variance.R` and the call fails
quietly at runtime while `R CMD check` flags an undefined global. If some
of these should not be public API yet, mark them experimental in the
documentation. Do not solve it by omission.

These twelve files call **none** of the legacy API. They share only
`run_select_dependency()`, `run_aggregate_dependency()`,
`run_modeled_dependency()` and the estimator registry. Verified: zero
function-name collisions with anything already in this package.

### Take the six test files

```
tests/testthat/test-vocabulary.R        tests/testthat/test-fork-config-grid.R
tests/testthat/test-run-grid.R          tests/testthat/test-flag-effect-sizes.R
tests/testthat/test-comparability-fragility.R
tests/testthat/test-reporting-decomposition.R
```

### Then

1. Convert all UTF-8 in the new roxygen to ASCII.
2. `Rscript -e 'roxygen2::roxygenise(".")'`
3. Do **not** copy dev's `man/` directory. Regenerate. Copying it deletes
   this repo's `compute_pre_post_es.Rd` and `register_metafor_estimators.Rd`.

### Gate 2

```bash
Rscript -e 'pkgload::load_all("."); testthat::test_dir("tests/testthat")'
git diff NAMESPACE
ls man | wc -l
```

- Suite green, roughly 715 plus this repo's existing tests.
- `NAMESPACE` gained the new exports and **lost none**. Confirm
  `compute_pre_post_es`, `run_pre_post_multiverse` and
  `register_metafor_estimators` are still exported.
- `man/` contains both the new Rd files and the pre/post ones.

```bash
git add -A && git commit -m "feat: estimand-aware fork architecture from dev line"
```

---

## Stage 3: metadata and hygiene

- `DESCRIPTION`: version to **0.4.0**. Trim the Description field; dev's
  runs to 19 lines and reads like an abstract, which CRAN rejects. Keep
  the new architecture summarized in about five lines.
- `.Rbuildignore`: add `^CLAUDE\.md$` and `^MERGE_BRIEF\.md$`. Keep
  `^CITATION\.cff$`.
- `NEWS.md`: a 0.4.0 entry on top describing the merge. Both histories
  share everything below 0.2.3, so splice rather than replace: keep this
  repo's 0.3.0 entry intact.
- `vignettes/eunx-workflow.Rmd` from dev.
- Optionally one example domain config to `inst/extdata/` for the
  vignette.

**Never copy** from dev: `analysis/`, `paper/`, `references/`,
`materials-code-data/`, `fable5_metamultiverse_prompt.md`,
`robma_benchmark_prompt.md`, `dev/`, `.claude/`.

### Gate 3

```bash
R CMD build .
R CMD check --as-cran metaMultiverse_0.4.0.tar.gz
```

0 errors, 0 warnings. NOTEs about installed size or a large namespace are
acceptable; any NOTE naming an undefined global, a missing import or a
non-ASCII Rd file is not, and means stage 2 was incomplete.

```bash
git add -A && git commit -m "chore: 0.4.0 metadata, buildignore, NEWS"
```

---

## Stage 4: the real regression test

`R CMD check` proves the package is well formed. It does not prove the
numbers did not move. This does. Write `regression_check.R` at the repo
root (do not commit it):

```r
LIVE <- normalizePath(".")
DEV  <- normalizePath("../../metaMultiverse-dev")

pkgload::load_all(LIVE, quiet = TRUE)
source(file.path(DEV, "analysis/domain_configs.R"))

data_dir  <- file.path(DEV, "materials-code-data/data/reanalysis")
base_dir  <- file.path(DEV, "analysis/output")          # read-only baselines
domains   <- names(domain_specs)

for (dom in domains) {
  pd   <- prepare_domain(dom, data_dir)
  grid <- build_specification_grid(pd$config, pd$data, strict = TRUE)
  new  <- run_multiverse_grid(grid, within_study_correlation = 0.6,
                              progress = FALSE)
  old  <- readRDS(file.path(base_dir, paste0("result_", dom, ".rds")))

  cmp_r <- all.equal(old$results,   new$results)
  cmp_a <- all.equal(old$attrition, new$attrition)
  cat(sprintf("%-22s results: %-9s attrition: %s\n", dom,
              if (isTRUE(cmp_r)) "IDENTICAL" else "DIFFERS",
              if (isTRUE(cmp_a)) "IDENTICAL" else "DIFFERS"))
  if (!isTRUE(cmp_r)) print(cmp_r)
  if (!isTRUE(cmp_a)) print(cmp_a)
}
```

Run it: `Rscript regression_check.R`. Takes roughly two minutes.

Gotcha: `analysis/domain_configs.R` calls `library(metaMultiverse)` at
line 17. `pkgload::load_all()` has already attached the package, so this is
a no-op, but if a released metaMultiverse is installed in your library,
confirm the loaded version is the branch and not the installed one
(`packageVersion("metaMultiverse")` and `find.package`).

### Gate 4

All 12 domains report IDENTICAL for both results and attrition.

If any differ, that is the finding, not a problem to patch around. Report
which domain and what `all.equal` said, and stop. The likely culprits, in
order: the `full_set` change (only bites when `es_id` is not row order),
the enriched `universe_result` altering a downstream join, or an estimator
whose new `se`/`tau2` extraction picked the wrong element.

---

## Known traps, collected

1. Copying `create_multiverse_specifications.R` wholesale reverts
   `stringsAsFactors = FALSE`. Silent mislabelling, no error.
2. Copying dev's `man/` deletes the pre/post Rd files.
3. Excluding `decompose_variance.R` breaks `reporting.R` quietly.
4. UTF-8 roxygen fails `--as-cran` on the PDF manual, at stage 3, long
   after it was introduced.
5. `test-reporting-decomposition.R` spans reporting and decomposition; if
   anything is held back, this file needs splitting, not deleting.
6. `pre_post_effect_sizes.R` depends on all four legacy files touched in
   stage 1. `test-pre_post.R` is the only thing watching.

## What this merge deliberately does not do

It does not remove the legacy positional `wf_` API (a 1.0 decision), and
it does not re-express `run_pre_post_multiverse()` as a fork. The second
one is planned; it is the first feature after 0.4.0 and is specified
below. It stays out of the merge because it changes the runner loop, and
the runner loop is what Gate 4 proves unchanged.

---

## Next step after 0.4.0: the effect-size metric as a fork

### Why the wrapper survives the merge

`run_multiverse_grid()` does exactly two things to a specification before
estimating:

```r
# data forks: row subsetting only
dat <- dat[dat[[column]] %in% vals, ]

# analysis forks: overrides can set exactly two engine settings
if (!is.null(ov$ma_method))  ma_method  <- ov$ma_method
if (!is.null(ov$dependency)) dependency <- ov$dependency
```

Then the dependency handlers read whatever `yi`/`vi` are already in `dat`.
A pre/post metric is neither of those. It **recomputes `yi` and `vi`**
from arm-level columns, upstream of subsetting and estimation. It is a
third kind of decision, a column transformation, and the architecture has
no slot for it. It is not an estimator and must not be registered as one.

### The change: a `transform` hook

Two files, roughly 30 lines, one new test file.

**`R/fork.R`.** Add an optional `transform` argument for analysis forks: a
named list mapping level label to `function(data) -> data`. Validate it
the way `overrides` is validated (every name must be a level, every value
must be a function). Store it on the `mv_fork` object. Print it in
`print.mv_fork()`.

**`R/run_grid.R`.** Apply transforms right after the data-fork subsetting
and **before** the `min_studies` check:

```r
for (f in forks) {
  if (f$type == "analysis" && !is.null(f$transform)) {
    lv <- spec[[f$name]]
    if (!is.null(lv) && !is.na(lv) && lv %in% names(f$transform)) {
      dat <- tryCatch(f$transform[[lv]](dat), error = function(e) {
        note(spec$spec_id, "structural",
             paste0("transform for level ", lv, " of fork ", f$name,
                    " failed: ", conditionMessage(e)))
        NULL
      })
      if (is.null(dat)) break
    }
  }
}
if (is.null(dat)) { if (progress) utils::setTxtProgressBar(pb, i); next }

k_studies <- length(unique(dat$study))   # existing check, now after transform
```

The ordering is the point. `imputed_post_sd = "exclude"` drops rows, so
the transform must run before `min_studies` is evaluated. Doing it this
way means the attrition ledger records per-metric row loss with a reason,
which the wrapper cannot do at all.

### Then the metric is an ordinary fork

```r
fork("es_metric",
     type = "analysis", decision = "uncertain", family = "analytic",
     justification = list(
       basis = "citation", citation = "Cochrane Handbook 6.5.2.8",
       text = "Post-test, change-score and SMC metrics target the same
               between-group estimand under different assumptions about
               baseline balance and the pre-post correlation."),
     levels = c("post_smd", "change_smd_r0.5", "smc_r0.5",
                "adjusted_post_smd"),
     reference = "post_smd",
     transform = list(
       post_smd          = \(d) compute_pre_post_es(d, metric = "post_smd"),
       change_smd_r0.5   = \(d) compute_pre_post_es(d, metric = "change_smd_r0.5"),
       smc_r0.5          = \(d) compute_pre_post_es(d, metric = "smc_r0.5"),
       adjusted_post_smd = \(d) compute_pre_post_es(d, metric = "adjusted_post_smd")))
```

`compute_pre_post_es()` is untouched. `imputed_post_sd` becomes a second
fork or a second level set, whichever reads better in the warrant table.
The metric now carries a justification, appears in `decompose_variance()`
as an analytic component, and is subject to `check_fork_tolerances()`
like everything else.

### What gets deleted

`run_pre_post_multiverse()` and its tests, after the equivalence test
below passes. `register_metafor_estimators()` stays; it is orthogonal.

### The ruling is a decision, not a default

The wrapper treated the metric as a positional "how" factor and asked
nothing more. `fork()` will refuse to build without a ruling. Is post-test
SMD versus change-score SMD `uncertain`, or does it change the question?
They target the same estimand under different assumptions, so `uncertain`
is the defensible default. Write that down with its basis before the
code exists, not after.

### Regression check for this step

The equivalence proof is a side-by-side on the Luo et al. (2020) data
already used in `test-pre_post.R`:

1. Old path: `run_pre_post_multiverse()` with the same `es_grid`.
2. New path: `build_specification_grid()` with the `es_metric` fork, then
   `run_multiverse_grid()`.
3. For every (metric, ma_method, dependency) triple present in both,
   `b`, `ci.lb`, `ci.ub` must match to `tolerance = 1e-10`. The
   corrigendum anchor (DerSimonian-Laird, g = 0.10 [-0.13, 0.33]) must
   hold on the new path.
4. Rerun `regression_check.R` from Stage 4. The 12 domains have no
   `es_metric` fork, so they must still report IDENTICAL. That is the
   proof that the hook is inert when unused.

Gate: all four hold. Then, and only then, delete the wrapper.
