# metaMultiverse — project instructions

R package for multiverse meta-analysis. Public repo `cyplessen/metaMultiverse`.
This is the **live** line and the only place package code lives.

Status (2026-09-17): the estimand-aware architecture from the former
`metaMultiverse-dev` line (0.9.0) has been ported into this repo as 0.4.0,
stage by stage following `MERGE_BRIEF.md`, whose gate log records every
result. The 12-domain regression proved the port moved no numbers. The
variance decomposition was deliberately left out: it is parked in
`dev/metapsy-paper/` and is developed with the Metapsy multiverse paper.

## Project layout: package plus paper

- This repo: the package, plus the JOSS paper in `paper/` (excluded from
  the build). `main` is what is released.
- One separate repo per paper (`luo-reanalysis`, the Metapsy multiverse
  paper), each pinning a tagged package release. Methods a paper needs are
  developed in the paper repo as plain R files sourced on top of the
  installed package, and enter the package through one PR and a new tag
  once they have settled.
- A paper never edits package code in place.

## Invariants: three things that exist only here

A merge from dev would silently undo all three. Two of them fail quietly,
with no error and no failing test unless the guard below survives.

1. **`stringsAsFactors = FALSE`** in `create_multiverse_specifications()`
   (commit `c833768`). Without it, `expand.grid` returns factors, and
   indexing a label vector by a factor column silently uses the integer
   codes, mislabelling every result. `tests/testthat/test-create_multiverse_specifications.R`
   is the guard. **Never delete that test file**, however superseded it
   looks.
2. **ASCII-only Rd.** Commit `cff5201` converted the roxygen so the PDF
   manual builds under `R CMD check --as-cran`. Write `\eqn{\tau^2}`, not
   the literal symbol; `>=`, not the sign; `\enc{Röver}{Roever}`. Dev's
   files violate this and must be converted on the way in.
3. **`R/pre_post_effect_sizes.R`** is the entire 0.3.0 feature set:
   `compute_pre_post_es()`, `run_pre_post_multiverse()` and
   `register_metafor_estimators()`. Nothing in dev supersedes or replaces
   it. It is also the file most exposed to a merge, because it calls
   `define_factors()`, `create_multiverse_specifications()`,
   `run_multiverse_analysis()` and `check_data_multiverse()`, which are
   exactly the four legacy files dev also modified.

## Design rules for the new architecture

These come from the dev line and govern any code in the fork/grid system.

- **Two-layer rule.** Effect summaries exist per estimand cell only;
  `assert_single_estimand()` guards it at runtime. Across cells there is
  only labeled divergence. Never pool effects across cells.
- **Every fork carries a justification.** `fork()` refuses to construct
  without one. Indefensible (X) options run only in the bias layer.
- **Nothing is lost silently.** Availability matrix plus attrition ledger
  with machine-readable reasons. The headline count is justified
  converged specifications, never raw combinatorial grid size.
  Frequencies are labeled descriptive, never inference.
- **Estimability.** A fit counts only with a finite estimate and finite CI
  limits (`is_valid_universe()`). Optimizer failures go to the ledger.
- Paper-specific conventions (`min_studies = 5`, `mid = 0.24`,
  `i2_high = 50`, `dominance_share = 0.5`) are the CPR paper's decisions,
  not universals. If they ship as defaults, document the domain they came
  from.

## Conventions

R, snake_case, base pipe `|>`, testthat edition 3, roxygen2 8.0.0.
New files get `# ABOUTME:` header comments. Prototype new mechanisms on
`simulate_multiverse_data()` before touching real data.

## Verify

```bash
Rscript -e 'pkgload::load_all("."); testthat::test_dir("tests/testthat")'
Rscript -e 'roxygen2::roxygenise(".")'
R CMD build . && R CMD check --as-cran metaMultiverse_*.tar.gz
```

## Git

Work on a branch, never on `main`. Commit at logical checkpoints. Push the
branch and open **one** PR against `main`; never stack PRs on each other
(a stacked PR was once merged into its base branch instead of `main`). The
maintainer merges, tags and publishes releases himself, because a release
mints a Zenodo DOI. Do not write into `../../metaMultiverse-dev` unless
explicitly asked to. Ask before any change that alters the statistical
meaning of results.
