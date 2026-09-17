# metaMultiverse — project instructions

R package for multiverse meta-analysis. Public repo `cyplessen/metaMultiverse`.
This is the **live** line, version 0.3.0. A second line,
`metaMultiverse-dev` (version 0.9.0), sits at `../../metaMultiverse-dev`
and holds an estimand-aware architecture built on top of v0.2.3. The two
diverged at commit `3b9cc47` and neither has seen the other's work.

If you are here to merge them, read `MERGE_BRIEF.md` first and follow it
stage by stage. Do not improvise a merge.

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

Work on a branch, never on `main`. Commit at logical checkpoints. Do not
push, and do not write anything into `../../metaMultiverse-dev`. Ask
before any change that alters the statistical meaning of results.
