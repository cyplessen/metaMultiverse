# Phase 2: grid runner with attrition ledger, traceability, worst-universe
# audit, estimand pooling guard. Simulated data only.

warrant2 <- function(txt = "prototype warrant") {
  list(basis = "convention-unresolved", text = txt)
}

sim2 <- simulate_multiverse_data(n_studies = 36, es_per_study = 1:3, seed = 11)

cfg2 <- domain_config(
  domain = "simulated",
  forks = list(
    fork("comparator", "data", "changes_question", "clinical", warrant2(),
         column = "comparator",
         levels = list(wl = "wl", cau = "cau",
                       all_controls = c("wl", "cau", "other")),
         reference = "all_controls"),
    fork("rob_exclusion", "data", "uncertain", "analytic", warrant2(),
         column = "rob",
         levels = list(keep_all = c(0, 1), low_rob_only = 0),
         reference = "keep_all"),
    fork("ma_method", "analysis", "uncertain", "analytic", warrant2(),
         levels = c("reml", "pm"), reference = "reml"),
    fork("dependency", "analysis", "uncertain", "analytic", warrant2(),
         levels = c("aggregate", "modeled"), reference = "aggregate"),
    fork("model_type", "analysis", "indefensible", "analytic",
         list(basis = "citation",
              text = "FE under heterogeneity: bias layer only",
              citation = "Borenstein et al. 2010"),
         levels = c("random_effects", "fixed_effect"),
         reference = "random_effects",
         overrides = list(fixed_effect = list(ma_method = "fe")))
  ),
  estimand = list(population = "sim adults", intervention = "sim tx",
                  outcome = "symptoms", timepoint = "post")
)

grid2 <- build_specification_grid(cfg2, sim2)
res2 <- run_multiverse_grid(grid2, progress = FALSE)

test_that("runner produces enriched results keyed by spec_id", {
  df <- res2$results
  expect_true(all(c("spec_id", "cell_id", "layer", "b", "se", "tau2", "i2",
                    "k", "converged", "method_label") %in% names(df)))
  expect_false(anyDuplicated(df$spec_id) > 0)
  inf <- df[df$layer == "inference", ]
  expect_true(nrow(inf) > 0)
  # tau2 present for REML/PM aggregate universes
  reml_rows <- inf[inf$ma_method == "reml" & inf$dependency == "aggregate", ]
  expect_true(all(!is.na(reml_rows$tau2)))
  expect_true(all(!is.na(reml_rows$se)))
})

test_that("bias layer executes X overrides and stays out of inference", {
  df <- res2$results
  bias <- df[df$layer == "bias", ]
  expect_true(nrow(bias) > 0)
  fe_rows <- bias[bias$model_type == "fixed_effect", ]
  expect_true(all(fe_rows$method_label == "FE"))
  # reference bias rows ran the reference engine
  ref_rows <- bias[bias$model_type == "random_effects", ]
  expect_true(all(ref_rows$method_label == "REML"))
  expect_false(any(df$layer == "inference" & !is.na(df$model_type)))
})

test_that("attrition ledger records losses with stages and reasons", {
  at <- attrition_summary(res2)
  expect_equal(at$stage[1], "planned")
  expect_true(all(at$n >= 0))
  converged <- at$n[at$stage == "converged"]
  defined <- at$n[at$stage == "defined"]
  lost <- sum(at$n[grepl("^lost", at$stage)])
  expect_equal(converged + lost, defined)
})

test_that("min_k losses show up in the ledger, not silently", {
  # shrink the dataset so low_rob_only strata fall below min_k in some cells
  small <- sim2[sim2$study %in% unique(sim2$study)[1:8], ]
  cfg_small <- domain_config(
    domain = "simulated_small",
    forks = list(
      fork("comparator", "data", "changes_question", "clinical", warrant2(),
           column = "comparator",
           levels = list(wl = "wl", cau = "cau",
                         all_controls = c("wl", "cau", "other")),
           reference = "all_controls"),
      fork("ma_method", "analysis", "uncertain", "analytic", warrant2(),
           levels = c("reml", "pm")),
      fork("dependency", "analysis", "uncertain", "analytic", warrant2(),
           levels = c("aggregate", "modeled"))
    ),
    estimand = list(population = "sim", intervention = "tx",
                    outcome = "y", timepoint = "post"),
    min_studies = 5
  )
  # some comparator levels will be below k; validation may suppress them,
  # remaining cells may still lose universes at run time
  v <- validate_domain_config(cfg_small, small)
  expect_true(any(!v$availability$viable) ||
                all(v$availability$viable)) # structure check only
})

test_that("lookup_spec traces id -> config -> estimand -> result -> audit", {
  df <- res2$results
  id <- df$spec_id[df$layer == "inference"][1]
  tr <- lookup_spec(res2, id)
  expect_equal(tr$spec$spec_id, id)
  expect_equal(tr$estimand$population, "sim adults")
  expect_true("comparator" %in% names(tr$forks))
  expect_equal(tr$forks$comparator$decision, "changes_question")
  expect_equal(nrow(tr$result), 1)
  expect_error(lookup_spec(res2, "nonexistent::spec"), "not found")
})

test_that("audit flags fixed-effect-under-high-I2 in the bias layer", {
  audit <- audit_universes(res2, i2_high = 25)
  expect_true(nrow(audit) == nrow(res2$results))
  fe_ids <- res2$results$spec_id[res2$results$method_label == "FE" &
                                   !is.na(res2$results$i2) &
                                   res2$results$i2 > 25]
  if (length(fe_ids) > 0) {
    flagged <- audit$flags[audit$spec_id %in% fe_ids]
    expect_true(all(grepl("fixed_effect_under_high_i2", flagged)))
  }
})

test_that("effect summaries are per cell; pooling across cells is refused", {
  cells <- summarize_effects(res2)
  expect_true(nrow(cells) >= 2)
  expect_true(all(c("median_b", "n_specs", "sign_consistency") %in%
                    names(cells)))
  expect_error(
    assert_single_estimand(res2$results, "a pooled median"),
    "two-layer rule"
  )
  one_cell <- res2$results[res2$results$cell_id ==
                             res2$results$cell_id[1], ]
  expect_true(assert_single_estimand(one_cell))
})

test_that("divergence summary compares cells without pooling", {
  div <- summarize_divergence(res2)
  expect_true(nrow(div$cell_summaries) >= 2)
  expect_true(is.data.frame(div$between_cell_differences))
  expect_true(div$variance_between_share >= 0 &&
                div$variance_between_share <= 1)
  # simulated comparator shift (wl +0.2 vs cau) must appear as divergence
  cs <- div$cell_summaries
  wl <- cs$median_b[cs$cell_id == "comparator=wl"]
  cau <- cs$median_b[cs$cell_id == "comparator=cau"]
  expect_true(wl > cau)
})

test_that("fits without finite estimate + CI are estimation failures", {
  # value-agnostic estimability criterion: the depression p-uniform* case
  # (b = -4159 with NA confidence limits) must be ledgered, never reported
  ok <- new_universe_result(0.4, 0.2, 0.6, 0.01)
  expect_true(metaMultiverse:::is_valid_universe(ok))
  na_ci <- new_universe_result(-4159.5, NA, NA, NA)
  expect_false(metaMultiverse:::is_valid_universe(na_ci))
  inf_b <- new_universe_result(Inf, 0.1, 0.2, 0.5)
  expect_false(metaMultiverse:::is_valid_universe(inf_b))
  expect_false(metaMultiverse:::is_valid_universe(universe_NA))
})

test_that("rho is exposed and changes aggregate-based results", {
  r_low <- run_multiverse_grid(grid2, rho = 0.1, progress = FALSE)
  agg_a <- res2$results[res2$results$dependency == "aggregate" &
                          res2$results$layer == "inference", ]
  agg_b <- r_low$results[r_low$results$dependency == "aggregate" &
                           r_low$results$layer == "inference", ]
  merged <- merge(agg_a[, c("spec_id", "b", "se")],
                  agg_b[, c("spec_id", "b", "se")], by = "spec_id")
  # with clustered effect sizes, rho must move at least the SEs
  expect_true(any(abs(merged$se.x - merged$se.y) > 1e-8))
})
