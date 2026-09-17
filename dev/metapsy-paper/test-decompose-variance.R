# Tests for dev/metapsy-paper/decompose_variance.R. Not run by the package
# suite: the decomposition is developed with the Metapsy multiverse paper.
# Run with the package loaded and decompose_variance.R sourced.

# Phase 5: hierarchical variance decomposition, warrant/fixed-decision
# tables, grid report with anti-probabilistic defaults, prereg export,
# two-layer plot.

warrant5 <- function(txt = "prototype warrant") {
  list(basis = "convention-unresolved", text = txt)
}

sim5 <- simulate_multiverse_data(n_studies = 40, es_per_study = 1:2,
                                 seed = 41)
cfg5 <- domain_config(
  domain = "simulated_report",
  forks = list(
    fork("comparator", "data", "changes_question", "clinical", warrant5(),
         column = "comparator",
         levels = list(wl = "wl", cau = "cau",
                       all_controls = c("wl", "cau", "other")),
         reference = "all_controls"),
    fork("rob_exclusion", "data", "uncertain", "analytic", warrant5(),
         column = "rob",
         levels = list(keep_all = c(0, 1), low_rob_only = 0)),
    fork("ma_method", "analysis", "uncertain", "analytic", warrant5(),
         levels = c("reml", "pm")),
    fork("dependency", "analysis", "uncertain", "analytic", warrant5(),
         levels = c("aggregate", "modeled")),
    fork("model_type", "analysis", "indefensible", "analytic",
         list(basis = "citation", text = "bias layer",
              citation = "Borenstein et al. 2010"),
         levels = c("random_effects", "fixed_effect"),
         reference = "random_effects",
         overrides = list(fixed_effect = list(ma_method = "fe")))
  ),
  estimand = list(population = "sim", intervention = "tx",
                  outcome = "y", timepoint = "post"),
  fixed_decisions = list(
    effect_metric = list(
      value = "Hedges' g",
      justification = list(basis = "citation",
                           text = "standard for continuous outcomes",
                           citation = "Hedges 1981"))
  )
)
res5 <- run_multiverse_grid(build_specification_grid(cfg5, sim5),
                            progress = FALSE)

test_that("decompose_variance returns order-independent labeled components", {
  dec <- decompose_variance(res5)
  expect_true(all(c("term", "family", "layer", "variance", "share",
                    "mom_share") %in% names(dec)))
  expect_equal(dec$layer[dec$term == "comparator"], "between_estimand")
  expect_true(all(dec$layer[dec$term %in%
                              c("ma_method", "dependency",
                                "rob_exclusion")] == "within_question"))
  expect_true(abs(sum(dec$share) - 1) < 1e-8)
  expect_false("model_type" %in% dec$term) # X forks never enter
  expect_match(attr(dec, "caveat"), "analytic sensitivity")
})

test_that("decomposition works within a single cell (no N components)", {
  dec <- decompose_variance(res5, cell = "comparator=cau")
  expect_false("comparator" %in% dec$term)
  expect_true(all(dec$layer[dec$term != "residual"] == "within_question"))
})

test_that("family shares aggregate clinical vs analytic vs between-estimand", {
  fs <- family_shares(decompose_variance(res5))
  expect_true("between_estimand/clinical" %in%
                paste(fs$layer, fs$family, sep = "/"))
  expect_true(abs(sum(fs$share) - 1) < 1e-8)
})

test_that("marginal fork means report raw and common-subgrid-balanced values", {
  mm <- marginal_fork_means(res5, "ma_method", cell = "comparator=cau")
  expect_setequal(mm$level, c("reml", "pm"))
  expect_true(all(!is.na(mm$balanced_mean)))
  expect_true(all(mm$n_common_combos >= 1))
})

test_that("decompose_variance records its estimator and announces the fallback", {
  skip_if_not_installed("lme4")
  dec <- decompose_variance(res5)
  expect_equal(attr(dec, "estimator"), "lmer")
  expect_no_message(decompose_variance(res5))

  # lme4 missing: method-of-moments components, and never silently
  local_mocked_bindings(.has_lme4 = function() FALSE)
  expect_message(dec_mom <- decompose_variance(res5), "lme4.*not installed")
  expect_equal(attr(dec_mom, "estimator"), "method_of_moments")
  expect_null(attr(dec_mom, "model"))
  expect_true(abs(sum(dec_mom$share) - 1) < 1e-8)
  # with the fallback, share and the method-of-moments companion coincide
  expect_equal(dec_mom$share, dec_mom$mom_share)
})
