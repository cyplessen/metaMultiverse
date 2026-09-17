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

test_that("warrant table covers forks, eligibility, and fixed decisions", {
  wt <- warrant_table(cfg5)
  expect_true(all(c("comparator", "model_type") %in% wt$name))
  expect_true("effect_metric" %in% wt$name)
  expect_equal(wt$decision[wt$name == "effect_metric"], "fixed")
  expect_true(all(nzchar(wt$justification)))
})

test_that("grid report implements the anti-probabilistic defaults", {
  rep_lines <- report_grid_result(res5)
  txt <- paste(rep_lines, collapse = "\n")
  expect_match(txt, "Purpose declaration")
  expect_match(txt, "justified, converged specifications") # headline count
  expect_match(txt, "NOT inferential")                     # labeled descriptives
  expect_match(txt, "per estimand cell")
  expect_match(txt, "Worst-universe audit")
  expect_match(txt, "Warrant table")
  expect_match(txt, "Bias-decomposition layer")
  expect_match(txt, "Ranked fork drivers")
  expect_match(txt, "analytic sensitivity")                # caveat present
  # raw combinatorial size must not headline: converged count is stated
  # before planned count
  expect_true(regexpr("justified, converged", txt) <
                regexpr("planned before viability", txt))
})

test_that("prereg export writes a time-stamped Quarto document", {
  f <- tempfile(fileext = ".qmd")
  export_prereg_config(cfg5, f)
  txt <- readLines(f)
  expect_match(txt[1], "^---$")
  expect_true(any(grepl("Preregistered multiverse configuration", txt)))
  expect_true(any(grepl("Estimand declaration", txt)))
  expect_true(any(grepl("before any pipeline contact", txt)))
  expect_true(any(grepl("min_studies", txt)))
  unlink(f)
})

test_that("two-layer plot and grid spec curve build without error", {
  p1 <- plot_two_layer(res5, "comparator=cau")
  expect_s3_class(p1, "ggplot")
  p2 <- plot_grid_spec_curve(res5)
  expect_s3_class(p2, "ggplot")
})
