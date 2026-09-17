# Phase 5: warrant/fixed-decision
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
