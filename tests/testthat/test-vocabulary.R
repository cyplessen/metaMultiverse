# Plain-language vocabulary: canonical names, letter/legacy aliases, and
# the guarantee that old and new spellings produce identical objects.

w <- function() list(basis = "convention-unresolved", text = "test warrant")

test_that("decision letters are aliases for the plain rulings", {
  f_new <- fork("comparator", type = "data", decision = "changes_question",
                family = "clinical", column = "comparator",
                levels = list(wl = "wl", cau = "cau"), justification = w())
  f_old <- fork("comparator", type = "which", decision = "N",
                family = "clinical", column = "comparator",
                levels = list(wl = "wl", cau = "cau"), justification = w())
  expect_identical(f_new, f_old)
  expect_equal(f_new$decision, "changes_question")
  expect_equal(f_new$type, "data")

  expect_equal(fork("f", "how", "E", "analytic", w(),
                    levels = c("a", "b"))$decision, "equivalent")
  expect_equal(fork("f", "how", "U", "analytic", w(),
                    levels = c("a", "b"))$decision, "uncertain")
  expect_equal(fork("f", "how", "X", "analytic", w(),
                    levels = c("a", "b"))$decision, "indefensible")
})

test_that("unknown decisions and types error with the full menu", {
  expect_error(fork("f", "data", "banana", "analytic", w(),
                    column = "x", levels = list(a = "a", b = "b")),
               "equivalent, uncertain, changes_question, indefensible")
  expect_error(fork("f", "sideways", "uncertain", "analytic", w(),
                    column = "x", levels = list(a = "a", b = "b")),
               "data, analysis")
})

test_that("tolerance is accepted as an alias for equivalence_margin", {
  f_new <- fork("f", "data", "equivalent", "analytic", w(), column = "x",
                levels = list(a = "a", b = "b"), equivalence_margin = 0.2)
  f_old <- fork("f", "data", "E", "analytic", w(), column = "x",
                levels = list(a = "a", b = "b"), tolerance = 0.2)
  expect_identical(f_new$equivalence_margin, f_old$equivalence_margin)
  expect_equal(f_new$equivalence_margin, 0.2)
})

test_that("min_k / min_k_sensitivity are aliases for min_studies(_strict)", {
  forks <- list(
    fork("ma_method", "analysis", "uncertain", "analytic", w(),
         levels = c("reml", "pm")),
    fork("dependency", "analysis", "uncertain", "analytic", w(),
         levels = c("aggregate", "modeled"))
  )
  est <- list(population = "p", intervention = "i", outcome = "o",
              timepoint = "t")
  cfg_new <- domain_config("d", forks, est, min_studies = 7,
                           min_studies_strict = 12)
  cfg_old <- domain_config("d", forks, est, min_k = 7,
                           min_k_sensitivity = 12)
  expect_equal(cfg_new$min_studies, 7)
  expect_equal(cfg_old$min_studies, 7)
  expect_equal(cfg_old$min_studies_strict, 12)
})

test_that("legacy plausibility level names are accepted and normalized", {
  pf_new <- plausibility_fork(include_levels = c("keep_all",
                                                 "drop_triple_flagged"))
  pf_old <- plausibility_fork(reference_level = "all_studies",
                              include_levels = c("all_studies",
                                                 "exclude_all_flagged"))
  sim <- simulate_multiverse_data(n_studies = 12, seed = 4)
  dat <- add_plausibility_flags(sim, high_rob = sim$rob == 1,
                                reference = "all")
  expect_identical(names(pf_new$adaptive(dat)), names(pf_old$adaptive(dat)))
  expect_equal(pf_old$reference, "keep_all")
  expect_error(plausibility_fork(include_levels = "banana"),
               "plausibility level")
})

test_that("old and new spellings build identical grids and spec ids", {
  sim <- simulate_multiverse_data(n_studies = 30, seed = 17)
  make <- function(type_c, dec_n, dec_u, min_arg) {
    forks <- list(
      fork("comparator", type_c, dec_n, "clinical", w(),
           column = "comparator",
           levels = list(wl = "wl", cau = "cau",
                         all_controls = c("wl", "cau", "other")),
           reference = "all_controls"),
      fork("ma_method", if (type_c == "which") "how" else "analysis",
           dec_u, "analytic", w(), levels = c("reml", "pm")),
      fork("dependency", if (type_c == "which") "how" else "analysis",
           dec_u, "analytic", w(), levels = c("aggregate", "modeled"))
    )
    est <- list(population = "p", intervention = "i", outcome = "o",
                timepoint = "t")
    cfg <- do.call(domain_config,
                   c(list("d", forks, est), min_arg))
    build_specification_grid(cfg, sim)
  }
  g_new <- make("data", "changes_question", "uncertain",
                list(min_studies = 5))
  g_old <- make("which", "N", "U", list(min_k = 5))
  expect_identical(g_new$spec_id, g_old$spec_id)
  expect_identical(attr(g_new, "decision_map"), attr(g_old, "decision_map"))
})

test_that("rho remains an alias in run_multiverse_grid", {
  sim <- simulate_multiverse_data(n_studies = 20, es_per_study = 1:2,
                                  seed = 9)
  forks <- list(
    fork("ma_method", "analysis", "uncertain", "analytic", w(),
         levels = c("reml", "pm")),
    fork("dependency", "analysis", "uncertain", "analytic", w(),
         levels = c("aggregate", "select_max"))
  )
  cfg <- domain_config("d", forks,
                       list(population = "p", intervention = "i",
                            outcome = "o", timepoint = "t"))
  g <- build_specification_grid(cfg, sim)
  r_new <- run_multiverse_grid(g, within_study_correlation = 0.3,
                               progress = FALSE)
  r_old <- run_multiverse_grid(g, rho = 0.3, progress = FALSE)
  expect_identical(r_new$results$b, r_old$results$b)
  expect_identical(r_new$results$se, r_old$results$se)
})

test_that("the decision crosswalk is pinned", {
  cw <- decision_crosswalk()
  expect_equal(cw$ruling,
               c("equivalent", "uncertain", "changes_question",
                 "indefensible", "settled"))
  expect_equal(cw$alias[1:4], c("E", "U", "N", "X"))
  expect_true(all(nzchar(cw$handling)))
  expect_true(all(nzchar(cw$lineage)))
})

test_that("availability statuses use the plain names", {
  sim <- simulate_multiverse_data(n_studies = 30, seed = 23)
  cfg <- domain_config(
    "d",
    list(fork("comparator", "data", "changes_question", "clinical", w(),
              column = "comparator",
              levels = list(wl = "wl", cau = "cau",
                            all_controls = c("wl", "cau", "other"))),
         fork("ma_method", "analysis", "uncertain", "analytic", w(),
              levels = c("reml", "pm")),
         fork("dependency", "analysis", "uncertain", "analytic", w(),
              levels = c("aggregate", "modeled"))),
    list(population = "p", intervention = "i", outcome = "o",
         timepoint = "t")
  )
  v <- validate_domain_config(cfg, sim)
  expect_true(all(v$availability$status %in%
                    c("viable", "too_few_studies", "not_in_data", "varies") |
                    startsWith(v$availability$status, "same_studies_as_")))
  expect_true(all(v$fork_status$status %in%
                    c("varies", "cannot_vary", "too_few_studies",
                      "not_in_data")))
})
