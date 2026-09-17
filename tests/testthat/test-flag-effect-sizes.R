# Phase 3: effect-size plausibility flagging (Harrer et al. 2025) and the
# plausibility handling-strategy fork. Threshold values are pinned against
# the published paper.

test_that("reference thresholds reproduce the published cutoffs", {
  # Harrer et al. (2025): overall g > 1.87; depression 1.66; psychosis 1.24;
  # PTSD 2.88 (S-value 4.32 bits = upper-tail p .05 of the fitted Gamma)
  crit <- function(ref) {
    fl <- flag_effect_sizes(smd = 0.3, se = 0.1, high_rob = FALSE,
                            reference = ref)
    fl$thresholds$smd_crit
  }
  expect_equal(round(crit("all"), 2), 1.87)
  expect_equal(round(crit("dep"), 2), 1.66)
  expect_equal(round(crit("psy"), 2), 1.24)
  expect_equal(round(crit("ptsd"), 2), 2.88)
})

test_that("power thresholds use the reference first quartile by default", {
  fl <- flag_effect_sizes(smd = 0.3, se = 0.1, high_rob = FALSE,
                          reference = "all")
  expect_equal(round(fl$thresholds$power, 3), 0.324) # Q1 = 32.4% overall
  fl_dep <- flag_effect_sizes(smd = 0.3, se = 0.1, high_rob = FALSE,
                              reference = "dep")
  expect_equal(round(fl_dep$thresholds$power, 2), 0.41)
  expect_equal(round(fl$thresholds$s_value_bits, 2), 4.32)
})

test_that("gad reference is a legitimate explicit choice (old validator bug)", {
  expect_no_error(
    flag_effect_sizes(smd = 0.3, se = 0.1, high_rob = FALSE,
                      reference = "gad")
  )
})

test_that("reference has no silent default", {
  expect_error(flag_effect_sizes(smd = 0.3, se = 0.1, high_rob = FALSE),
               "explicit")
})

test_that("operative flag is the conjunction: never magnitude alone", {
  # huge effect, well-powered, low RoB -> extreme indicator only, NOT flagged
  fl <- flag_effect_sizes(smd = c(3.5, 3.5, 0.3),
                          se = c(0.05, 0.9, 0.9),
                          high_rob = c(FALSE, TRUE, TRUE),
                          reference = "all")
  expect_equal(fl$flag_effect, c(TRUE, TRUE, FALSE))
  expect_equal(fl$flagged, c(FALSE, TRUE, FALSE)) # only all-three fires
  expect_equal(fl$score, c(1L, 3L, 2L))
  expect_equal(fl$flag_profile, c("100", "111", "011"))
})

test_that("missing RoB ratings are treated as high risk with a message (D2)", {
  expect_message(
    fl <- flag_effect_sizes(smd = c(0.3, 0.3), se = c(0.1, 0.1),
                            high_rob = c(NA, FALSE), reference = "all"),
    "HIGH"
  )
  expect_equal(fl$flag_rob, c(TRUE, FALSE))
})

test_that("add_plausibility_flags appends aligned columns", {
  sim <- simulate_multiverse_data(n_studies = 15, seed = 3)
  out <- add_plausibility_flags(sim, high_rob = sim$rob == 1,
                                reference = "all")
  expect_true(all(c("flag_effect", "flag_power", "flag_rob", "flag_score",
                    "flag_profile", "flagged") %in% names(out)))
  expect_equal(nrow(out), nrow(sim))
  expect_true(all(nchar(out$flag_profile) == 3))
  expect_equal(attr(out, "flag_thresholds")$reference, "all")
})

test_that("plausibility menu has the seven documented strategies", {
  menu <- metaMultiverse:::.plausibility_menu()
  expect_equal(names(menu),
               c("keep_all", "drop_triple_flagged", "drop_two_plus_flagged",
                 "drop_any_flagged", "drop_extreme_effects",
                 "drop_underpowered", "drop_high_rob"))
  # score-3 exclusion retains all but "111"
  expect_false("111" %in% menu$drop_triple_flagged)
  expect_equal(length(menu$drop_triple_flagged), 7)
  # loosest tier keeps only "000"
  expect_equal(menu$drop_any_flagged, "000")
})

test_that("plausibility fork integrates with configs, grids, and the runner", {
  sim <- simulate_multiverse_data(n_studies = 40, es_per_study = 1, seed = 5)
  # inject implausible pattern: some large, imprecise, high-RoB effects
  sim$yi[1:6] <- 3.2
  sim$vi[1:6] <- 0.35
  sim$rob[1:6] <- 1
  dat <- add_plausibility_flags(sim, high_rob = sim$rob == 1,
                                reference = "all")

  cfg <- domain_config(
    domain = "simulated_flags",
    forks = list(
      plausibility_fork(),
      fork("ma_method", "analysis", "uncertain", "analytic", "prototype",
           levels = c("reml", "pm")),
      fork("dependency", "analysis", "uncertain", "analytic", "prototype",
           levels = c("aggregate", "select_max"))
    ),
    estimand = list(population = "sim", intervention = "tx",
                    outcome = "y", timepoint = "post"),
    flag_reference = "all"
  )
  v <- validate_domain_config(cfg, dat)
  av <- v$availability[v$availability$fork == "plausibility", ]
  expect_equal(nrow(av), 7) # full menu present in the availability matrix
  # empty or duplicate levels are recorded, not dropped
  expect_true(all(av$status %in%
                    c("viable", "too_few_studies", "not_in_data") |
                    startsWith(av$status, "same_studies_as_")))

  g <- build_specification_grid(cfg, dat)
  r <- run_multiverse_grid(g, progress = FALSE)
  df <- r$results
  expect_true(all(c("plausibility", "ma_method") %in% names(df)))

  # excluding flagged studies must reduce the pooled estimate here
  med_all <- stats::median(df$b[df$plausibility == "keep_all"])
  med_excl <- stats::median(df$b[df$plausibility == "drop_triple_flagged"])
  expect_true(med_excl < med_all)
})

test_that("plausibility and RoB stay separate fork families", {
  pf <- plausibility_fork()
  expect_equal(pf$name, "plausibility")
  expect_equal(pf$family, "analytic")
  expect_equal(pf$decision, "uncertain")
  # a standalone RoB fork can coexist under its own name
  sim <- simulate_multiverse_data(n_studies = 30, seed = 9)
  dat <- add_plausibility_flags(sim, high_rob = sim$rob == 1,
                                reference = "all")
  cfg <- domain_config(
    domain = "simulated_two_families",
    forks = list(
      plausibility_fork(include_levels = c("keep_all",
                                           "drop_triple_flagged")),
      fork("rob_exclusion", "data", "uncertain", "analytic",
           "separate quality-signal family", column = "rob",
           levels = list(keep_all = c(0, 1), low_rob_only = 0)),
      fork("ma_method", "analysis", "uncertain", "analytic", "prototype",
           levels = c("reml", "pm")),
      fork("dependency", "analysis", "uncertain", "analytic", "prototype",
           levels = c("aggregate", "modeled"))
    ),
    estimand = list(population = "sim", intervention = "tx",
                    outcome = "y", timepoint = "post")
  )
  v <- validate_domain_config(cfg, dat)
  expect_true(all(c("plausibility", "rob_exclusion") %in%
                    v$fork_status$fork))
})

test_that("empty adaptive levels are availability entries, not errors (phobia case)", {
  sim <- simulate_multiverse_data(n_studies = 30, seed = 13)
  # every study gets at least one flag: profile "000" never occurs, so
  # drop_any_flagged (retains only "000") is empty -- must not error
  dat <- add_plausibility_flags(sim, high_rob = rep(TRUE, nrow(sim)),
                                reference = "all")
  expect_false("000" %in% dat$flag_profile)
  cfg <- domain_config(
    domain = "all_flagged",
    forks = list(
      plausibility_fork(),
      fork("ma_method", "analysis", "uncertain", "analytic", "prototype",
           levels = c("reml", "pm")),
      fork("dependency", "analysis", "uncertain", "analytic", "prototype",
           levels = c("aggregate", "modeled"))
    ),
    estimand = list(population = "sim", intervention = "tx",
                    outcome = "y", timepoint = "post")
  )
  expect_no_error(v <- validate_domain_config(cfg, dat, strict = TRUE))
  av <- v$availability
  expect_equal(av$status[av$level == "drop_any_flagged"], "not_in_data")
})

test_that("restricted menus keep the reference level", {
  pf <- plausibility_fork(include_levels = "drop_triple_flagged")
  sim <- simulate_multiverse_data(n_studies = 10, seed = 2)
  dat <- add_plausibility_flags(sim, high_rob = sim$rob == 1,
                                reference = "all")
  lv <- pf$adaptive(dat)
  expect_true("keep_all" %in% names(lv))
})
