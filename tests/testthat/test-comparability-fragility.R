# Phase 4: cross-domain comparability layer + M1-M6 fragility metrics.
# Two simulated "domains" with overlapping fork structure.

warrant4 <- function(txt = "prototype warrant") {
  list(basis = "convention-unresolved", text = txt)
}

make_domain <- function(name, n_studies, seed, bias = 0) {
  dat <- simulate_multiverse_data(n_studies = n_studies, es_per_study = 1:2,
                                  seed = seed)
  # optional small-study effect so bias-sensitive estimators diverge
  if (bias > 0) dat$yi <- dat$yi + bias * sqrt(dat$vi) * 2
  cfg <- domain_config(
    domain = name,
    forks = list(
      fork("comparator", "data", "changes_question", "clinical", warrant4(),
           column = "comparator",
           levels = list(wl = "wl", cau = "cau",
                         all_controls = c("wl", "cau", "other")),
           reference = "all_controls"),
      fork("rob_exclusion", "data", "uncertain", "analytic", warrant4(),
           column = "rob",
           levels = list(keep_all = c(0, 1), low_rob_only = 0),
           reference = "keep_all"),
      fork("ma_method", "analysis", "uncertain", "analytic", warrant4(),
           levels = c("reml", "pm", "uwls"), reference = "reml"),
      fork("dependency", "analysis", "uncertain", "analytic", warrant4(),
           levels = c("aggregate", "select_max"), reference = "aggregate")
    ),
    estimand = list(population = "sim", intervention = "tx",
                    outcome = "y", timepoint = "post")
  )
  grid <- build_specification_grid(cfg, dat)
  run_multiverse_grid(grid, progress = FALSE)
}

res_a <- make_domain("domain_a", n_studies = 40, seed = 21, bias = 1.5)
res_b <- make_domain("domain_b", n_studies = 34, seed = 22)
results <- list(domain_a = res_a, domain_b = res_b)
cells <- c(domain_a = "comparator=cau", domain_b = "comparator=cau")

test_that("fork availability matrix spans domains, forks, and levels", {
  fam <- fork_availability_matrix(results)
  expect_true(all(c("domain", "fork", "level", "status") %in% names(fam)))
  expect_setequal(unique(fam$domain), c("domain_a", "domain_b"))
  expect_true(all(c("comparator", "rob_exclusion", "ma_method",
                    "dependency") %in% unique(fam$fork)))
  fs <- attr(fam, "fork_status")
  expect_true(all(fs$status %in%
                    c("varies", "cannot_vary", "too_few_studies", "not_in_data")))
})

test_that("attrition report combines domains with stage counts", {
  ar <- attrition_report(results)
  expect_setequal(unique(ar$domain), c("domain_a", "domain_b"))
  expect_true(all(c("planned", "converged") %in% ar$stage))
})

test_that("common core is the intersection of surviving analytic combos", {
  core <- common_core_specs(results, cells)
  expect_true(all(c("rob_exclusion", "ma_method", "dependency") %in%
                    core$shared_forks))
  expect_false("comparator" %in% core$shared_forks) # N fork: fixed by cells
  expect_true(nrow(core$common_combos) >= 1)
  for (dom in names(results)) {
    sub <- core$subsets[[dom]]
    expect_true(all(sub$cell_id == cells[[dom]]))
  }
  # every common combo appears in both subsets
  key <- function(df) apply(df[, core$shared_forks, drop = FALSE], 1,
                            paste, collapse = "|")
  common_keys <- apply(core$common_combos, 1, paste, collapse = "|")
  expect_true(all(common_keys %in% key(core$subsets$domain_a)))
  expect_true(all(common_keys %in% key(core$subsets$domain_b)))
})

test_that("M1 reports per-fork marginal impact in g units, N forks excluded", {
  m1 <- m1_fork_impact(res_a, "comparator=cau")
  expect_false("comparator" %in% m1$fork)
  expect_true(all(m1$sigma_g >= 0, na.rm = TRUE))
  expect_true("ma_method" %in% m1$fork)
})

test_that("M2 spec-I2 lies in [0,1] and uses reported SEs", {
  m2 <- m2_spec_i2(res_a, "comparator=cau")
  expect_true(m2$spec_i2 >= 0 && m2$spec_i2 <= 1)
  expect_true(m2$range_in_se >= 0)
  expect_true(m2$n_specs > 0)
})

test_that("M3/M4 quantify MID exceedance flat vs fork-balanced", {
  m3 <- m3_mid_consistency(res_a, "comparator=cau", mid = 0.24)
  expect_true(m3$prop_above_mid >= 0 && m3$prop_above_mid <= 1)
  m4 <- m4_fork_balanced(res_a, "comparator=cau")
  expect_equal(m4$delta, m4$fork_balanced - m4$flat)
})

test_that("M5 localizes fragility: fixing the driving fork lowers spec-I2", {
  m5 <- m5_lofo(res_a, "comparator=cau")
  expect_equal(m5$fixed_fork[1], "none")
  expect_true(all(m5$spec_i2 >= 0 & m5$spec_i2 <= 1, na.rm = TRUE))
  # localization property: fixing the divergence-driving fork (whichever it
  # is) collapses spec-I2 well below the full grid
  none <- m5$spec_i2[m5$fixed_fork == "none"]
  best_drop <- min(m5$spec_i2[m5$fixed_fork != "none"], na.rm = TRUE)
  expect_true(best_drop < none)
})

test_that("M6 compares full vs common-subgrid spec-I2 per domain", {
  m6 <- m6_common_subgrid(results, cells)
  expect_setequal(m6$domain, c("domain_a", "domain_b"))
  expect_true(all(m6$n_common <= m6$n_full))
  expect_true(all(m6$spec_i2_common >= 0 & m6$spec_i2_common <= 1,
                  na.rm = TRUE))
})

test_that("fragility_metrics wrapper returns all five within-cell metrics", {
  fm <- fragility_metrics(res_b, "comparator=cau")
  expect_named(fm, c("m1", "m2", "m3", "m4", "m5"))
})

test_that("cell restriction is enforced: unknown cell errors", {
  expect_error(m2_spec_i2(res_a, "comparator=nonexistent"), "No converged")
})

test_that("E forks exceeding tolerance are escalated for scrutiny", {
  dat <- simulate_multiverse_data(n_studies = 40, seed = 31)
  # make delivery format genuinely consequential despite an E label
  dat$yi <- dat$yi + ifelse(dat$format == "grp", 0.6, 0)
  cfg <- domain_config(
    domain = "escalation",
    forks = list(
      fork("delivery_format", "data", "equivalent", "clinical", warrant4(),
           column = "format",
           levels = list(individual = "ind", group = "grp",
                         any_format = c("ind", "grp")),
           reference = "any_format", equivalence_margin = 0.05),
      fork("ma_method", "analysis", "uncertain", "analytic", warrant4(),
           levels = c("reml", "pm")),
      fork("dependency", "analysis", "uncertain", "analytic", warrant4(),
           levels = c("aggregate", "select_max"))
    ),
    estimand = list(population = "sim", intervention = "tx",
                    outcome = "y", timepoint = "post")
  )
  r <- run_multiverse_grid(build_specification_grid(cfg, dat),
                           progress = FALSE)
  chk <- check_fork_tolerances(r, "main")
  row <- chk[chk$fork == "delivery_format", ]
  expect_true(row$exceeds_margin)
  expect_true(row$escalate)
})
