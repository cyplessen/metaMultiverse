# ABOUTME: Every registered estimator must fill the enriched universe_result
# ABOUTME: (se, tau2, i2, k, convergence) correctly, not just b/ci/pval.
# ABOUTME: Values are checked against metafor / meta called directly.

ef_single <- function() {
  data.frame(
    study = paste0("Study_", 1:10),
    es_id = 1:10,
    yi = c(0.3, 0.5, 0.7, -0.2, 0.6, 0.4, 0.8, 0.1, -0.3, 0.9),
    vi = c(0.02, 0.03, 0.01, 0.04, 0.02, 0.03, 0.01, 0.05, 0.02, 0.01),
    stringsAsFactors = FALSE
  )
}

# two effect sizes per study, for the methods that model the dependency
ef_nested <- function() {
  data.frame(
    study = rep(paste0("Study_", 1:8), each = 2),
    es_id = 1:16,
    yi = c(0.31, 0.42, 0.55, 0.47, 0.72, 0.60, -0.15, 0.02, 0.61, 0.70,
           0.38, 0.29, 0.83, 0.75, 0.12, 0.20),
    vi = rep(c(0.02, 0.03, 0.015, 0.04, 0.02, 0.03, 0.012, 0.05), each = 2),
    stringsAsFactors = FALSE
  )
}

ef_fields <- c("b", "ci.lb", "ci.ub", "pval", "se", "tau2", "i2", "k",
               "convergence", "notes")
ef_fun <- function(key) .ma_method_registry[[key]]$fun
ef_data_for <- function(key) {
  if (identical(.ma_method_registry[[key]]$deps, "modeled")) ef_nested() else ef_single()
}

test_that("every registered method returns the full, correctly typed result", {
  keys <- setdiff(list_ma_methods(), "bayesmeta")   # bayesmeta: see slow test below
  expect_true(all(c("fe", "reml", "pm", "hk-sj", "pet-peese", "uwls", "waap",
                    "three-level", "rve") %in% keys))
  for (key in keys) {
    fit <- suppressWarnings(suppressMessages(ef_fun(key)(ef_data_for(key))))
    expect_s3_class(fit, "universe_result")
    expect_named(fit, ef_fields, info = key)
    for (f in c("b", "ci.lb", "ci.ub", "pval", "se", "tau2", "i2")) {
      expect_true(is.numeric(fit[[f]]) && length(fit[[f]]) == 1, info = paste(key, f))
    }
    expect_true(is.integer(fit$k) && length(fit$k) == 1, info = key)
    expect_true(is.logical(fit$convergence) && length(fit$convergence) == 1, info = key)
    # a converged fit with a finite estimate always reports how many units it used
    if (is.finite(fit$b)) {
      expect_false(is.na(fit$k), info = key)
      expect_true(fit$convergence, info = key)
    }
  }
})

test_that("fe, reml and pm reproduce metafor for se, tau2, i2 and k", {
  d <- ef_single()
  refs <- list(
    fe   = metafor::rma(yi = d$yi, vi = d$vi, method = "FE"),
    reml = metafor::rma(yi = d$yi, vi = d$vi, method = "REML",
                        control = list(stepadj = 0.5, maxiter = 2000)),
    pm   = metafor::rma(yi = d$yi, vi = d$vi, method = "PM", test = "z")
  )
  for (key in names(refs)) {
    fit <- ef_fun(key)(d); ref <- refs[[key]]
    expect_equal(fit$b, as.numeric(ref$b), info = key)
    expect_equal(fit$se, as.numeric(ref$se), info = key)
    expect_equal(fit$i2, ref$I2, info = key)
    expect_identical(fit$k, as.integer(ref$k), info = key)
  }
  expect_equal(ef_fun("reml")(d)$tau2, refs$reml$tau2)
  expect_equal(ef_fun("pm")(d)$tau2, refs$pm$tau2)
  # a fixed-effect model has no between-study variance by definition
  expect_identical(ef_fun("fe")(d)$tau2, 0)
})

test_that("tau2 is a variance and i2 is a percentage, for every method that fills them", {
  d <- ef_single()
  ref <- metafor::rma(yi = d$yi, vi = d$vi, method = "REML",
                      control = list(stepadj = 0.5, maxiter = 2000))
  fit <- ef_fun("reml")(d)
  # the historical mix-up: tau stored where tau2 belongs
  expect_equal(sqrt(fit$tau2), sqrt(ref$tau2))
  expect_false(isTRUE(all.equal(fit$tau2, sqrt(ref$tau2))))

  for (key in setdiff(list_ma_methods(), "bayesmeta")) {
    fit <- suppressWarnings(suppressMessages(ef_fun(key)(ef_data_for(key))))
    if (!is.na(fit$tau2)) expect_gte(fit$tau2, 0, label = paste(key, "tau2"))
    if (!is.na(fit$i2)) {
      expect_gte(fit$i2, 0, label = paste(key, "i2"))
      expect_lte(fit$i2, 100, label = paste(key, "i2"))
    }
  }
  # this data set is heterogeneous: a proportion (0-1) would fail here
  expect_gt(ef_fun("reml")(d)$i2, 1)
  expect_gt(ef_fun("hk-sj")(d)$i2, 1)
})

test_that("hk-sj reproduces meta::metagen and converts I2 to percent", {
  d <- ef_single()
  ref <- meta::metagen(TE = d$yi, seTE = sqrt(d$vi),
                       method.random.ci = "HK", method.tau = "SJ")
  fit <- ef_fun("hk-sj")(d)
  expect_equal(fit$b, ref$TE.random)
  expect_equal(fit$se, ref$seTE.random)
  expect_equal(fit$tau2, ref$tau2)
  expect_equal(fit$i2, 100 * ref$I2)
  expect_identical(fit$k, as.integer(ref$k))
})

test_that("regression-based bias methods report se and k but no heterogeneity model", {
  d <- ef_single()
  for (key in c("pet-peese", "pet-peese-corrected", "uwls")) {
    fit <- ef_fun(key)(d)
    expect_true(is.finite(fit$se) && fit$se > 0, info = key)
    expect_identical(fit$k, nrow(d), info = key)
    expect_true(is.na(fit$tau2), info = key)
    expect_true(is.na(fit$i2), info = key)
  }
  reg <- stats::lm(yi ~ 0 + Precision, data = transform(d, yi = yi / sqrt(vi), Precision = 1 / sqrt(vi)))
  expect_equal(ef_fun("uwls")(d)$se,
               unname(stats::coef(summary(reg))["Precision", "Std. Error"]))

  waap <- ef_fun("waap")(d)
  if (is.finite(waap$b)) {
    # k counts the adequately powered studies only, and the note says so
    expect_lte(waap$k, nrow(d))
    expect_match(waap$notes, paste0("^", waap$k, "/", nrow(d), " studies powered$"))
  }
})

test_that("p-uniform* reports k and a non-negative tau2 when it fits", {
  skip_if_not("p-uniform" %in% list_ma_methods())
  d <- ef_single()
  fit <- suppressWarnings(ef_fun("p-uniform")(d))
  skip_if(!is.finite(fit$b), "p-uniform* did not fit on the test data")
  expect_identical(fit$k, nrow(d))
  expect_true(is.na(fit$tau2) || fit$tau2 >= 0)
})

test_that("three-level and rve report study-level k and total between-cluster variance", {
  d <- ef_nested()
  ref <- metafor::rma.mv(data = d, yi = yi, V = vi, random = ~ 1 | study/es_id,
                         method = "REML", sparse = TRUE)
  tl <- ef_fun("three-level")(d)
  expect_equal(tl$b, as.numeric(ref$b))
  expect_equal(tl$se, as.numeric(ref$se))
  expect_equal(tl$tau2, sum(ref$sigma2))
  # k is the number of studies, not the 16 effect sizes
  expect_identical(tl$k, 8L)
  expect_match(tl$notes, "^sigma2 = ")

  skip_if_not_installed("clubSandwich")
  rob <- metafor::robust(ref, cluster = d$study, clubSandwich = TRUE)
  rve <- ef_fun("rve")(d)
  expect_equal(rve$b, as.numeric(rob$b))
  expect_equal(rve$se, as.numeric(rob$se))
  expect_equal(rve$tau2, sum(ref$sigma2))
  expect_identical(rve$k, 8L)
  # same point estimate as the three-level model, different standard error
  expect_equal(rve$b, tl$b)
})

test_that("bayesmeta fills se from the posterior sd and tau2 from the squared median tau", {
  skip_if(getOption("metaMultiverse.skip_slow", FALSE))
  skip_if_not("bayesmeta" %in% list_ma_methods())
  d <- ef_single()
  fit <- suppressWarnings(ef_fun("bayesmeta")(d))
  skip_if(!is.finite(fit$b), "bayesmeta did not fit on the test data")
  expect_true(is.finite(fit$se) && fit$se > 0)
  expect_true(is.finite(fit$tau2) && fit$tau2 >= 0)
  expect_identical(fit$k, nrow(d))
  expect_true(is.na(fit$pval))
})

test_that("a failed fit is flagged as not converged and carries no numbers", {
  expect_false(universe_NA$convergence)
  expect_true(all(is.na(unlist(universe_NA[c("b", "ci.lb", "ci.ub", "pval", "se", "tau2", "i2", "k")]))))
  empty <- ef_single()[0, ]
  fit <- suppressWarnings(suppressMessages(ef_fun("reml")(empty)))
  expect_true(is.na(fit$b))
  expect_false(fit$convergence)
  expect_false(is_valid_universe(fit))
})

test_that("the grid runner carries the enriched fields into its results", {
  sim <- simulate_multiverse_data(n_studies = 30, es_per_study = 1:2, seed = 11)
  cfg <- domain_config(
    domain = "estimator_fields",
    forks = list(
      fork("ma_method", "analysis", "uncertain", "analytic",
           list(basis = "convention-unresolved", text = "test"),
           levels = c("reml", "fe")),
      fork("dependency", "analysis", "uncertain", "analytic",
           list(basis = "convention-unresolved", text = "test"),
           levels = c("aggregate", "modeled"))
    ),
    estimand = list(population = "sim", intervention = "tx", outcome = "y", timepoint = "post")
  )
  res <- suppressWarnings(suppressMessages(
    run_multiverse_grid(build_specification_grid(cfg, sim), progress = FALSE)
  ))
  out <- res$results
  expect_true(all(c("se", "tau2", "i2", "k", "n_es", "converged", "method_label") %in% names(out)))
  expect_true(all(is.finite(out$se) & out$se > 0))
  expect_true(all(out$converged))
  expect_true(all(out$k <= out$n_es))
  reml <- out[out$ma_method == "reml", ]
  expect_true(all(is.finite(reml$tau2) & reml$tau2 >= 0))
})
