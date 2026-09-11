# tests/testthat/test-pre_post.R
# Pre/post (change-score) designs: effect-size metric as a "how" factor

pp_toy <- function() {
  data.frame(
    study = c("A", "B", "C"),
    n_1 = c(30, 40, 25), n_2 = c(32, 38, 27),
    pre_1_m = c(24, 22, 25), pre_1_sd = c(6, 5, 7),
    pre_2_m = c(23, 23, 24), pre_2_sd = c(6, 5, 6),
    post_1_m = c(14, 12, 15), post_1_sd = c(8, 7, 9),
    post_2_m = c(13, 12, 12), post_2_sd = c(8, 7, 8)
  )
}

pp_eight <- function() {
  data.frame(
    study = paste("Study", 1:8),
    n_1 = c(30, 40, 25, 50, 20, 35, 45, 28), n_2 = c(32, 38, 27, 48, 22, 33, 44, 30),
    pre_1_m = c(24, 22, 25, 23, 26, 21, 24, 22), pre_1_sd = 6,
    pre_2_m = c(23, 23, 24, 23, 25, 22, 24, 23), pre_2_sd = 6,
    post_1_m = c(14, 12, 15, 13, 16, 11, 14, 12), post_1_sd = 8,
    post_2_m = c(13, 12, 12, 13, 14, 12, 13, 11), post_2_sd = 8,
    rater = rep(c("clinician", "self"), 4)
  )
}

# Luo et al. (2020) corrigendum, Table 1: eCBT (e) vs face-to-face CBT (c).
# Group 1 = eCBT, group 2 = face-to-face, so positive g favours face-to-face.
pp_luo_corrigendum <- function() {
  d <- read.csv(text = "
study,n_e,n_c,pre_c_m,pre_c_sd,pre_e_m,pre_e_sd,post_c_m,post_c_sd,post_e_m,post_e_sd
Luxton 2016,62,59,29.69,11.74,26.65,11.80,11.74,12.08,13.63,12.47
Mohr 2012,163,162,22.83,4.60,22.83,4.60,12.51,8.41,13.58,7.55
Choi 2014,56,63,27.75,6.59,23.54,6.44,14.08,7.46,13.68,7.48
Wagner 2014,32,30,23.41,7.63,22.96,6.07,12.33,8.77,12.41,10.03
Sethi 2013,23,21,21.42,6.80,17.65,5.03,7.80,3.09,12.17,3.12
Sethi 2010,9,10,19.00,5.10,16.40,9.20,7.20,3.10,15.70,4.20
Wright 2005,15,15,24.40,6.80,31.10,8.20,9.70,8.50,10.70,8.10
Himelhoch 2013,16,18,24.60,5.30,22.60,5.20,15.90,7.20,16.30,8.60
Poppelaars 2016,51,50,63.35,10.39,62.61,11.97,59.33,13.27,57.88,12.57
Glueckauf 2012,7,7,11.80,7.40,12.67,8.91,9.00,5.70,4.67,2.07
Kalapatapu 2014,50,53,21.80,3.80,22.20,4.70,11.80,7.20,12.80,9.20
Nelson 2003,14,14,13.57,8.75,14.36,9.85,11.64,11.63,6.71,4.78
Kay-Lambkin 2009,32,35,34.91,9.70,28.57,9.89,16.65,10.63,17.09,12.14
Andersson 2013,33,36,24.10,5.00,23.60,4.80,17.10,8.00,13.60,9.80
", strip.white = TRUE, stringsAsFactors = FALSE)
  data.frame(
    study = d$study,
    n_1 = d$n_e, n_2 = d$n_c,
    pre_1_m = d$pre_e_m, pre_1_sd = d$pre_e_sd, pre_2_m = d$pre_c_m, pre_2_sd = d$pre_c_sd,
    post_1_m = d$post_e_m, post_1_sd = d$post_e_sd, post_2_m = d$post_c_m, post_2_sd = d$post_c_sd,
    inclusion = "all",
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------------------------
# compute_pre_post_es()
# ------------------------------------------------------------------------------

test_that("post_smd reproduces metafor::escalc for the post-test SMD", {
  d <- pp_toy()
  out <- compute_pre_post_es(d, "post_smd")
  ref <- metafor::escalc("SMD", m1i = d$post_1_m, sd1i = d$post_1_sd, n1i = d$n_1,
                         m2i = d$post_2_m, sd2i = d$post_2_sd, n2i = d$n_2)
  expect_equal(out$yi, as.numeric(ref$yi))
  expect_equal(out$vi, as.numeric(ref$vi))
  expect_equal(out$es_metric, rep("post_smd", 3))
  expect_equal(out$imputed_post_sd, rep("borrow", 3))
})

test_that("post_smd uses n_*_post for the SMD when present", {
  d <- pp_toy()
  d$n_1_post <- c(28, NA, 20); d$n_2_post <- c(30, NA, NA)
  out <- compute_pre_post_es(d, "post_smd")
  ref <- metafor::escalc("SMD", m1i = d$post_1_m, sd1i = d$post_1_sd, n1i = c(28, 40, 20),
                         m2i = d$post_2_m, sd2i = d$post_2_sd, n2i = c(30, 38, 27))
  expect_equal(out$yi, as.numeric(ref$yi))
  expect_equal(out$vi, as.numeric(ref$vi))
})

test_that("change_smd_r<r> imputes the change SD from pre and post SDs (Cochrane 6.5.2.8)", {
  d <- pp_toy()
  r <- 0.6
  out <- compute_pre_post_es(d, "change_smd_r0.6")
  sd_chg <- function(sd_pre, sd_post) sqrt(sd_pre^2 + sd_post^2 - 2 * r * sd_pre * sd_post)
  ref <- metafor::escalc(
    "SMD",
    m1i = d$pre_2_m - d$post_2_m, sd1i = sd_chg(d$pre_2_sd, d$post_2_sd), n1i = d$n_2,
    m2i = d$pre_1_m - d$post_1_m, sd2i = sd_chg(d$pre_1_sd, d$post_1_sd), n2i = d$n_1
  )
  expect_equal(out$yi, as.numeric(ref$yi))
  expect_equal(out$vi, as.numeric(ref$vi))
  expect_equal(unique(out$es_metric), "change_smd_r0.6")
  expect_equal(unique(out$imputed_post_sd), "not_applicable")
})

test_that("change_smd_r<r> uses reported change scores where present", {
  d <- pp_toy()
  d$change_1_m <- c(NA, 9.5, NA); d$change_1_sd <- c(NA, 6.5, NA)
  d$change_2_m <- c(NA, 11.5, NA); d$change_2_sd <- c(NA, 6.0, NA)
  out <- compute_pre_post_es(d, "change_smd_r0.3")
  ref_b <- metafor::escalc("SMD", m1i = 11.5, sd1i = 6.0, n1i = 38, m2i = 9.5, sd2i = 6.5, n2i = 40)
  expect_equal(out$yi[out$study == "B"], as.numeric(ref_b$yi))
  expect_equal(out$vi[out$study == "B"], as.numeric(ref_b$vi))
  # rows without reported change scores are unaffected
  plain <- compute_pre_post_es(pp_toy(), "change_smd_r0.3")
  expect_equal(out$yi[out$study != "B"], plain$yi[plain$study != "B"])
})

test_that("change_smd_r<r> has the same sign as post_smd and is monotone in r", {
  d <- pp_toy()
  d$pre_2_m <- d$pre_1_m   # equal baselines: change and post differences agree in sign
  post <- compute_pre_post_es(d, "post_smd")
  chg <- compute_pre_post_es(d, "change_smd_r0.5")
  expect_equal(sign(post$yi), sign(chg$yi))
  # a higher r shrinks the imputed change SD and so inflates |yi|
  lo <- compute_pre_post_es(d, "change_smd_r0.2")
  hi <- compute_pre_post_es(d, "change_smd_r0.8")
  expect_true(all(abs(hi$yi) >= abs(lo$yi)))
})

test_that("smc_r<r> equals the difference of metafor SMCR per arm", {
  d <- pp_toy()[1:2, ]
  out <- compute_pre_post_es(d, "smc_r0.5")
  e1 <- metafor::escalc("SMCR", m1i = d$post_1_m, m2i = d$pre_1_m, sd1i = d$pre_1_sd, ni = d$n_1, ri = c(0.5, 0.5))
  e2 <- metafor::escalc("SMCR", m1i = d$post_2_m, m2i = d$pre_2_m, sd1i = d$pre_2_sd, ni = d$n_2, ri = c(0.5, 0.5))
  expect_equal(out$yi, as.numeric(e1$yi - e2$yi))
  expect_equal(out$vi, as.numeric(e1$vi + e2$vi))
  expect_equal(unique(out$imputed_post_sd), "not_applicable")
})

test_that("imputed post SD handling: exclude, borrow, change_smd", {
  d <- data.frame(
    study = c("A", "B"), n_1 = c(30, 15), n_2 = c(32, 15),
    pre_1_m = c(24, 31.4), pre_1_sd = c(6, 8.2), pre_2_m = c(23, 24.4), pre_2_sd = c(6, 6.8),
    post_1_m = c(14, 13.9), post_1_sd = c(8, 10.8), post_2_m = c(13, 9.7), post_2_sd = c(8, 8.0),
    change_1_m = c(NA, 17.5), change_1_sd = c(NA, 10.8), change_2_m = c(NA, 14.7), change_2_sd = c(NA, 8.0),
    post_sd_imputed = c(FALSE, TRUE)
  )
  ex <- compute_pre_post_es(d, "post_smd", "exclude")
  expect_equal(ex$study, "A")
  expect_equal(unique(ex$imputed_post_sd), "exclude")
  br <- compute_pre_post_es(d, "post_smd", "borrow")
  expect_equal(nrow(br), 2)
  cs <- compute_pre_post_es(d, "post_smd", "change_smd")
  ref <- metafor::escalc("SMD", m1i = 14.7, sd1i = 8.0, n1i = 15, m2i = 17.5, sd2i = 10.8, n2i = 15)
  expect_equal(cs$yi[cs$study == "B"], as.numeric(ref$yi))
  expect_equal(br$yi[br$study == "A"], cs$yi[cs$study == "A"])
  # sign flips between metrics for the baseline-imbalanced study B
  expect_gt(br$yi[br$study == "B"], 0)
  expect_lt(cs$yi[cs$study == "B"], 0)
  # the rule does not touch the change-score and SMC metrics
  expect_equal(nrow(compute_pre_post_es(d, "change_smd_r0.3", "exclude")), 2)
  expect_equal(nrow(compute_pre_post_es(d, "smc_r0.3", "exclude")), 2)
})

test_that("adjusted_post_smd uses the reported adjusted difference where available", {
  d <- data.frame(
    study = c("A", "B"), n_1 = c(62, 30), n_2 = c(59, 32),
    pre_1_m = c(27.6, 24), pre_1_sd = c(10.45, 6), pre_2_m = c(29.71, 23), pre_2_sd = c(11.33, 6),
    post_1_m = c(13.82, 14), post_1_sd = c(12.02, 8), post_2_m = c(11.74, 13), post_2_sd = c(12.08, 8),
    adj_diff = c(4.23, NA), adj_diff_lb = c(0.66, NA), adj_diff_ub = c(7.81, NA), adj_ci_level = c(0.90, NA)
  )
  out <- compute_pre_post_es(d, "adjusted_post_smd")
  sd_pool <- sqrt((61 * 12.02^2 + 58 * 12.08^2) / 119)
  se_raw <- (7.81 - 0.66) / (2 * qnorm(0.95))
  expect_equal(out$yi[out$study == "A"], 4.23 / sd_pool)
  expect_equal(out$vi[out$study == "A"], (se_raw / sd_pool)^2)
  # fallback for rows without an adjusted difference is the post-test SMD
  post <- compute_pre_post_es(d, "post_smd")
  expect_equal(out$yi[out$study == "B"], post$yi[post$study == "B"])
  expect_equal(out$vi[out$study == "B"], post$vi[post$study == "B"])
  # without any adjusted difference the metric equals post_smd everywhere
  expect_equal(compute_pre_post_es(pp_toy(), "adjusted_post_smd")$yi,
               compute_pre_post_es(pp_toy(), "post_smd")$yi)
})

test_that("compute_pre_post_es errors informatively", {
  expect_error(compute_pre_post_es(data.frame(study = "A"), "post_smd"), "missing columns")
  d <- pp_toy()
  expect_error(compute_pre_post_es(d, "nonsense"), "unknown metric")
  expect_error(compute_pre_post_es(d, "adjusted_hybrid"), "unknown metric")
  expect_error(compute_pre_post_es(d, "smc_r1.5"), "unknown metric")
  expect_error(compute_pre_post_es(d, "change_smd"), "unknown metric")
  expect_error(compute_pre_post_es(d, "post_smd", "drop"), "should be one of")
  expect_error(compute_pre_post_es("not a data frame", "post_smd"), "data frame")
})

test_that("rows without a computable effect size are dropped", {
  d <- pp_toy()
  d$post_1_sd[2] <- NA
  out <- compute_pre_post_es(d, "post_smd")
  expect_equal(out$study, c("A", "C"))
  # SMC needs only the pre SD, so the row survives there
  expect_equal(nrow(compute_pre_post_es(d, "smc_r0.5")), 3)
})

# ------------------------------------------------------------------------------
# register_metafor_estimators()
# ------------------------------------------------------------------------------

test_that("register_metafor_estimators adds four keys that reproduce metafor::rma", {
  keys <- c("dl", "dl_hksj", "reml_hksj", "pm_hksj")
  out <- register_metafor_estimators()
  expect_true(all(keys %in% out))
  expect_true(all(keys %in% list_ma_methods()))
  # calling again is harmless
  expect_true(all(keys %in% register_metafor_estimators()))

  dat <- compute_pre_post_es(pp_eight(), "post_smd")
  fit <- .ma_method_registry[["dl_hksj"]]$fun(dat)
  ref <- metafor::rma(yi = dat$yi, vi = dat$vi, method = "DL", test = "knha",
                      control = list(stepadj = 0.5, maxiter = 2000))
  expect_s3_class(fit, "universe_result")
  expect_equal(fit$b, as.numeric(ref$b))
  expect_equal(fit$ci.lb, ref$ci.lb)
  expect_equal(fit$ci.ub, ref$ci.ub)
  expect_equal(fit$pval, ref$pval)
  expect_setequal(.ma_method_registry[["dl"]]$deps, c("select_max", "select_min", "aggregate"))
})

# ------------------------------------------------------------------------------
# run_pre_post_multiverse()
# ------------------------------------------------------------------------------

test_that("run_pre_post_multiverse stacks variants and returns character factors", {
  register_metafor_estimators()
  mv <- run_pre_post_multiverse(
    pp_eight(), which_factors = list(rater = "rater|E"),
    es_grid = data.frame(es_metric = c("post_smd", "smc_r0.5"), imputed_post_sd = "borrow"),
    ma_methods = c("reml", "dl_hksj"), dependencies = "aggregate",
    k_smallest_ma = 3, verbose = FALSE
  )
  expect_s3_class(mv$results, "data.frame")
  expect_setequal(unique(mv$results$es_metric), c("post_smd", "smc_r0.5"))
  expect_setequal(unique(mv$results$ma_method), c("reml", "dl_hksj"))
  expect_true("rater" %in% names(mv$results))
  expect_false("wf_1" %in% names(mv$results))
  expect_type(mv$results$rater, "character")
  expect_type(mv$results$ma_method, "character")
  expect_true(all(mv$results$k_studies <= 8))
  expect_true(all(mv$results$k_studies == mv$results$k))
  # the all-levels option (total_<wf>) uses all 8 studies
  expect_equal(unique(mv$results$k_studies[startsWith(mv$results$rater, "total_")]), 8)
  expect_true(all(grepl("Study 1; ", mv$results$studies_in_set[startsWith(mv$results$rater, "total_")])))
  # 3 rater levels x 2 methods x 2 metrics
  expect_equal(nrow(mv$results), 12)
  expect_equal(mv$n_attempted, 12)
  expect_s3_class(mv$specifications, "data.frame")
  expect_true(all(c("rater", "es_metric", "imputed_post_sd") %in% names(mv$specifications)))
  expect_type(mv$warnings, "character")
})

test_that("run_pre_post_multiverse accepts which_factors as a function and a spec_filter", {
  register_metafor_estimators()
  wf_fun <- function(dat) {
    present <- unique(dat$study)
    list(
      rater = "rater|E",
      inclusion = list("study", decision = "U", groups = list(
        all = present,
        no_study_1 = setdiff(present, "Study 1")
      ))
    )
  }
  # keep only the "all" inclusion level, dropping the automatic total
  keep_all <- function(specs, es_row, factor_info) {
    wf <- setNames(factor_info$wf_internal, factor_info$label)
    specs[specs[[wf[["inclusion"]]]] == "all", , drop = FALSE]
  }
  mv <- run_pre_post_multiverse(
    pp_eight(), which_factors = wf_fun,
    es_grid = data.frame(es_metric = c("post_smd", "change_smd_r0.5"), imputed_post_sd = "borrow"),
    ma_methods = "dl", dependencies = "aggregate",
    spec_filter = keep_all, k_smallest_ma = 3, verbose = FALSE
  )
  expect_true(all(c("rater", "inclusion") %in% names(mv$results)))
  expect_equal(unique(mv$results$inclusion), "all")
  expect_equal(nrow(mv$results), 3 * 2)
  expect_equal(mv$n_attempted, 6)
})

test_that("run_pre_post_multiverse survives variants where every specification fails", {
  mv <- run_pre_post_multiverse(
    pp_eight(), which_factors = list(rater = "rater|E"),
    es_grid = data.frame(es_metric = "post_smd", imputed_post_sd = "borrow"),
    ma_methods = "reml", dependencies = "aggregate",
    k_smallest_ma = 20, verbose = FALSE
  )
  expect_null(mv$results)
  expect_null(mv$specifications)
  expect_equal(mv$n_attempted, 3)
  expect_true(length(mv$warnings) > 0)
})

test_that("run_pre_post_multiverse validates its inputs before running", {
  d <- pp_eight()
  wf <- list(rater = "rater|E")
  expect_error(run_pre_post_multiverse(d, wf, es_grid = data.frame(es_metric = "post_smd")),
               "es_grid")
  expect_error(run_pre_post_multiverse(d, wf, es_grid = data.frame(es_metric = "adjusted_hybrid", imputed_post_sd = "borrow")),
               "unknown metric")
  expect_error(run_pre_post_multiverse(d, wf, es_grid = data.frame(es_metric = "post_smd", imputed_post_sd = "drop")),
               "unknown imputed_post_sd")
  expect_error(run_pre_post_multiverse(d, "rater|E"), "which_factors")
  expect_error(run_pre_post_multiverse(d, wf, ma_methods = "no_such_method"), "unknown ma_methods")
})

test_that("k_smallest_ma is restored after the call", {
  old <- getOption("metaMultiverse.k_smallest_ma")
  invisible(run_pre_post_multiverse(
    pp_eight(), which_factors = list(rater = "rater|E"),
    es_grid = data.frame(es_metric = "post_smd", imputed_post_sd = "borrow"),
    ma_methods = "reml", dependencies = "aggregate", k_smallest_ma = 3, verbose = FALSE
  ))
  expect_identical(getOption("metaMultiverse.k_smallest_ma"), old)
})

# ------------------------------------------------------------------------------
# regression against the Luo et al. (2020) reanalysis
# ------------------------------------------------------------------------------

test_that("corrigendum Table 1, post-test SMD, DerSimonian-Laird: g = 0.10 [-0.13, 0.33]", {
  register_metafor_estimators()
  d <- pp_luo_corrigendum()

  dat <- compute_pre_post_es(d, "post_smd")
  expect_equal(nrow(dat), 14)
  fit <- .ma_method_registry[["dl"]]$fun(dat)
  expect_equal(round(c(fit$b, fit$ci.lb, fit$ci.ub), 2), c(0.10, -0.13, 0.33))

  # the same number through the full pipeline
  mv <- run_pre_post_multiverse(
    d, which_factors = list(inclusion = "inclusion|E"),
    es_grid = data.frame(es_metric = "post_smd", imputed_post_sd = "borrow"),
    ma_methods = "dl", dependencies = "aggregate", k_smallest_ma = 5, verbose = FALSE
  )
  # "all" and "total_wf_1" select the same 14 studies and are deduplicated
  expect_equal(nrow(mv$results), 1)
  expect_equal(mv$results$k_studies, 14)
  expect_equal(round(c(mv$results$b, mv$results$ci.lb, mv$results$ci.ub), 2), c(0.10, -0.13, 0.33))
})
