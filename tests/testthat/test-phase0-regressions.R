# Phase 0 regression tests: standalone bug fixes made before the v1.0 refactor
# (see dev/gap-analysis-and-plan.md, Phase 0). Each test pins a bug that either
# existed in v0.2.3 or existed in legacy materials and must not re-enter.

# --- full_set with non-sequential es_id -------------------------------------

test_that("full_set is correct when es_id is not 1..n row order", {
  set.seed(42)
  dat <- data.frame(
    study = rep(paste0("s", 1:6), each = 1),
    es_id = c(101, 205, 7, 999, 55, 3), # deliberately non-sequential
    yi = rnorm(6, 0.4, 0.2),
    vi = runif(6, 0.01, 0.05),
    wf_1 = rep(c("a", "b"), 3),
    stringsAsFactors = FALSE
  )
  dat <- check_data_multiverse(dat)
  setup <- define_factors(dat, group = "wf_1|E")
  specs <- create_multiverse_specifications(setup, ma_methods = "reml",
                                            dependencies = "aggregate")
  res <- suppressWarnings(
    run_multiverse_analysis(specs, progress = FALSE, verbose = FALSE)
  )
  full_rows <- res$results[res$results$full_set == 1, ]
  # the total_wf_1 universe includes every effect size -> must be flagged full
  expect_true(nrow(full_rows) >= 1)
  expect_true(all(grepl("total_", full_rows$wf_1)))
  # and universes filtered to one level must never be flagged full
  sub_rows <- res$results[!grepl("total_", res$results$wf_1), ]
  expect_true(all(sub_rows$full_set == 0))
})

# --- silent method x dependency dropout now warns ---------------------------

test_that("requesting a method with no compatible dependency warns instead of silently dropping", {
  dat <- create_data_tiny()
  setup <- define_factors(dat, group = "wf_1|E")
  # p-uniform supports only select_max/select_min; aggregate-only request
  # would silently exclude it in v0.2.3
  expect_warning(
    create_multiverse_specifications(setup,
                                     ma_methods = c("reml", "p-uniform"),
                                     dependencies = "aggregate"),
    regexp = "p-uniform"
  )
})

test_that("no dropout warning when all requested methods are compatible", {
  dat <- create_data_tiny()
  setup <- define_factors(dat, group = "wf_1|E")
  expect_no_warning(
    create_multiverse_specifications(setup, ma_methods = "reml",
                                     dependencies = "aggregate")
  )
})

# --- wf column detection is anchored ----------------------------------------

test_that("columns merely starting with 'wf' are not swept into the factor system", {
  dat <- create_data_tiny()
  dat$wfoo <- rnorm(nrow(dat)) # numeric non-factor column starting with wf
  # v0.2.3's ^wf regex made this a required character factor column -> error
  expect_no_error(check_data_multiverse(dat))
})

# --- narrative report runs with default NULL optional stats -----------------

test_that("generate_multiverse_report_text works with all optional stats NULL", {
  df <- data.frame(
    b = rnorm(20, 0.4, 0.1),
    ci.lb = rnorm(20, 0.2, 0.1),
    ci.ub = rnorm(20, 0.6, 0.1),
    pval = runif(20),
    k = sample(10:60, 20, replace = TRUE),
    multiverse_id = "main",
    stringsAsFactors = FALSE
  )
  expect_no_error(
    report <- generate_multiverse_report_text(df, original_data = NULL,
                                              min_k = 10)
  )
  expect_true(is.character(report))
  expect_true(length(report) > 5)
})

test_that("consistency glue template renders thresholds correctly", {
  df <- data.frame(
    b = rnorm(20, 0.4, 0.1), ci.lb = rnorm(20, 0.2, 0.1),
    ci.ub = rnorm(20, 0.6, 0.1), pval = runif(20),
    k = sample(10:60, 20, replace = TRUE), multiverse_id = "main"
  )
  report <- generate_multiverse_report_text(
    df, original_data = NULL, min_k = 10,
    consistency = list(max_delta = 0.02, thresholds = c(10, 25, 50))
  )
  expect_true(any(grepl("10, 25, 50", report)))
})

# --- prop_significant denominator with NA p-values --------------------------

test_that("prop_significant denominator includes analyses without p-values", {
  df <- data.frame(
    b = rep(0.5, 10), ci.lb = rep(0.3, 10), ci.ub = rep(0.7, 10),
    pval = c(rep(0.01, 5), rep(NA, 5)) # e.g. bayesmeta rows
  )
  rep_out <- generate_multiverse_report(df, specifications = NULL,
                                        original_data = NULL,
                                        include_plots = FALSE)
  expect_equal(rep_out$summary_stats$prop_significant, 0.5) # 5/10, not 5/5
  expect_equal(rep_out$summary_stats$n_with_pval, 5)
})

# --- guards against historical legacy bugs ----------------------------------

test_that("data checker requires yi and vi as distinct numeric columns", {
  # legacy checkDataFormat bug: 'variable.class' listed vi twice instead of
  # yi + vi; guard that both are independently validated
  dat <- create_data_tiny()
  dat_bad_yi <- dat; dat_bad_yi$yi <- as.character(dat_bad_yi$yi)
  expect_error(check_data_multiverse(dat_bad_yi), regexp = "yi")
  dat_bad_vi <- dat; dat_bad_vi$vi <- as.character(dat_bad_vi$vi)
  expect_error(check_data_multiverse(dat_bad_vi), regexp = "vi")
})

test_that("no package code uses read.csv2 or delta.tau2", {
  r_files <- list.files(system.file("R", package = "metaMultiverse"),
                        full.names = TRUE)
  # fallback for load_all/source trees: check the package's R sources directly
  pkg_r <- file.path(testthat::test_path("..", ".."), "R")
  files <- list.files(pkg_r, pattern = "\\.R$", full.names = TRUE)
  skip_if(length(files) == 0)
  src <- unlist(lapply(files, readLines, warn = FALSE))
  expect_false(any(grepl("read\\.csv2", src)))
  expect_false(any(grepl("delta\\.tau2", src)))
})
