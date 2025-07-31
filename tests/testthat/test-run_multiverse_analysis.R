# tests/testthat/test-run_multiverse_analysis.R

library(testthat)
library(withr)
library(metaMultiverse)

# Stub out general_multiverse() so we control success/failure
dummy_gm <- function(i, data, specifications) {
  spec <- specifications[i, ]
  if (spec$row_id == 1) {
    stop("boom on purpose")
  }
  # Return minimal one‐row data.frame matching expected columns
  data.frame(
    b             = spec$bfake,
    ci.lb         = spec$bfake - 0.1,
    ci.ub         = spec$bfake + 0.1,
    pval          = 0.05,
    k             = nrow(data),
    set           = paste(seq_len(nrow(data)), collapse = ","),
    wf_test       = spec$wf_test,
    dependency    = spec$dependency,
    ma_method     = spec$ma_method,
    multiverse_id = spec$multiverse_id,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

test_that("errors on invalid inputs", {
  df <- data.frame(a = 1)
  specs <- data.frame()
  expect_error(
    run_multiverse_analysis(list(), specs),
    "'data' must be a data frame"
  )
  expect_error(
    run_multiverse_analysis(df, list()),
    "'specifications' must be a data frame"
  )
  expect_error(
    run_multiverse_analysis(df, specs),
    "'specifications' cannot be empty"
  )
})

test_that("warns if data unvalidated and returns multiverse_result", {
  df <- data.frame(study = 1, es_id = 1, yi = 0, vi = 1, wf_test = "A")
  specs <- data.frame(
    bfake         = 0.2,
    wf_test       = "A",
    dependency    = "aggregate",
    ma_method     = "fe",
    multiverse_id = 1,
    row_id        = 2,
    stringsAsFactors = FALSE
  )

  local_mocked_bindings(
    general_multiverse = dummy_gm,
    .env = asNamespace("metaMultiverse")
  )

  expect_warning(
    res <- run_multiverse_analysis(df, specs, progress = FALSE),
    "Data appears unvalidated"
  )

  expect_s3_class(res, "multiverse_result")
  expect_equal(res$n_attempted, 1)
  expect_equal(res$n_successful, 1)
  expect_equal(res$n_warnings, 0)
  expect_length(res$multiverse_warnings, 0)
})

test_that("handles mix of success and failure, processes results correctly", {
  # mark data validated
  df <- data.frame(study = 1:2, es_id = 1:2, yi = c(0,0), vi = c(1,1), wf_test = c("X","Y"))
  attr(df, "multiverse_validated") <- TRUE

  specs <- data.frame(
    bfake         = c(NA, 0.5),
    wf_test       = c("X", "Y"),
    dependency    = c("select_max", "aggregate"),
    ma_method     = c("fe", "reml"),
    multiverse_id = c(1,1),
    row_id        = 1:2,
    stringsAsFactors = FALSE
  )

  local_mocked_bindings(
    general_multiverse = dummy_gm,
    .env = asNamespace("metaMultiverse")
  )

  res <- run_multiverse_analysis(df, specs, verbose = FALSE, progress = FALSE)

  # Should attempt 2, succeed 1
  expect_s3_class(res, "multiverse_result")
  expect_equal(res$n_attempted, 2)
  expect_equal(res$n_successful, 1)
  expect_equal(res$n_final, 1)
  expect_equal(res$n_warnings, 1)

  # Check results data.frame
  df_res <- res$results
  expect_s3_class(df_res, "data.frame")
  expect_equal(nrow(df_res), 1)
  expect_equal(df_res$b, 0.5)
  expect_equal(df_res$ci.lb, 0.4)
  expect_equal(df_res$ci.ub, 0.6)
  expect_equal(df_res$k, 2)
  expect_equal(df_res$set, "1,2")
  expect_equal(df_res$wf_test, "Y")
  expect_equal(df_res$dependency, "aggregate")
  expect_equal(df_res$ma_method, "reml")
  expect_equal(df_res$multiverse_id, 1)
  expect_equal(df_res$full_set, 1)
})

test_that("returns warning when no specs succeed", {
  df <- data.frame(study = 1, es_id = 1, yi = 0, vi = 1, wf_test = "A")
  attr(df, "multiverse_validated") <- TRUE

  specs <- data.frame(
    bfake         = NA,
    wf_test       = "A",
    dependency    = "select_max",
    ma_method     = "fe",
    multiverse_id = 1,
    row_id        = 1,
    stringsAsFactors = FALSE
  )

  local_mocked_bindings(
    general_multiverse = dummy_gm,
    .env = asNamespace("metaMultiverse")
  )

  expect_warning(
    res <- run_multiverse_analysis(df, specs, progress = FALSE),
    "All \\d+ specifications failed\\. Check your data and specifications\\."
  )
  expect_null(res$results)
  expect_equal(res$n_successful, 0)
  expect_equal(res$n_attempted, 1)
})
