# tests/testthat/test-general_multiverse.R

library(testthat)
library(withr)
library(metaMultiverse)

# Lower the smallest‐MA threshold so no automatic skips
withr::local_options(list(metaMultiverse.k_smallest_ma = 1))

# ----------------------------------------
# Register three fake meta‐analysis methods
# ----------------------------------------
register_ma_method(
  "fake_select",
  function(data) new_universe_result(10, 9, 11, 0.1),
  dependencies = c("select_max", "select_min")
)
register_ma_method(
  "fake_agg",
  function(data) new_universe_result(20, 19, 21, 0.2),
  dependencies = "aggregate"
)
register_ma_method(
  "fake_mod",
  function(data) new_universe_result(30, 29, 31, 0.3),
  dependencies = "modeled"
)

# ----------------------------------------
# Minimal dataset exercising all branches
# ----------------------------------------
fake_data <- data.frame(
  study = rep(c("S1", "S2"), each = 2),
  es_id = 1:4,
  yi    = c(0.1, 0.2, 0.3, 0.4),
  vi    = c(0.01, 0.02, 0.03, 0.04),
  wf_1  = c("A", "A", "B", "B"),
  wf_2  = c("total_X", "total_X", "total_X", "total_X"),
  stringsAsFactors = FALSE
)

base_specs <- data.frame(
  multiverse_id = 1,
  wf_1          = rep("A", 3),
  wf_2          = rep("total_X", 3),
  dependency    = c("select_max", "aggregate", "modeled"),
  ma_method     = c("fake_select", "fake_agg",   "fake_mod"),
  stringsAsFactors = FALSE
)

test_that("select_max branch uses fake_select", {
  res <- general_multiverse(
    i = 1,
    data = fake_data,
    specifications = base_specs[1, , drop = FALSE],
    k_smallest_ma = 1
  )
  expect_s3_class(res, "data.frame")
  expect_equal(res$b, 10)
  expect_equal(res$ci.lb, 9)
})

test_that("aggregate branch uses fake_agg", {
  res <- general_multiverse(
    i = 1,
    data = fake_data,
    specifications = base_specs[2, , drop = FALSE],
    k_smallest_ma = 1
  )
  expect_equal(res$b, 20)
  expect_equal(res$pval, 0.2)
})

test_that("modeled branch uses fake_mod", {
  # ensure at least one duplicated study
  res <- general_multiverse(
    i = 1,
    data = fake_data,
    specifications = base_specs[3, , drop = FALSE],
    k_smallest_ma = 1
  )
  expect_equal(res$b, 30)
  expect_equal(res$ci.ub, 31)
})

test_that("filters wf_ and drops NAs correctly", {
  d2 <- rbind(
    fake_data,
    data.frame(study = "S3", es_id = 5, yi = NA, vi = 0.05,
               wf_1 = "C", wf_2 = "total_X", stringsAsFactors = FALSE)
  )
  specs <- base_specs[1, , drop = FALSE]
  res <- general_multiverse(i = 1, data = d2, specifications = specs, k_smallest_ma = 1)
  # wf_1 == "A" keeps es_id 1,2; both non-NA → k = 2
  expect_equal(res$k, 2L)
})

test_that("returns NULL + warning when below k_smallest_ma", {
  expect_warning(
    out <- general_multiverse(
      i = 1,
      data = fake_data,
      specifications = base_specs[1, , drop = FALSE],
      k_smallest_ma = 3
    ),
    "Specification 1 skipped: only .* unique studies found"
  )
  expect_null(out)
})

test_that("output contains spec columns and result columns", {
  res <- general_multiverse(
    i = 1,
    data = fake_data,
    specifications = base_specs[1, , drop = FALSE],
    k_smallest_ma = 1
  )
  cols <- names(res)
  expect_true(all(
    c("wf_1", "wf_2", "dependency", "ma_method", "multiverse_id",
      "b", "ci.lb", "ci.ub", "pval", "k", "set"
    ) %in% cols
  ))
  expect_type(res$b,   "double")
  expect_type(res$set, "character")
})
