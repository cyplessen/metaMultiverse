# tests/testthat/test-general-multiverse.R
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

# ----------------------------------------
# ORIGINAL TESTS (Unchanged)
# ----------------------------------------
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

# ----------------------------------------
# NEW TESTS FOR CUSTOM FACTOR GROUPINGS
# ----------------------------------------

# Create realistic test data for custom groups
create_custom_group_data <- function() {
  data.frame(
    study = rep(paste0("Study_", 1:6), each = 2),
    es_id = 1:12,
    yi = rnorm(12, 0.5, 0.2),
    vi = runif(12, 0.01, 0.03),
    wf_1 = rep(c("low risk", "some concerns", "high risk"), each = 4),
    wf_2 = rep(c("website", "mobile"), times = 6),
    stringsAsFactors = FALSE
  )
}

test_that("custom factor groups map correctly to data levels", {
  custom_data <- create_custom_group_data()

  # Define custom groups
  factor_groups <- list(
    wf_1 = list(
      "conservative" = "low risk",
      "moderate" = c("low risk", "some concerns"),
      "liberal" = c("low risk", "some concerns", "high risk")
    )
  )

  # Specification uses custom group name
  specs <- data.frame(
    multiverse_id = 1,
    wf_1 = "conservative",  # Custom group name
    wf_2 = "total_wf_2",
    dependency = "aggregate",
    ma_method = "fake_agg",
    stringsAsFactors = FALSE
  )

  res <- general_multiverse(
    i = 1,
    data = custom_data,
    specifications = specs,
    factor_groups = factor_groups,
    k_smallest_ma = 1
  )

  # Should only include studies with "low risk" (es_id 1-4)
  expect_equal(res$k, 4)
  expect_equal(res$set, "1,2,3,4")
})

test_that("custom factor groups handle multiple levels correctly", {
  custom_data <- create_custom_group_data()

  factor_groups <- list(
    wf_1 = list(
      "exclude_high_risk" = c("low risk", "some concerns")
    )
  )

  specs <- data.frame(
    multiverse_id = 1,
    wf_1 = "exclude_high_risk",  # Maps to two levels
    wf_2 = "total_wf_2",
    dependency = "aggregate",
    ma_method = "fake_agg",
    stringsAsFactors = FALSE
  )

  res <- general_multiverse(
    i = 1,
    data = custom_data,
    specifications = specs,
    factor_groups = factor_groups,
    k_smallest_ma = 1
  )

  # Should include studies with "low risk" OR "some concerns" (es_id 1-8)
  expect_equal(res$k, 8)
  expect_equal(res$set, "1,2,3,4,5,6,7,8")
})

test_that("custom factor groups work with multiple wf factors", {
  custom_data <- create_custom_group_data()

  factor_groups <- list(
    wf_1 = list(
      "safe_only" = "low risk"
    ),
    wf_2 = list(
      "web_only" = "website"
    )
  )

  specs <- data.frame(
    multiverse_id = 1,
    wf_1 = "safe_only",
    wf_2 = "web_only",
    dependency = "aggregate",
    ma_method = "fake_agg",
    stringsAsFactors = FALSE
  )

  res <- general_multiverse(
    i = 1,
    data = custom_data,
    specifications = specs,
    factor_groups = factor_groups,
    k_smallest_ma = 1
  )

  # Should include studies with "low risk" AND "website" (es_id 1,3)
  expect_equal(res$k, 2)
  expect_equal(res$set, "1,3")
})

test_that("falls back to original behavior when group name not found", {
  custom_data <- create_custom_group_data()

  factor_groups <- list(
    wf_1 = list(
      "conservative" = "low risk"
    )
  )

  # Use actual data level, not group name
  specs <- data.frame(
    multiverse_id = 1,
    wf_1 = "some concerns",  # Raw data level, not in custom groups
    wf_2 = "total_wf_2",
    dependency = "aggregate",
    ma_method = "fake_agg",
    stringsAsFactors = FALSE
  )

  res <- general_multiverse(
    i = 1,
    data = custom_data,
    specifications = specs,
    factor_groups = factor_groups,
    k_smallest_ma = 1
  )

  # Should fall back to direct matching: "some concerns" (es_id 5-8)
  expect_equal(res$k, 4)
  expect_equal(res$set, "5,6,7,8")
})

test_that("works without factor_groups (backward compatibility)", {
  custom_data <- create_custom_group_data()

  specs <- data.frame(
    multiverse_id = 1,
    wf_1 = "low risk",  # Raw data level
    wf_2 = "website",
    dependency = "aggregate",
    ma_method = "fake_agg",
    stringsAsFactors = FALSE
  )

  res <- general_multiverse(
    i = 1,
    data = custom_data,
    specifications = specs,
    factor_groups = NULL,  # No custom groups
    k_smallest_ma = 1
  )

  # Should work with original behavior
  expect_equal(res$k, 2)
  expect_equal(res$set, "1,3")
})

test_that("handles total_ prefix correctly with custom groups", {
  custom_data <- create_custom_group_data()

  factor_groups <- list(
    wf_1 = list("conservative" = "low risk")
  )

  specs <- data.frame(
    multiverse_id = 1,
    wf_1 = "total_wf_1",  # Should include all levels, ignore custom groups
    wf_2 = "website",
    dependency = "aggregate",
    ma_method = "fake_agg",
    stringsAsFactors = FALSE
  )

  res <- general_multiverse(
    i = 1,
    data = custom_data,
    specifications = specs,
    factor_groups = factor_groups,
    k_smallest_ma = 1
  )

  # Should include all wf_1 levels but only website for wf_2
  expect_equal(res$k, 6)  # All studies with website
  expect_equal(res$set, "1,3,5,7,9,11")
})

test_that("custom groups work with different dependency methods", {
  custom_data <- create_custom_group_data()

  factor_groups <- list(
    wf_1 = list("safe_studies" = "low risk")
  )

  # Test with select_max
  specs_select <- data.frame(
    multiverse_id = 1,
    wf_1 = "safe_studies",
    wf_2 = "total_wf_2",
    dependency = "select_max",
    ma_method = "fake_select",
    stringsAsFactors = FALSE
  )

  res_select <- general_multiverse(
    i = 1,
    data = custom_data,
    specifications = specs_select,
    factor_groups = factor_groups,
    k_smallest_ma = 1
  )

  expect_equal(res_select$b, 10)  # fake_select returns 10
  expect_equal(res_select$k, 4)   # 4 low risk studies

  # Test with modeled
  specs_modeled <- data.frame(
    multiverse_id = 1,
    wf_1 = "safe_studies",
    wf_2 = "total_wf_2",
    dependency = "modeled",
    ma_method = "fake_mod",
    stringsAsFactors = FALSE
  )

  res_modeled <- general_multiverse(
    i = 1,
    data = custom_data,
    specifications = specs_modeled,
    factor_groups = factor_groups,
    k_smallest_ma = 1
  )

  expect_equal(res_modeled$b, 30)  # fake_mod returns 30
  expect_equal(res_modeled$k, 4)   # 4 low risk studies
})

test_that("custom groups handle empty result sets correctly", {
  custom_data <- create_custom_group_data()

  factor_groups <- list(
    wf_1 = list("nonexistent" = "nonexistent_level")
  )

  specs <- data.frame(
    multiverse_id = 1,
    wf_1 = "nonexistent",
    wf_2 = "total_wf_2",
    dependency = "aggregate",
    ma_method = "fake_agg",
    stringsAsFactors = FALSE
  )

  # Should return NULL due to insufficient studies
  expect_warning(
    res <- general_multiverse(
      i = 1,
      data = custom_data,
      specifications = specs,
      factor_groups = factor_groups,
      k_smallest_ma = 1
    ),
    "Specification 1 skipped.*0 unique studies found"
  )

  expect_null(res)
})

test_that("custom groups preserve specification values in output", {
  custom_data <- create_custom_group_data()

  factor_groups <- list(
    wf_1 = list("conservative" = "low risk")
  )

  specs <- data.frame(
    multiverse_id = "test_id",
    wf_1 = "conservative",
    wf_2 = "website",
    dependency = "aggregate",
    ma_method = "fake_agg",
    extra_column = "test_value",
    stringsAsFactors = FALSE
  )

  res <- general_multiverse(
    i = 1,
    data = custom_data,
    specifications = specs,
    factor_groups = factor_groups,
    k_smallest_ma = 1
  )

  # Should preserve all specification values in output
  expect_equal(res$multiverse_id, "test_id")
  expect_equal(res$wf_1, "conservative")  # Group name, not data level
  expect_equal(res$wf_2, "website")
  expect_equal(res$dependency, "aggregate")
  expect_equal(res$ma_method, "fake_agg")
  expect_equal(res$extra_column, "test_value")
})

test_that("validates k_smallest_ma parameter", {
  expect_error(
    general_multiverse(
      i = 1,
      data = fake_data,
      specifications = base_specs[1, , drop = FALSE],
      k_smallest_ma = -1
    ),
    "k_smallest_ma.*must be a positive numeric value"
  )

  expect_error(
    general_multiverse(
      i = 1,
      data = fake_data,
      specifications = base_specs[1, , drop = FALSE],
      k_smallest_ma = "invalid"
    ),
    "k_smallest_ma.*must be a positive numeric value"
  )
})
