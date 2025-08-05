# tests/testthat/test-define-factors.R

library(testthat)

# Test data setup
create_test_data <- function() {
  data.frame(
    study = paste0("Study_", 1:12),
    es_id = 1:12,
    yi = rnorm(12, 0.5, 0.3),
    vi = runif(12, 0.01, 0.05),
    risk_of_bias = sample(c("low risk", "some concerns", "high risk"), 12, replace = TRUE),
    technology_type = sample(c("website", "mobile", "tablet"), 12, replace = TRUE),
    population = sample(c("adults", "children"), 12, replace = TRUE),
    study_design = sample(c("RCT", "observational"), 12, replace = TRUE),
    quality = sample(c("high", "medium", "low"), 12, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

test_that("define_factors handles simple factor definitions", {
  test_data <- create_test_data()

  # Test basic simple factor
  setup <- define_factors(test_data, Population = "population")

  expect_s3_class(setup, "multiverse_factor_setup")  # Now matches your structure() call
  expect_named(setup, c("data", "factors", "decision_map", "factor_groups"))

  # Check data has wf_* columns
  expect_true("wf_1" %in% names(setup$data))
  expect_equal(setup$data$wf_1, as.character(test_data$population))

  # Check factor info
  expect_equal(nrow(setup$factors), 1)
  expect_equal(setup$factors$label[1], "Population")
  expect_equal(setup$factors$column[1], "population")
  expect_equal(setup$factors$decision[1], "U")  # Default
  expect_equal(setup$factors$grouping_type[1], "simple")

  # Check decision map
  expect_equal(setup$decision_map, c("wf_1" = "U"))

  # Check factor_groups is empty for simple factors
  expect_equal(length(setup$factor_groups), 0)
})

test_that("define_factors handles simple factors with decision types", {
  test_data <- create_test_data()

  setup <- define_factors(test_data,
                          Population = "population|N",
                          Design = "study_design|E"
  )

  expect_equal(nrow(setup$factors), 2)
  expect_equal(setup$factors$decision, c("N", "E"))
  expect_equal(setup$decision_map, c("wf_1" = "N", "wf_2" = "E"))

  # Check both wf_* columns exist
  expect_true(all(c("wf_1", "wf_2") %in% names(setup$data)))
})

test_that("define_factors handles custom group definitions", {
  test_data <- create_test_data()

  setup <- define_factors(test_data,
                          Risk_of_Bias = list("risk_of_bias", decision = "N",
                                              groups = list(
                                                "conservative" = "low risk",
                                                "moderate" = c("low risk", "some concerns"),
                                                "liberal" = c("low risk", "some concerns", "high risk")
                                              )
                          )
  )

  # Check factor info for custom groups
  expect_equal(setup$factors$grouping_type[1], "custom")
  expect_equal(setup$factors$decision[1], "N")

  # Check factor_groups structure
  expect_equal(length(setup$factor_groups), 1)
  expect_true("wf_1" %in% names(setup$factor_groups))

  groups <- setup$factor_groups$wf_1
  expect_equal(length(groups), 3)
  expect_equal(groups$conservative, "low risk")
  expect_equal(groups$moderate, c("low risk", "some concerns"))
  expect_equal(groups$liberal, c("low risk", "some concerns", "high risk"))
})

test_that("define_factors handles ordered factors", {
  test_data <- create_test_data()

  setup <- define_factors(test_data,
                          Quality = list("quality", type = "ordered", decision = "U",
                                         levels = c("high", "medium", "low")
                          )
  )

  expect_equal(setup$factors$grouping_type[1], "ordered")
  expect_equal(setup$factors$decision[1], "U")

  # Check ordered groups structure
  groups <- setup$factor_groups$wf_1
  expect_equal(length(groups), 3)
  expect_equal(groups$up_to_high, "high")
  expect_equal(groups$up_to_medium, c("high", "medium"))
  expect_equal(groups$up_to_low, c("high", "medium", "low"))
})

test_that("define_factors handles binary factors", {
  test_data <- create_test_data()

  setup <- define_factors(test_data,
                          Technology = list("technology_type", type = "binary", decision = "E")
  )

  expect_equal(setup$factors$grouping_type[1], "binary")

  # Check binary groups structure
  groups <- setup$factor_groups$wf_1
  unique_techs <- unique(test_data$technology_type)
  expected_groups <- length(unique_techs) + 1  # Each level + all_levels
  expect_equal(length(groups), expected_groups)
  expect_true("all_levels" %in% names(groups))
  expect_equal(groups$all_levels, unique_techs)
})

test_that("define_factors handles mixed factor types", {
  test_data <- create_test_data()

  setup <- define_factors(test_data,
                          Risk_of_Bias = list("risk_of_bias", decision = "N",
                                              groups = list(
                                                "safe" = "low risk",
                                                "all" = c("low risk", "some concerns", "high risk")
                                              )
                          ),
                          Technology = list("technology_type", type = "binary", decision = "U"),
                          Population = "population|E",
                          Design = "study_design"
  )

  expect_equal(nrow(setup$factors), 4)
  expect_equal(setup$factors$grouping_type, c("custom", "binary", "simple", "simple"))
  expect_equal(setup$factors$decision, c("N", "U", "E", "U"))

  # Check all wf_* columns created
  expect_true(all(paste0("wf_", 1:4) %in% names(setup$data)))

  # Check factor_groups has correct structure
  expect_equal(length(setup$factor_groups), 2)  # Only custom and binary have groups
  expect_true(all(c("wf_1", "wf_2") %in% names(setup$factor_groups)))
})

test_that("define_factors validates input data", {
  # Test non-data.frame input
  expect_error(define_factors("not a data frame"), "Please provide a data frame")

  # Test empty factor list
  test_data <- create_test_data()
  expect_error(define_factors(test_data), "Please specify at least one factor")
})

test_that("define_factors validates column existence", {
  test_data <- create_test_data()

  # Test missing column
  expect_error(
    define_factors(test_data, Missing = "nonexistent_column"),
    "Column 'nonexistent_column' not found in data"
  )
})

test_that("define_factors validates custom groups", {
  test_data <- create_test_data()

  # Test invalid group levels
  expect_error(
    define_factors(test_data,
                   Risk = list("risk_of_bias",
                               groups = list("invalid" = "nonexistent_level")
                   )
    ),
    "Levels not found in data"
  )

  # Test malformed groups
  expect_error(
    define_factors(test_data,
                   Risk = list("risk_of_bias", groups = "not a list")
    ),
    "'groups' must be a named list"
  )
})

test_that("define_factors validates decision types", {
  test_data <- create_test_data()

  # Test invalid decision type in simple factor - should error on decision type
  expect_error(
    define_factors(test_data, Population = "population|X"),
    "Decision type must be 'E', 'U', or 'N'.*Found.*X"
  )

  # Test invalid decision type in advanced factor
  expect_error(
    define_factors(test_data,
                   Risk = list("risk_of_bias", decision = "invalid")
    ),
    "Decision type must be 'E', 'U', or 'N'"
  )
})

test_that("define_factors validates ordered factors", {
  test_data <- create_test_data()

  # Test ordered factor with missing levels
  expect_error(
    define_factors(test_data,
                   Quality = list("quality", type = "ordered",
                                  levels = c("high", "nonexistent")
                   )
    ),
    "Ordered levels not found in data"
  )
})

test_that("define_factors preserves original data", {
  test_data <- create_test_data()
  original_names <- names(test_data)
  original_nrow <- nrow(test_data)

  setup <- define_factors(test_data, Population = "population")

  # Original columns should still exist
  expect_true(all(original_names %in% names(setup$data)))
  expect_equal(nrow(setup$data), original_nrow)

  # Original data should be unchanged
  for (col in original_names) {
    expect_equal(setup$data[[col]], test_data[[col]])
  }
})

test_that("define_factors creates correct wf_* column mappings", {
  test_data <- create_test_data()

  setup <- define_factors(test_data,
                          Pop = "population",
                          Tech = "technology_type",
                          Risk = "risk_of_bias"
  )

  # Check wf_* columns match original data
  expect_equal(setup$data$wf_1, as.character(test_data$population))
  expect_equal(setup$data$wf_2, as.character(test_data$technology_type))
  expect_equal(setup$data$wf_3, as.character(test_data$risk_of_bias))

  # Check factor info matches
  expect_equal(setup$factors$column, c("population", "technology_type", "risk_of_bias"))
  expect_equal(setup$factors$wf_internal, c("wf_1", "wf_2", "wf_3"))
})

test_that("define_factors handles factors with default decision type", {
  test_data <- create_test_data()

  setup <- define_factors(test_data,
                          Risk = list("risk_of_bias",
                                      groups = list("safe" = "low risk")
                          )  # No decision specified
  )

  # Should default to "U"
  expect_equal(setup$factors$decision[1], "U")
  expect_equal(setup$decision_map, c("wf_1" = "U"))
})
