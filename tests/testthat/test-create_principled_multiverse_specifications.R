# tests/testthat/test-create-principled-multiverse-specifications.R

library(testthat)

# Helper to create validated test data
create_validated_test_data <- function() {
  test_data <- data.frame(
    study = paste0("Study_", 1:12),
    es_id = 1:12,
    yi = rnorm(12, 0.5, 0.3),
    vi = runif(12, 0.01, 0.05),
    wf_1 = sample(c("low risk", "some concerns", "high risk"), 12, replace = TRUE),
    wf_2 = sample(c("website", "mobile", "tablet"), 12, replace = TRUE),
    wf_3 = sample(c("adults", "children"), 12, replace = TRUE),
    stringsAsFactors = FALSE
  )

  # Add validation attribute
  attr(test_data, "multiverse_validated") <- TRUE
  return(test_data)
}

test_that("create_principled_multiverse_specifications handles simple factors without custom groups", {
  test_data <- create_validated_test_data()
  decision_map <- c("wf_1" = "U", "wf_2" = "N", "wf_3" = "E")

  specs <- create_principled_multiverse_specifications(
    data = test_data,
    wf_vars = c("wf_1", "wf_2", "wf_3"),
    ma_methods = c("reml", "fe"),
    dependencies = c("aggregate"),
    decision_map = decision_map,
    factor_groups = NULL
  )

  expect_type(specs, "list")
  expect_named(specs, c("specifications", "number_specs"))

  # Check specifications structure
  expect_s3_class(specs$specifications, "data.frame")
  expect_true("wf_1" %in% names(specs$specifications))
  expect_true("wf_2" %in% names(specs$specifications))
  expect_true("wf_3" %in% names(specs$specifications))
  expect_true("ma_method" %in% names(specs$specifications))
  expect_true("dependency" %in% names(specs$specifications))
  expect_true("multiverse_id" %in% names(specs$specifications))

  # Check that wf_1 (U type) includes total option
  wf_1_values <- unique(specs$specifications$wf_1)
  expect_true("total_wf_1" %in% wf_1_values)
  expect_true(all(c("low risk", "some concerns", "high risk") %in% wf_1_values))

  # Check that wf_2 (N type) doesn't include total option
  wf_2_values <- unique(specs$specifications$wf_2)
  expect_false(any(grepl("^total_", wf_2_values)))
  expect_true(all(c("website", "mobile", "tablet") %in% wf_2_values))

  # Check that wf_3 (E type) includes total option
  wf_3_values <- unique(specs$specifications$wf_3)
  expect_true("total_wf_3" %in% wf_3_values)
  expect_true(all(c("adults", "children") %in% wf_3_values))

  # Check multiverse_id creation (based on N-type factors)
  multiverse_ids <- unique(specs$specifications$multiverse_id)
  expect_equal(sort(multiverse_ids), sort(c("website", "mobile", "tablet")))
})

test_that("create_principled_multiverse_specifications handles custom factor groups", {
  test_data <- create_validated_test_data()

  # Create custom factor groups
  factor_groups <- list(
    wf_1 = list(
      "conservative" = "low risk",
      "moderate" = c("low risk", "some concerns"),
      "liberal" = c("low risk", "some concerns", "high risk")
    ),
    wf_2 = list(
      "mobile_only" = "mobile",
      "web_mobile" = c("website", "mobile"),
      "all_tech" = c("website", "mobile", "tablet")
    )
  )

  decision_map <- c("wf_1" = "N", "wf_2" = "U")

  specs <- create_principled_multiverse_specifications(
    data = test_data,
    wf_vars = c("wf_1", "wf_2"),
    ma_methods = c("reml"),
    dependencies = c("aggregate"),
    decision_map = decision_map,
    factor_groups = factor_groups
  )

  # Check that specifications use custom group names, not data levels
  wf_1_values <- unique(specs$specifications$wf_1)
  expect_true(all(c("conservative", "moderate", "liberal") %in% wf_1_values))
  expect_false(any(c("low risk", "some concerns", "high risk") %in% wf_1_values))

  wf_2_values <- unique(specs$specifications$wf_2)
  expect_true(all(c("mobile_only", "web_mobile", "all_tech") %in% wf_2_values))
  expect_false(any(c("website", "mobile", "tablet") %in% wf_2_values))

  # Check factor_groups stored as attribute
  expect_equal(attr(specs$specifications, "factor_groups"), factor_groups)

  # Check multiverse_id uses custom group names (wf_1 is N-type)
  multiverse_ids <- unique(specs$specifications$multiverse_id)
  expect_equal(sort(multiverse_ids), sort(c("conservative", "moderate", "liberal")))
})

test_that("create_principled_multiverse_specifications handles mixed simple and custom factors", {
  test_data <- create_validated_test_data()

  # Mix of custom groups and simple factors
  factor_groups <- list(
    wf_1 = list(
      "low_risk_only" = "low risk",
      "exclude_high_risk" = c("low risk", "some concerns")
    )
    # wf_2 has no custom groups - should use original data levels
  )

  decision_map <- c("wf_1" = "U", "wf_2" = "U")

  specs <- create_principled_multiverse_specifications(
    data = test_data,
    wf_vars = c("wf_1", "wf_2"),
    ma_methods = c("reml"),
    dependencies = c("aggregate"),
    decision_map = decision_map,
    factor_groups = factor_groups
  )

  # wf_1 should use custom groups
  wf_1_values <- unique(specs$specifications$wf_1)
  expect_true(all(c("low_risk_only", "exclude_high_risk") %in% wf_1_values))
  expect_false(any(c("low risk", "some concerns", "high risk") %in% wf_1_values))

  # wf_2 should use original data levels (no custom groups) + total option
  wf_2_values <- unique(specs$specifications$wf_2)
  expect_true(all(c("website", "mobile", "tablet", "total_wf_2") %in% wf_2_values))
})

test_that("create_principled_multiverse_specifications validates custom factor groups", {
  test_data <- create_validated_test_data()
  decision_map <- c("wf_1" = "U")

  # Test with empty factor_groups
  factor_groups_empty <- list(
    wf_1 = list()  # Empty groups
  )

  expect_error(
    create_principled_multiverse_specifications(
      data = test_data,
      wf_vars = "wf_1",
      ma_methods = "reml",
      dependencies = "aggregate",
      decision_map = decision_map,
      factor_groups = factor_groups_empty
    ),
    "empty custom groups"
  )
})

test_that("create_principled_multiverse_specifications validates inputs", {
  test_data <- create_validated_test_data()
  decision_map <- c("wf_1" = "U")

  # Test unvalidated data
  unvalidated_data <- test_data
  attr(unvalidated_data, "multiverse_validated") <- NULL
  expect_error(
    create_principled_multiverse_specifications(
      data = unvalidated_data,
      wf_vars = "wf_1",
      ma_methods = "reml",
      dependencies = "aggregate",
      decision_map = decision_map
    ),
    "Data must be validated first"
  )

  # Test missing wf_vars
  expect_error(
    create_principled_multiverse_specifications(
      data = test_data,
      wf_vars = "nonexistent_wf",
      ma_methods = "reml",
      dependencies = "aggregate",
      decision_map = c("nonexistent_wf" = "U")
    ),
    "wf_vars not found in data"
  )

  # Test invalid decision_map
  expect_error(
    create_principled_multiverse_specifications(
      data = test_data,
      wf_vars = "wf_1",
      ma_methods = "reml",
      dependencies = "aggregate",
      decision_map = c("wf_1" = "invalid")
    ),
    "decision_map.*values must be.*E.*U.*N"
  )

  # Test empty methods
  expect_error(
    create_principled_multiverse_specifications(
      data = test_data,
      wf_vars = "wf_1",
      ma_methods = character(0),
      dependencies = "aggregate",
      decision_map = decision_map
    ),
    "need at least one ma_method"
  )
})

test_that("create_principled_multiverse_specifications handles multiverse_id creation correctly", {
  test_data <- create_validated_test_data()

  # Test with custom groups and N-type factor
  factor_groups <- list(
    wf_1 = list(
      "group_A" = "low risk",
      "group_B" = c("some concerns", "high risk")
    ),
    wf_2 = list(
      "tech_1" = "website",
      "tech_2" = c("mobile", "tablet")
    )
  )

  # Both factors N-type - should create multiverse_id combinations
  decision_map <- c("wf_1" = "N", "wf_2" = "N")

  specs <- create_principled_multiverse_specifications(
    data = test_data,
    wf_vars = c("wf_1", "wf_2"),
    ma_methods = c("reml"),
    dependencies = c("aggregate"),
    decision_map = decision_map,
    factor_groups = factor_groups
  )

  # multiverse_id should be combinations of custom group names
  multiverse_ids <- unique(specs$specifications$multiverse_id)
  expected_combinations <- c("group_A|tech_1", "group_A|tech_2", "group_B|tech_1", "group_B|tech_2")
  expect_equal(sort(multiverse_ids), sort(expected_combinations))
})

test_that("create_principled_multiverse_specifications preserves factor_groups as attribute", {
  test_data <- create_validated_test_data()

  factor_groups <- list(
    wf_1 = list(
      "safe_studies" = "low risk",
      "all_studies" = c("low risk", "some concerns", "high risk")
    )
  )

  decision_map <- c("wf_1" = "U")

  specs <- create_principled_multiverse_specifications(
    data = test_data,
    wf_vars = "wf_1",
    ma_methods = "reml",
    dependencies = "aggregate",
    decision_map = decision_map,
    factor_groups = factor_groups
  )

  # factor_groups should be stored as attribute
  stored_groups <- attr(specs$specifications, "factor_groups")
  expect_equal(stored_groups, factor_groups)
})

test_that("create_principled_multiverse_specifications messages include custom group info", {
  test_data <- create_validated_test_data()

  factor_groups <- list(
    wf_1 = list("group1" = "low risk")
  )

  decision_map <- c("wf_1" = "U")

  expect_message(
    specs <- create_principled_multiverse_specifications(
      data = test_data,
      wf_vars = "wf_1",
      ma_methods = "reml",
      dependencies = "aggregate",
      decision_map = decision_map,
      factor_groups = factor_groups
    ),
    "Using custom factor groupings for 1 factor"
  )
})

test_that("create_principled_multiverse_specifications returns correct number_specs", {
  test_data <- create_validated_test_data()
  decision_map <- c("wf_1" = "U")

  specs <- create_principled_multiverse_specifications(
    data = test_data,
    wf_vars = "wf_1",
    ma_methods = c("reml", "fe"),
    dependencies = c("aggregate"),
    decision_map = decision_map,
    factor_groups = NULL
  )

  expect_equal(specs$number_specs, nrow(specs$specifications))
  expect_type(specs$number_specs, "integer")
})

test_that("create_principled_multiverse_specifications preserves row_id uniqueness", {
  test_data <- create_validated_test_data()
  decision_map <- c("wf_1" = "U", "wf_2" = "U")

  specs <- create_principled_multiverse_specifications(
    data = test_data,
    wf_vars = c("wf_1", "wf_2"),
    ma_methods = c("reml", "fe"),
    dependencies = c("aggregate"),
    decision_map = decision_map,
    factor_groups = NULL
  )

  # All row_ids should be unique
  expect_equal(length(unique(specs$specifications$row_id)), nrow(specs$specifications))

  # row_ids should be sequential from 1
  expect_equal(sort(specs$specifications$row_id), 1:nrow(specs$specifications))
})
