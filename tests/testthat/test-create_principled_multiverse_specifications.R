## tests/testthat/test-create_principled_multiverse_specifications.R
## ---------------------------------------------------------------

# Helper function to create valid test data
create_test_data <- function() {
  data.frame(
    study = paste0("Study_", 1:8),
    es_id = 1:8,
    yi = c(0.3, 0.5, 0.7, 0.2, 0.6, 0.4, 0.8, 0.1),
    vi = c(0.02, 0.03, 0.01, 0.04, 0.02, 0.03, 0.01, 0.05),
    wf_1 = c("A", "B", "A", "C", "B", "C", "A", "B"),  # Population type
    wf_2 = c("X", "Y", "X", "Y", "X", "Y", "X", "Y"),  # Measure type
    wf_3 = c("high", "low", "medium", "high", "low", "medium", "high", "low"),
    stringsAsFactors = FALSE
  )
}

# Helper to create validated data
create_validated_data <- function() {
  data <- create_test_data()
  # Simulate validation by adding attributes
  attr(data, "multiverse_validated") <- TRUE
  attr(data, "validation_timestamp") <- Sys.time()
  data
}

# Test successful specification creation
test_that("valid inputs create specifications correctly", {
  validated_data <- create_validated_data()
  decision_map <- c("wf_1" = "N", "wf_2" = "U")

  specs <- metaMultiverse::create_principled_multiverse_specifications(
    data = validated_data,
    wf_vars = c("wf_1", "wf_2"),
    ma_methods = c("reml", "fe"),
    dependencies = c("aggregate"),
    decision_map = decision_map
  )

  # Test return structure
  expect_type(specs, "list")
  expect_named(specs, c("specifications", "number_specs"))
  expect_s3_class(specs$specifications, "data.frame")
  expect_type(specs$number_specs, "integer")

  # Test specifications content
  expect_true(nrow(specs$specifications) > 0)
  expect_equal(specs$number_specs, nrow(specs$specifications))

  # Test required columns
  required_cols <- c("wf_1", "wf_2", "dependency", "ma_method", "multiverse_id", "row_id")
  expect_true(all(required_cols %in% colnames(specs$specifications)))

  # Test unique row_ids
  expect_equal(length(unique(specs$specifications$row_id)), nrow(specs$specifications))
})

# Test validation dependency enforcement
test_that("unvalidated data triggers error", {
  unvalidated_data <- create_test_data()  # No validation attributes
  decision_map <- c("wf_1" = "N", "wf_2" = "U")

  expect_error(
    metaMultiverse::create_principled_multiverse_specifications(
      data = unvalidated_data,
      wf_vars = c("wf_1", "wf_2"),
      ma_methods = c("reml", "fe"),
      dependencies = c("aggregate"),
      decision_map = decision_map
    ),
    regexp = "Data must be validated first"
  )
})

# Test decision_map parameter validation
test_that("decision_map validation works correctly", {
  validated_data <- create_validated_data()

  # Missing wf_var in decision_map
  expect_error(
    metaMultiverse::create_principled_multiverse_specifications(
      data = validated_data,
      wf_vars = c("wf_1", "wf_2"),
      ma_methods = c("reml"),
      dependencies = c("aggregate"),
      decision_map = c("wf_1" = "N")  # Missing wf_2
    ),
    regexp = "decision_map.*must contain entries for all wf_vars.*wf_2"
  )

  # Invalid decision_map values
  expect_error(
    metaMultiverse::create_principled_multiverse_specifications(
      data = validated_data,
      wf_vars = c("wf_1", "wf_2"),
      ma_methods = c("reml"),
      dependencies = c("aggregate"),
      decision_map = c("wf_1" = "INVALID", "wf_2" = "U")
    ),
    regexp = "decision_map.*values must be.*E.*U.*N.*Found.*INVALID"
  )
})

# Test wf_vars validation
test_that("wf_vars validation works correctly", {
  validated_data <- create_validated_data()

  # Empty wf_vars
  expect_error(
    metaMultiverse::create_principled_multiverse_specifications(
      data = validated_data,
      wf_vars = character(0),
      ma_methods = c("reml"),
      dependencies = c("aggregate"),
      decision_map = c()
    ),
    regexp = "wf_vars.*cannot be empty"
  )

  # Missing wf_vars in data
  expect_error(
    metaMultiverse::create_principled_multiverse_specifications(
      data = validated_data,
      wf_vars = c("wf_1", "wf_nonexistent"),
      ma_methods = c("reml"),
      dependencies = c("aggregate"),
      decision_map = c("wf_1" = "N", "wf_nonexistent" = "U")
    ),
    regexp = "wf_vars not found in data.*wf_nonexistent"
  )
})

# Test ma_methods validation
test_that("ma_methods validation works correctly", {
  validated_data <- create_validated_data()
  decision_map <- c("wf_1" = "N")

  # Empty ma_methods
  expect_error(
    metaMultiverse::create_principled_multiverse_specifications(
      data = validated_data,
      wf_vars = c("wf_1"),
      ma_methods = character(0),
      dependencies = c("aggregate"),
      decision_map = decision_map
    ),
    regexp = "need at least one ma_method"
  )

  # Invalid ma_methods (assuming registry exists)
  expect_error(
    metaMultiverse::create_principled_multiverse_specifications(
      data = validated_data,
      wf_vars = c("wf_1"),
      ma_methods = c("invalid_method"),
      dependencies = c("aggregate"),
      decision_map = decision_map
    ),
    regexp = "Invalid ma_methods.*invalid_method"
  )
})

# Test dependencies parameter validation
test_that("dependencies validation works correctly", {
  validated_data <- create_validated_data()
  decision_map <- c("wf_1" = "N")

  # Empty dependencies
  expect_error(
    metaMultiverse::create_principled_multiverse_specifications(
      data = validated_data,
      wf_vars = c("wf_1"),
      ma_methods = c("reml"),
      dependencies = character(0),
      decision_map = decision_map
    ),
    regexp = "need at least one dependency"
  )
})

# Test multiverse_id logic for different decision types
test_that("multiverse_id creation works correctly", {
  validated_data <- create_validated_data()

  # Test N-type factors create separate multiverse_ids
  decision_map_N <- c("wf_1" = "N", "wf_2" = "U")
  specs_N <- metaMultiverse::create_principled_multiverse_specifications(
    data = validated_data,
    wf_vars = c("wf_1", "wf_2"),
    ma_methods = c("reml"),
    dependencies = c("aggregate"),
    decision_map = decision_map_N
  )

  # Should have multiple multiverse_ids (one for each wf_1 level: A, B, C)
  unique_ids <- unique(specs_N$specifications$multiverse_id)
  expect_true(length(unique_ids) > 1)
  expect_true(all(unique_ids %in% c("A", "B", "C")))

  # Test no N-type factors creates single multiverse_id
  decision_map_EU <- c("wf_1" = "E", "wf_2" = "U")
  specs_EU <- metaMultiverse::create_principled_multiverse_specifications(
    data = validated_data,
    wf_vars = c("wf_1", "wf_2"),
    ma_methods = c("reml"),
    dependencies = c("aggregate"),
    decision_map = decision_map_EU
  )

  # Should have single multiverse_id
  unique_ids_EU <- unique(specs_EU$specifications$multiverse_id)
  expect_equal(length(unique_ids_EU), 1)
  expect_equal(unique_ids_EU, "all")
})

# Test E/U vs N factor behavior for wf_factors creation
test_that("E/U factors get total_ options, N factors do not", {
  validated_data <- create_validated_data()
  decision_map <- c("wf_1" = "N", "wf_2" = "U", "wf_3" = "E")

  specs <- metaMultiverse::create_principled_multiverse_specifications(
    data = validated_data,
    wf_vars = c("wf_1", "wf_2", "wf_3"),
    ma_methods = c("reml"),
    dependencies = c("aggregate"),
    decision_map = decision_map
  )

  # N-type factor (wf_1) should NOT have total_ options
  wf_1_values <- unique(specs$specifications$wf_1)
  expect_false(any(grepl("total_wf_1", wf_1_values)))
  expect_true(all(wf_1_values %in% c("A", "B", "C")))

  # U-type factor (wf_2) should have total_ option
  wf_2_values <- unique(specs$specifications$wf_2)
  expect_true("total_wf_2" %in% wf_2_values)
  expect_true(all(c("X", "Y", "total_wf_2") %in% wf_2_values))

  # E-type factor (wf_3) should have total_ option
  wf_3_values <- unique(specs$specifications$wf_3)
  expect_true("total_wf_3" %in% wf_3_values)
  expect_true("total_wf_3" %in% wf_3_values)
})

# Test method-dependency filtering
test_that("invalid method-dependency combinations are filtered out", {
  validated_data <- create_validated_data()
  decision_map <- c("wf_1" = "U")

  # This test assumes some methods don't support all dependencies
  # You may need to adjust based on your actual registry
  specs <- metaMultiverse::create_principled_multiverse_specifications(
    data = validated_data,
    wf_vars = c("wf_1"),
    ma_methods = c("reml", "3-level"),  # Assume 3-level only works with "modeled"
    dependencies = c("aggregate", "modeled"),
    decision_map = decision_map
  )

  # Should only have valid combinations
  combinations <- unique(specs$specifications[, c("ma_method", "dependency")])

  # All combinations should be valid according to registry
  for (i in 1:nrow(combinations)) {
    method <- combinations$ma_method[i]
    dep <- combinations$dependency[i]
    registry_deps <- .ma_method_registry[[method]]$deps
    expect_true(dep %in% registry_deps)
  }
})

# Test edge cases
test_that("edge cases are handled correctly", {
  # Data with NA values in wf variables
  data_with_na <- create_validated_data()
  data_with_na$wf_1[1] <- NA

  expect_warning(
    specs <- metaMultiverse::create_principled_multiverse_specifications(
      data = data_with_na,
      wf_vars = c("wf_1"),
      ma_methods = c("reml"),
      dependencies = c("aggregate"),
      decision_map = c("wf_1" = "U")
    ),
    regexp = "contains NA values"
  )

  # Should still create specifications
  expect_true(nrow(specs$specifications) > 0)
})

# Test return value structure and content
test_that("return value has correct structure and content", {
  validated_data <- create_validated_data()
  decision_map <- c("wf_1" = "N", "wf_2" = "U")

  specs <- metaMultiverse::create_principled_multiverse_specifications(
    data = validated_data,
    wf_vars = c("wf_1", "wf_2"),
    ma_methods = c("reml", "fe"),
    dependencies = c("aggregate"),
    decision_map = decision_map
  )

  # Test specifications data frame structure
  expect_true(all(c("wf_1", "wf_2") %in% colnames(specs$specifications)))
  expect_true(all(c("dependency", "ma_method", "multiverse_id", "row_id") %in% colnames(specs$specifications)))

  # Test that row_id starts at 1 and is sequential
  expect_equal(min(specs$specifications$row_id), 1)
  expect_equal(max(specs$specifications$row_id), nrow(specs$specifications))
  expect_equal(length(unique(specs$specifications$row_id)), nrow(specs$specifications))

  # Test that number_specs matches actual rows
  expect_equal(specs$number_specs, nrow(specs$specifications))

  # Test that all specified methods and dependencies appear
  expect_true(all(c("reml", "fe") %in% specs$specifications$ma_method))
  expect_true(all("aggregate" %in% specs$specifications$dependency))
})

# Test informative messages
test_that("informative messages are displayed", {
  validated_data <- create_validated_data()
  decision_map <- c("wf_1" = "N", "wf_2" = "U")

  expect_message(
    specs <- metaMultiverse::create_principled_multiverse_specifications(
      data = validated_data,
      wf_vars = c("wf_1", "wf_2"),
      ma_methods = c("reml"),
      dependencies = c("aggregate"),
      decision_map = decision_map
    ),
    regexp = "Created .* specifications across .* multiverse"
  )

  expect_message(
    specs <- metaMultiverse::create_principled_multiverse_specifications(
      data = validated_data,
      wf_vars = c("wf_1", "wf_2"),
      ma_methods = c("reml"),
      dependencies = c("aggregate"),
      decision_map = decision_map
    ),
    regexp = "Non-equivalent factors.*wf_1.*created .* separate multiverse analyses"
  )
})
