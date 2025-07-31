## tests/testthat/test-check_data_multiverse.R
## -------------------------------------------

# Helper function to create valid test data
create_valid_data <- function() {
  data.frame(
    study = c("Study A", "Study B", "Study C", "Study D"),
    es_id = 1:4,
    yi = c(0.5, -0.2, 0.8, 0.1),
    vi = c(0.02, 0.03, 0.01, 0.04),
    sei = c(0.141, 0.173, 0.100, 0.200),
    wf_1 = c("X", "Y", "X", "Y"),
    wf_2 = c("high", "low", "medium", "high"),
    stringsAsFactors = FALSE
  )
}

# Test successful validation
test_that("valid data passes and returns TRUE", {
  valid_data <- create_valid_data()
  expect_message(
    result <- metaMultiverse::check_data_multiverse(valid_data),
    regexp = "Data validation passed"
  )
  expect_true(result)
})

# Test input validation errors
test_that("non-data.frame input triggers error", {
  expect_error(
    metaMultiverse::check_data_multiverse("not a dataframe"),
    regexp = "Input 'data' must be a data.frame"
  )

  expect_error(
    metaMultiverse::check_data_multiverse(list(a = 1, b = 2)),
    regexp = "Input 'data' must be a data.frame"
  )
})

test_that("empty data triggers error", {
  empty_data <- data.frame()
  expect_error(
    metaMultiverse::check_data_multiverse(empty_data),
    regexp = "Input data is empty \\(0 rows\\)"
  )
})

# Test missing required columns
test_that("missing required columns trigger errors", {
  valid_data <- create_valid_data()

  # Missing single core column
  bad_data <- valid_data |> dplyr::select(-yi)
  expect_error(
    metaMultiverse::check_data_multiverse(bad_data),
    regexp = "Missing required columns: yi"
  )

  # Missing multiple core columns
  bad_data <- valid_data |> dplyr::select(-yi, -vi)
  expect_error(
    metaMultiverse::check_data_multiverse(bad_data),
    regexp = "Missing required columns: yi, vi"
  )

  # Note: wf columns are dynamically detected, so we can't test "missing wf_1"
  # if other wf columns exist. The function only requires wf columns that exist.
  # Let's skip this test since the logic is correct as designed.
})

# Test data type validation
test_that("incorrect data types trigger errors", {
  valid_data <- create_valid_data()

  # Wrong type for es_id (should be numeric)
  bad_data <- valid_data
  bad_data$es_id <- as.character(bad_data$es_id)
  expect_error(
    metaMultiverse::check_data_multiverse(bad_data),
    regexp = "Data type errors found.*Column 'es_id' is character but should be numeric"
  )

  # Wrong type for study (should be character)
  bad_data <- valid_data
  bad_data$study <- as.factor(bad_data$study)
  expect_error(
    metaMultiverse::check_data_multiverse(bad_data),
    regexp = "Data type errors found.*Column 'study' is factor but should be character"
  )

  # Wrong type for yi (should be numeric)
  bad_data <- valid_data
  bad_data$yi <- as.character(bad_data$yi)
  expect_error(
    metaMultiverse::check_data_multiverse(bad_data),
    regexp = "Data type errors found.*Column 'yi' is character but should be numeric"
  )

  # Wrong type for vi (should be numeric)
  bad_data <- valid_data
  bad_data$vi <- as.character(bad_data$vi)
  expect_error(
    metaMultiverse::check_data_multiverse(bad_data),
    regexp = "Data type errors found.*Column 'vi' is character but should be numeric"
  )
})

# Test wf column type validation
test_that("incorrect wf column types trigger errors", {
  valid_data <- create_valid_data()

  # wf column should be character
  bad_data <- valid_data
  bad_data$wf_1 <- as.factor(bad_data$wf_1)
  expect_error(
    metaMultiverse::check_data_multiverse(bad_data),
    regexp = "Which factor column errors.*Column 'wf_1' is factor but should be character"
  )

  # Multiple wf columns with wrong types
  bad_data <- valid_data
  bad_data$wf_1 <- as.factor(bad_data$wf_1)
  bad_data$wf_2 <- as.numeric(as.factor(bad_data$wf_2))
  expect_error(
    metaMultiverse::check_data_multiverse(bad_data),
    regexp = "Which factor column errors"
  )
})

# Test unique es_id validation
test_that("duplicate es_id triggers error", {
  valid_data <- create_valid_data()
  bad_data <- valid_data
  bad_data$es_id[2] <- bad_data$es_id[1]  # Create duplicate

  expect_error(
    metaMultiverse::check_data_multiverse(bad_data),
    regexp = "Duplicate values found in 'es_id'"
  )
})

# Test non-finite value validation
test_that("non-finite values trigger errors", {
  valid_data <- create_valid_data()

  # Inf in yi
  bad_data <- valid_data
  bad_data$yi[1] <- Inf
  expect_error(
    metaMultiverse::check_data_multiverse(bad_data),
    regexp = "Found 1 non-finite values.*in column yi"
  )

  # -Inf in vi
  bad_data <- valid_data
  bad_data$vi[1] <- -Inf
  expect_error(
    metaMultiverse::check_data_multiverse(bad_data),
    regexp = "Found 1 non-finite values.*in column vi"
  )

  # NaN in yi
  bad_data <- valid_data
  bad_data$yi[1] <- NaN
  expect_error(
    metaMultiverse::check_data_multiverse(bad_data),
    regexp = "Found 1 non-finite values.*in column yi"
  )
})

# Test variance validation
test_that("non-positive variances trigger errors", {
  valid_data <- create_valid_data()

  # Zero variance
  bad_data <- valid_data
  bad_data$vi[1] <- 0
  expect_error(
    metaMultiverse::check_data_multiverse(bad_data),
    regexp = "Found 1 non-positive variance values"
  )

  # Negative variance
  bad_data <- valid_data
  bad_data$vi[1] <- -0.01
  expect_error(
    metaMultiverse::check_data_multiverse(bad_data),
    regexp = "Found 1 non-positive variance values"
  )

  # Multiple non-positive variances
  bad_data <- valid_data
  bad_data$vi[1:2] <- c(0, -0.01)
  expect_error(
    metaMultiverse::check_data_multiverse(bad_data),
    regexp = "Found 2 non-positive variance values"
  )
})

# Test warnings for extreme values
test_that("extreme effect sizes trigger warnings but pass", {
  valid_data <- create_valid_data()
  bad_data <- valid_data
  bad_data$yi[1] <- 15  # Extreme effect size

  expect_warning(
    result <- metaMultiverse::check_data_multiverse(bad_data),
    regexp = "Found 1 effect sizes with absolute value > 10"
  )
  expect_true(result)
})

test_that("large Cohen's d triggers SD/SE confusion warning", {
  valid_data <- create_valid_data()
  bad_data <- valid_data
  bad_data$yi[1] <- 3.2  # Large Cohen's d (> 2.5)

  expect_warning(
    result <- metaMultiverse::check_data_multiverse(bad_data),
    regexp = "unreasonably large d detected.*Check if SD and SE were confused"
  )
  expect_true(result)
})

test_that("multiple large Cohen's d values are detected", {
  valid_data <- create_valid_data()
  bad_data <- valid_data
  bad_data$yi[1:2] <- c(3.0, -2.8)  # Multiple large Cohen's d

  expect_warning(
    result <- metaMultiverse::check_data_multiverse(bad_data),
    regexp = "Found 2 unreasonably large d detected"
  )
  expect_true(result)
})

test_that("extreme variances trigger warnings but pass", {
  valid_data <- create_valid_data()
  bad_data <- valid_data
  bad_data$vi[1] <- 150  # Extreme variance
  # Also need to update sei to match, or remove it to avoid consistency warning
  bad_data <- bad_data |> dplyr::select(-sei)

  expect_warning(
    result <- metaMultiverse::check_data_multiverse(bad_data),
    regexp = "Found 1 variances > 100"
  )
  expect_true(result)
})

# Test study count warnings
test_that("few studies trigger warnings", {
  # Only 2 studies
  few_studies_data <- data.frame(
    study = c("Study A", "Study B"),
    es_id = 1:2,
    yi = c(0.5, -0.2),
    vi = c(0.02, 0.03),
    wf_1 = c("X", "Y"),
    stringsAsFactors = FALSE
  )

  expect_warning(
    result <- metaMultiverse::check_data_multiverse(few_studies_data),
    regexp = "Only 2 unique studies found"
  )
  expect_true(result)
})

# Test missing value warnings
test_that("missing values trigger warnings but pass", {
  valid_data <- create_valid_data()
  bad_data <- valid_data
  # Put missing values in non-numeric columns to avoid non-finite error
  bad_data$study[1] <- NA  # Missing study name
  bad_data$wf_1[2] <- NA   # Missing wf value

  expect_warning(
    result <- metaMultiverse::check_data_multiverse(bad_data),
    regexp = "Some required columns contain missing values"
  )
  expect_true(result)
})

# Test sei/vi consistency warnings
test_that("sei/vi inconsistency triggers warnings", {
  valid_data <- create_valid_data()
  bad_data <- valid_data
  bad_data$sei[1] <- 0.5  # Should be sqrt(0.02) ≈ 0.141, but we set it to 0.5

  expect_warning(
    result <- metaMultiverse::check_data_multiverse(bad_data),
    regexp = "cases where sei and sqrt\\(vi\\) differ substantially"
  )
  expect_true(result)
})

# Test study effect size distribution messages
test_that("study effect size distribution messages work", {
  # All studies have exactly one effect size
  single_effect_data <- data.frame(
    study = c("Study A", "Study B", "Study C"),
    es_id = 1:3,
    yi = c(0.5, -0.2, 0.8),
    vi = c(0.02, 0.03, 0.01),
    wf_1 = c("X", "Y", "X"),
    stringsAsFactors = FALSE
  )

  expect_message(
    metaMultiverse::check_data_multiverse(single_effect_data),
    regexp = "All studies contribute exactly one effect size"
  )

  # Mixed: some studies have multiple effect sizes
  mixed_data <- data.frame(
    study = c("Study A", "Study A", "Study B", "Study C"),
    es_id = 1:4,
    yi = c(0.5, -0.2, 0.8, 0.1),
    vi = c(0.02, 0.03, 0.01, 0.04),
    wf_1 = c("X", "Y", "X", "Y"),
    stringsAsFactors = FALSE
  )

  expect_message(
    metaMultiverse::check_data_multiverse(mixed_data),
    regexp = "2 studies contribute only one effect size, 1 studies contribute multiple effect sizes"
  )
})

# Test edge cases
test_that("edge cases are handled correctly", {
  # Data with only required columns (no wf columns)
  minimal_data <- data.frame(
    study = c("Study A", "Study B", "Study C"),
    es_id = 1:3,
    yi = c(0.5, -0.2, 0.8),
    vi = c(0.02, 0.03, 0.01),
    stringsAsFactors = FALSE
  )

  expect_message(
    result <- metaMultiverse::check_data_multiverse(minimal_data),
    regexp = "Data validation passed"
  )
  expect_true(result)

  # Data without sei column (should not trigger sei/vi consistency check)
  no_sei_data <- create_valid_data() |> dplyr::select(-sei)
  expect_message(
    result <- metaMultiverse::check_data_multiverse(no_sei_data),
    regexp = "Data validation passed"
  )
  expect_true(result)
})

# Test multiple simultaneous issues
test_that("multiple issues are reported appropriately", {
  valid_data <- create_valid_data()

  # Create data with multiple warnings (should still pass)
  # Remove sei column to avoid consistency warnings interfering
  multi_warning_data <- valid_data |> dplyr::select(-sei)
  multi_warning_data$yi[1] <- 15      # Extreme effect size (triggers first warning)
  multi_warning_data$vi[2] <- 150     # Extreme variance (triggers second warning)
  multi_warning_data$wf_1[3] <- NA    # Missing value (triggers third warning)

  # The function will trigger multiple warnings, expect the first one
  expect_warning(
    result <- metaMultiverse::check_data_multiverse(multi_warning_data)
  )
  expect_true(result)
})
