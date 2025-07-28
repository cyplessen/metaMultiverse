## tests/testthat/test-check_data_multiverse.R
## -------------------------------------------

core_cols <- c("study","es_id","yi","vi")

test_that("valid data passes and returns TRUE", {
  expect_message(                       # prints “Data check passed…”
    ok <- metaMultiverse::check_data_multiverse(data_tiny),
    regexp = "Dataset is valid"
  )
  expect_true(ok)
})

test_that("missing required column triggers error", {
  bad <- data_tiny |> dplyr::select(-yi)     # drop one core column
  expect_error(
    metaMultiverse::check_data_multiverse(bad),
    regexp = "Missing required columns"
  )
})

test_that("wrong data type triggers error", {
  bad <- data_tiny
  bad$es_id <- as.character(bad$es_id)       # should be numeric
  expect_error(
    metaMultiverse::check_data_multiverse(bad),
    regexp = "incorrect.*type"
  )
})

test_that("duplicate es_id triggers error", {
  bad <- data_tiny
  bad$es_id[1:2] <- bad$es_id[1]             # make a duplicate
  expect_error(
    metaMultiverse::check_data_multiverse(bad),
    regexp = "Duplicate values"
  )
})

test_that("missing values issue a warning but still return TRUE", {
  bad <- data_tiny
  bad$yi[3] <- NA
  expect_warning(
    ok <- metaMultiverse::check_data_multiverse(bad),
    regexp = "missing values"
  )
  expect_true(ok)
})
