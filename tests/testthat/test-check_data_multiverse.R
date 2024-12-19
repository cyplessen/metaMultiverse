test_that("check_data_multiverse validates input data", {
  # Mock correct data with appropriate column types
  valid_data <- data.frame(
    study = as.character(c("Study 1", "Study 2")),
    es_id = as.numeric(1:2),
    yi = as.numeric(c(0.5, 0.6)),
    vi = as.numeric(c(0.02, 0.03)),
    sei = as.numeric(c(0.14, 0.17)),
    wf_1 = as.character(c("A", "B"))
  )

  # Expect TRUE for valid input
  expect_true(check_data_multiverse(valid_data))

  # Missing required columns
  invalid_data <- valid_data[, -1]
  expect_error(check_data_multiverse(invalid_data), "Missing required columns")
})
