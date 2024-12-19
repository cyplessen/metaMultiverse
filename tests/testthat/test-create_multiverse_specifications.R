test_that("create_multiverse_specifications generates a specifications grid", {
  # Mock data
  data <- data.frame(
    wf_1 = c("A", "B"),
    wf_2 = c("X", "Y")
  )
  wf_vars <- c("wf_1", "wf_2")
  ma_methods <- c("reml", "fe")
  dependencies <- c("aggregate", "modeled")

  # Run the function
  result <- create_multiverse_specifications(data, wf_vars, ma_methods, dependencies)

  # Expectations
  expect_s3_class(result$specifications, "data.frame")
  expect_true("ma_method" %in% colnames(result$specifications))
  expect_true(nrow(result$specifications) > 0)
})
