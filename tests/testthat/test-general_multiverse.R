test_that("general_multiverse runs meta-analysis for a given specification", {
  # Mock data with at least 5 unique studies
  data_multiverse <- data.frame(
    study = rep(paste0("S", 1:5), each = 2),
    es_id = 1:10,
    yi = rnorm(10, mean = 0.5),
    vi = runif(10, min = 0.01, max = 0.1),
    wf_1 = rep(c("A", "B"), times = 5)
  )
  specifications <- data.frame(
    wf_1 = c("A", "B"),
    dependency = "aggregate",
    ma_method = "reml"
  )

  # Run function
  result <- general_multiverse(1, data_multiverse, specifications, how_methods = c("reml"))

  # Expectations
  expect_s3_class(result, "data.frame")
  expect_true("b" %in% colnames(result))
  expect_true("k" %in% colnames(result))
})
