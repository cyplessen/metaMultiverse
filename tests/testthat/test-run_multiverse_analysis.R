test_that("run_multiverse_analysis combines multiverse results", {
  # Mock data
  data_multiverse <- data.frame(
    study = rep(paste0("S", 1:6), each = 2),
    es_id = 1:12,
    yi = rnorm(12, mean = 0.5),
    vi = runif(12, min = 0.01, max = 0.1),
    wf_1 = rep(c("A", "B"), times = 6)
  )
  specifications <- data.frame(
    wf_1 = c("A", "B"),
    dependency = "aggregate",
    ma_method = "reml"
  )

  # Run function
  result <- run_multiverse_analysis(data_multiverse, specifications, how_methods = c("reml"))

  # Expectations
  expect_s3_class(result, "data.frame")
  expect_true("b" %in% colnames(result))
  expect_true("k" %in% colnames(result))
})
