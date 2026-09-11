# tests/testthat/test-create_multiverse_specifications.R
# Specification grid: column types

test_that("specification grid and results use character, not factor, columns", {
  data("data_digDep")

  spec_out <- suppressWarnings(check_data_multiverse(data_digDep)) %>%
    define_factors(Population = "wf_3|E", Design = "wf_1|U") %>%
    create_multiverse_specifications(
      ma_methods = c("fe", "reml"),
      dependencies = "aggregate"
    )
  specs <- spec_out$specifications

  expect_false(any(vapply(specs, is.factor, logical(1))))
  expect_type(specs$wf_1, "character")
  expect_type(specs$wf_2, "character")
  expect_type(specs$ma_method, "character")
  expect_type(specs$dependency, "character")

  results <- suppressWarnings(
    run_multiverse_analysis(spec_out, verbose = FALSE, progress = FALSE)
  )
  expect_false(any(vapply(results$results, is.factor, logical(1))))

  # indexing a named label vector by a factor column would use the integer
  # codes; with character columns the labels line up with the values
  labels <- setNames(paste("Method:", c("fe", "reml")), c("fe", "reml"))
  expect_equal(unname(labels[results$results$ma_method]),
               paste("Method:", results$results$ma_method))
})
