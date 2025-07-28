## tests/testthat/test-general_multiverse.R
## -----------------------------------------

# small spec grid: one which-factor, one estimator, one dependency
specs <- metaMultiverse::create_multiverse_specifications(
  data         = data_tiny,        # <- fixture from helper-data.R
  wf_vars      = "wf_1",
  ma_methods   = "reml",
  dependencies = "aggregate"
)$specifications

test_that("general_multiverse returns one-row data.frame for a valid spec", {

  res <- metaMultiverse::general_multiverse(
    i               = 1,
    data_multiverse = data_tiny,
    specifications  = specs,
    k_smallest_ma   = 3            # low threshold, always satisfied
  )

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)

  core <- c("b","ci.lb","ci.ub","pval","k","set")
  expect_true(all(core %in% names(res)))

  expect_type(res$b,     "double")
  expect_type(res$ci.lb, "double")
  expect_type(res$ci.ub, "double")
  expect_type(res$pval,  "double")
  expect_length(res$b, 1)
})

test_that("general_multiverse returns NULL when subset is too small", {

  expect_warning(
    res_null <- metaMultiverse::general_multiverse(
      i               = 1,
      data_multiverse = data_tiny,
      specifications  = specs,
      k_smallest_ma   = 20          # impossible with only 10 rows
    ),
    regexp = "skipped"
  )

  expect_null(res_null)
})
