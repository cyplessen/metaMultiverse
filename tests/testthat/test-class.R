test_that("summary.multiverse_result returns 1×5 tibble", {
  x  <- new_multiverse_result(.5, .2, .8, .03)
  attr(x, "method") <- "dummy"
  s <- summary(x)
  expect_s3_class(s, "tbl_df")
  expect_equal(nrow(s), 1)
  expect_named(s, c("b", "ci.lb", "ci.ub", "pval", "method"))
})
