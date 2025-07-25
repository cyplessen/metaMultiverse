test_that("fit_pet.peese returns finite numeric scalars", {

  # skip Bayesian / slow paths if user asked to
  skip_if(getOption("metaMultiverse.skip_slow", FALSE))

  res <- metaMultiverse:::fit_pet.peese(pet_toy)   # ::: calls internal fn

  # structure checks
  expect_type(res, "list")
  expect_named(res, c("b", "ci.lb", "ci.ub", "pval", "type"))

  # value checks
  expect_length(res$b,     1)
  expect_length(res$ci.lb, 1)
  expect_length(res$ci.ub, 1)
  expect_length(res$pval,  1)

  expect_true(is.finite(res$b))
  expect_true(is.finite(res$ci.lb))
  expect_true(is.finite(res$ci.ub))
  expect_true(is.numeric(res$pval) | is.na(res$pval))
})
