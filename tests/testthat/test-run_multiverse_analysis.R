test_that("run_multiverse_analysis returns tidy multiverse", {

  spec_df <- create_multiverse_specifications(
    data          = data_tiny,
    wf_vars       = "wf_1",
    ma_methods    = c("reml"),
    dependencies  = "aggregate"
  )$specifications

  withr::with_options(
    list(metaMultiverse.k_smallest_ma = 1),   # read by general_multiverse()
    {
      res <- run_multiverse_analysis(data_tiny, spec_df)
    }
  )
  # --- structure -----------------------------
  expect_s3_class(res, "data.frame")
  expect_true(all(c("b", "ci.lb", "ci.ub", "pval",
                    "k", "set", "full_set") %in% names(res)))

  # --- each result corresponds to a spec -----
  expect_true(all(res$row_id %in% spec_df$row_id))

  # --- no duplicate b / set / method rows ----
  expect_equal(
    nrow(res),
    nrow(unique(res[, c("b", "set", "ma_method")]))
  )

  # --- full_set is always 0 or 1 -------------
  expect_true(all(res$full_set %in% c(0, 1)))
})
