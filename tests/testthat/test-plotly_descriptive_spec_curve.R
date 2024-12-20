test_that("plotly_descriptive_spec_curve produces an interactive plot", {
  # Mock data with required columns
  mock_data <- data.frame(
    b = c(0.5, 0.4),
    ci.lb = c(0.3, 0.2),
    ci.ub = c(0.7, 0.6),
    pval = c(0.01, 0.05),
    k = c(10, 15),
    set = c("1,2,3", "4,5,6"),  # Add 'set' column
    wf_1 = c("A", "B"),
    ma_method = "reml"
  )

  # Flexible lookup table for WF and How factors
  factor_label_lookup <- list(
    wf_1 = "Group",
    ma_method = "Meta-Analysis Method"
  )

  # Run the function
  plot <- plotly_descriptive_spec_curve(mock_data,
                                        colorblind_friendly = TRUE,
                                        factor_label_lookup = factor_label_lookup)

  # Expectations
  expect_s3_class(plot, "plotly")
})
