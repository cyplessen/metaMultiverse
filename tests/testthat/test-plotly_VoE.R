test_that("plotly_VoE generates an interactive VoE plot", {
  # Mock data with correct numeric columns
  mock_data <- data.frame(
    b = as.numeric(c(0.5, 0.3)),
    pval = as.numeric(c(0.01, 0.05)),
    k = c(10, 12),
    set = c("1,2,3", "4,5,6")
  )

  # Run the function
  plot <- plotly_VoE(mock_data, x = "b", y = "pval", colorblind_friendly = T)

  # Expectations
  expect_s3_class(plot, "plotly")
})
