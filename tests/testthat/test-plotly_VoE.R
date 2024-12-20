test_that("plotly_VoE returns the correct plot type", {

  # Mock data with correct numeric columns
  mock_data <- data.frame(
    b = as.numeric(c(0.5, 0.3, 0.3)),
    pval = as.numeric(c(0.001, 0.05, 0.3)),
    k = c(10, 12, 11),
    set = c("1,2,3", "4,5,6", "1")
  )
  # Test with interactive = TRUE (should return a plotly object)
  plot_interactive <- plotly_VoE(
    data = mock_data,
    x = "b",
    y = "pval",
    colorblind_friendly = TRUE,
    interactive = TRUE
  )

  # Expectations: Check if the plot is a plotly object
  expect_s3_class(plot_interactive, "plotly")

  # Test with interactive = FALSE (should return a ggplot object)
  plot_ggplot <- plotly_VoE(
    data = mock_data,
    x = "b",
    y = "pval",
    colorblind_friendly = TRUE,
    interactive = FALSE
  )

  # Expectations: Check if the plot is a ggplot object
  expect_s3_class(plot_ggplot, "gg")
})
