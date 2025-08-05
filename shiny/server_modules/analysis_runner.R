# ==============================================================================
# SERVER_MODULES/ANALYSIS_RUNNER.R
# ==============================================================================

#' Analysis Runner Server Module
analysis_runner_server <- function(input, output, session, values) {

  # Track analysis status (MOVED HERE - don't initialize in server function)
  output$analysis_running <- reactive({
    if (is.null(values$analysis_running)) FALSE else values$analysis_running
  })
  outputOptions(output, "analysis_running", suspendWhenHidden = FALSE)

  # Run analysis (UPDATED to use enhanced handler)
  observeEvent(input$run_analysis, {
    run_enhanced_multiverse_analysis_handler(input, values)
  })

  # Analysis complete status
  output$analysis_complete <- reactive({
    !is.null(values$results) && !is.null(values$results$results)
  })
  outputOptions(output, "analysis_complete", suspendWhenHidden = FALSE)
}

# And add the enhanced analysis handler function:
run_enhanced_multiverse_analysis_handler <- function(input, values) {
  req(values$data, values$specifications, input$k_smallest)

  values$analysis_running <- TRUE
  options(metaMultiverse.k_smallest_ma = input$k_smallest)

  showNotification("Running enhanced multiverse analysis with custom groupings...",
                   type = "message", duration = NULL, id = "analysis_msg")

  tryCatch({
    # Use the enhanced run_multiverse_analysis with factor_groups support
    analysis_data <- if (!is.null(values$factor_setup)) values$factor_setup$data else values$data
    factor_groups <- if (!is.null(values$factor_setup)) values$factor_setup$factor_groups else NULL

    values$results <- run_multiverse_analysis(
      data = analysis_data,
      specifications = values$specifications,
      factor_groups = factor_groups,
      verbose = TRUE,
      progress = TRUE
    )

    values$analysis_running <- FALSE
    removeNotification("analysis_msg")
    showNotification("Enhanced analysis completed successfully!",
                     type = "message", duration = 5)

  }, error = function(e) {
    values$analysis_running <- FALSE
    removeNotification("analysis_msg")
    showNotification(paste("Error in enhanced analysis:", e$message),
                     type = "error", duration = 8)
  })
}


# ==============================================================================
# Helper Functions
# ==============================================================================

#' Run Multiverse Analysis Handler
run_enhanced_multiverse_analysis_handler <- function(input, values) {
  req(values$data, values$specifications, input$k_smallest)

  values$analysis_running <- TRUE
  options(metaMultiverse.k_smallest_ma = input$k_smallest)

  showNotification("Running enhanced multiverse analysis with custom groupings...",
                   type = "message", duration = NULL, id = "analysis_msg")

  tryCatch({
    # Use the enhanced run_multiverse_analysis with factor_groups support
    values$results <- run_multiverse_analysis(
      data = values$factor_setup$data,  # Use the setup data with wf_* columns
      specifications = values$specifications,
      factor_groups = values$factor_setup$factor_groups,  # Pass custom groups
      verbose = TRUE,
      progress = TRUE
    )

    values$analysis_running <- FALSE
    removeNotification("analysis_msg")
    showNotification("Enhanced analysis completed successfully! Custom groupings processed.",
                     type = "message", duration = 5)

  }, error = function(e) {
    values$analysis_running <- FALSE
    removeNotification("analysis_msg")
    showNotification(paste("Error in enhanced analysis:", e$message),
                     type = "error", duration = 8)
  })
}
