# ==============================================================================
# SERVER_MODULES/ANALYSIS_RUNNER.R
# ==============================================================================

#' Analysis Runner Server Module
analysis_runner_server <- function(input, output, session, values) {

  # Track analysis status
  output$analysis_running <- reactive({
    if (is.null(values$analysis_running)) FALSE else values$analysis_running
  })
  outputOptions(output, "analysis_running", suspendWhenHidden = FALSE)

  # Run analysis using v0.2.0 API
  observeEvent(input$run_analysis, {
    run_multiverse_analysis_handler(input, values)
  })

  # Analysis complete status
  output$analysis_complete <- reactive({
    !is.null(values$results) && !is.null(values$results$results)
  })
  outputOptions(output, "analysis_complete", suspendWhenHidden = FALSE)
}

# ==============================================================================
# Helper Functions
# ==============================================================================

#' Run Multiverse Analysis Handler (v0.2.0 API)
run_multiverse_analysis_handler <- function(input, values) {
  req(values$spec_output, input$k_smallest)

  values$analysis_running <- TRUE
  options(metaMultiverse.k_smallest_ma = input$k_smallest)

  showNotification("Running multiverse analysis...",
                   type = "message", duration = NULL, id = "analysis_msg")

  tryCatch({
    # v0.2.0 API: run_multiverse_analysis() takes spec_output directly
    values$results <- run_multiverse_analysis(
      spec_output = values$spec_output,
      verbose = TRUE,
      progress = TRUE
    )

    values$analysis_running <- FALSE
    removeNotification("analysis_msg")
    showNotification("Analysis completed successfully!",
                     type = "message", duration = 5)

  }, error = function(e) {
    values$analysis_running <- FALSE
    removeNotification("analysis_msg")
    showNotification(paste("Error in analysis:", e$message),
                     type = "error", duration = 8)
  })
}
