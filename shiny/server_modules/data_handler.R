# ==============================================================================
# SERVER_MODULES/DATA_HANDLER.R
# ==============================================================================

#' Data Upload Server Module
data_upload_server <- function(input, output, session, values) {

  # Auto-load example data on startup
  observe({
    if (is.null(values$data)) {
      load_example_data(values)
    }
  })

  # Handle file upload with debug
  observeEvent(input$file, {
    cat("=== FILE UPLOAD EVENT TRIGGERED ===\n")
    handle_file_upload(input, values)  # FIXED: removed double 'h'
    cat("=== FILE UPLOAD COMPLETED ===\n")
  })

  # Data status outputs with req() for proper reactivity
  output$data_uploaded <- reactive({
    result <- !is.null(values$data)
    cat("data_uploaded check:", result, "\n")
    result
  })
  outputOptions(output, "data_uploaded", suspendWhenHidden = FALSE)

  output$n_rows <- renderText({
    req(values$data)
    cat("n_rows rendered\n")
    nrow(values$data)
  })

  output$n_studies <- renderText({
    req(values$data)
    cat("n_studies rendered\n")
    length(unique(values$data$study))
  })

  output$n_effects <- renderText({
    req(values$data)
    cat("n_effects rendered\n")
    length(unique(values$data$es_id))
  })

  output$n_potential_factors <- renderText({
    req(values$data)
    cat("n_potential_factors rendered\n")
    required_cols <- c("study", "es_id", "yi", "vi")
    potential_cols <- names(values$data)[!names(values$data) %in% required_cols]
    length(potential_cols)
  })

  # Validation status display
  output$data_validation_status <- renderUI({
    req(values$data_validation_status)
    cat("validation status rendered\n")
    render_validation_status(values$data_validation_status, values$data_validation_messages)
  })

  # Data preview with forced reactivity and debug
  output$data_preview_step2 <- DT::renderDataTable({
    cat("=== DATA PREVIEW RENDER CALLED ===\n")

    if (is.null(values$data)) {
      cat("Data is NULL, returning empty table\n")
      return(DT::datatable(data.frame(Message = "No data loaded")))
    }

    cat("Data exists:", nrow(values$data), "rows,", ncol(values$data), "columns\n")
    cat("Column names:", paste(names(values$data), collapse = ", "), "\n")

    result <- create_data_preview_table(values$data)
    cat("Data table created successfully\n")
    return(result)
  })
}
