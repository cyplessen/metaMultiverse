# ==============================================================================
# UTILS/VALIDATION_HELPERS.R
# ==============================================================================

#' Render Data Validation Status
render_validation_status <- function(status, messages) {
  if (is.null(status)) return(NULL)

  if (status == "success") {
    div(
      style = "color: #28a745; border: 1px solid #28a745; padding: 10px; border-radius: 5px; background-color: #f8fff9;",
      tags$strong("✓ Validation Passed"),
      if (!is.null(messages) && length(messages) > 0) {
        div(style = "margin-top: 5px; font-size: 0.9em;",
            paste(messages, collapse = " "))
      }
    )
  } else if (status == "warning") {
    div(
      style = "color: #856404; border: 1px solid #ffc107; padding: 10px; border-radius: 5px; background-color: #fff3cd;",
      tags$strong("⚠ Validation Warnings"),
      if (!is.null(messages)) {
        div(style = "margin-top: 5px; font-size: 0.9em;", messages)
      }
    )
  } else if (status == "error") {
    div(
      style = "color: #721c24; border: 1px solid #dc3545; padding: 10px; border-radius: 5px; background-color: #f8d7da;",
      tags$strong("✗ Validation Failed"),
      if (!is.null(messages)) {
        div(style = "margin-top: 5px; font-size: 0.9em;", messages)
      }
    )
  }
}

#' Load Example Data
load_example_data <- function(values) {
  # This function should only be called from within a reactive context

  tryCatch({
    # Try to load package example data
    if (exists("data_tiny")) {
      values$data <- data_tiny
    } else {
      # Create fallback data
      values$data <- data.frame(
        study = rep(paste0("Study_", 1:10), each = 2),
        es_id = 1:20,
        yi = rnorm(20, mean = 0.3, sd = 0.2),
        vi = runif(20, min = 0.01, max = 0.1),
        population = rep(c("adults", "children"), times = 10),
        risk_of_bias = rep(c("low risk", "some concerns", "high risk"), length.out = 20),
        intervention_type = rep(c("cbt", "mindfulness", "behavioral"), length.out = 20),
        stringsAsFactors = FALSE
      )
    }

    # Validate example data
    values$data <- check_data_multiverse(values$data)
    values$data_validation_status <- "success"
    values$data_validation_messages <- "Example dataset loaded and validated successfully"

    showNotification("Example dataset with risk_of_bias column loaded!", type = "message", duration = 3)

  }, error = function(e) {
    values$data_validation_status <- "error"
    values$data_validation_messages <- e$message
    showNotification(paste("Error loading example data:", e$message), type = "error", duration = 5)
  })
}

#' Handle File Upload (FIXED - added better error handling)
handle_file_upload <- function(input, values) {
  req(input$file)

  cat("=== DETAILED FILE UPLOAD DEBUG ===\n")
  cat("File name:", input$file$name, "\n")
  cat("File path:", input$file$datapath, "\n")
  cat("File size:", file.info(input$file$datapath)$size, "bytes\n")

  # Show current data BEFORE upload
  cat("BEFORE UPLOAD:\n")
  if (!is.null(values$data)) {
    cat("  Current data rows:", nrow(values$data), "\n")
    cat("  Current data columns:", paste(names(values$data), collapse = ", "), "\n")
    cat("  First few study names:", paste(head(values$data$study, 3), collapse = ", "), "\n")
  } else {
    cat("  Current data: NULL\n")
  }

  # Reset validation status
  values$data_validation_status <- NULL
  values$data_validation_messages <- NULL

  # IMPORTANT: Also reset any derived data
  values$factor_setup <- NULL
  values$specifications <- NULL
  values$results <- NULL

  # FORCE NULL FIRST to trigger reactivity
  values$data <- NULL
  cat("Data set to NULL\n")

  tryCatch({
    # Load the data
    cat("Reading CSV from:", input$file$datapath, "\n")

    # Check if file exists and is readable
    if (!file.exists(input$file$datapath)) {
      stop("File does not exist at path: ", input$file$datapath)
    }

    raw_data <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
    cat("Raw data loaded successfully\n")
    cat("  Raw data rows:", nrow(raw_data), "\n")
    cat("  Raw data columns:", paste(names(raw_data), collapse = ", "), "\n")

    # Show first few rows of actual uploaded data
    cat("  First few studies in uploaded data:", paste(head(raw_data$study, 3), collapse = ", "), "\n")

    # Run data validation
    cat("Running validation...\n")
    validated_data <- check_data_multiverse(raw_data)
    cat("Validation completed\n")

    # ASSIGN NEW DATA
    values$data <- validated_data
    cat("NEW DATA ASSIGNED:\n")
    cat("  New data rows:", nrow(values$data), "\n")
    cat("  New data columns:", paste(names(values$data), collapse = ", "), "\n")
    cat("  First few study names in new data:", paste(head(values$data$study, 3), collapse = ", "), "\n")

    values$data_validation_status <- "success"
    values$data_validation_messages <- "Data uploaded and validated successfully"

    showNotification("Data uploaded and validated successfully!", type = "message", duration = 3)

  }, error = function(e) {
    cat("ERROR during upload:", e$message, "\n")
    values$data_validation_status <- "error"
    values$data_validation_messages <- e$message

    showNotification(paste("Data validation failed:", e$message),
                     type = "error", duration = 8)

  }, warning = function(w) {
    cat("WARNING during upload:", w$message, "\n")
    values$data_validation_status <- "warning"
    values$data_validation_messages <- w$message

    showNotification(paste("Data loaded with warnings:", w$message),
                     type = "warning", duration = 5)
  })

  cat("=== FILE UPLOAD DEBUG COMPLETED ===\n")
}


#' Create Data Preview Table
create_data_preview_table <- function(data) {
  if (is.null(data)) return(NULL)

  # Create the datatable
  dt <- DT::datatable(head(data, 100),
                      options = list(scrollX = TRUE, pageLength = 10))

  # Apply formatting to numeric columns
  numeric_cols <- names(data)[sapply(data, is.numeric)]

  # Format common meta-analysis columns
  effect_size_cols <- intersect(numeric_cols, c("yi", "d", "r", "es", "effect_size", "effect"))
  variance_cols <- intersect(numeric_cols, c("vi", "var", "variance", "var_yi"))
  se_cols <- intersect(numeric_cols, c("sei", "se", "standard_error", "se_yi"))

  # Apply appropriate formatting
  if (length(effect_size_cols) > 0) {
    dt <- dt %>% DT::formatRound(effect_size_cols, 2)
  }
  if (length(variance_cols) > 0) {
    dt <- dt %>% DT::formatRound(variance_cols, 4)
  }
  if (length(se_cols) > 0) {
    dt <- dt %>% DT::formatRound(se_cols, 2)
  }

  return(dt)
}
