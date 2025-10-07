# ==============================================================================
# MINIMAL server_modules/factor_config_server.R
# ==============================================================================

factor_config_server <- function(input, output, session, values) {

  # Render factor configuration UI
  output$factor_config_content <- renderUI({
    req(values$data)
    create_minimal_factor_ui(values$data)
  })

  # Create specifications
  observeEvent(input$create_specs, {
    create_define_factors_specs(input, values, session)
  })

  # Specs created status (v0.2.0: use spec_output)
  output$specs_created <- reactive({ !is.null(values$spec_output) })
  outputOptions(output, "specs_created", suspendWhenHidden = FALSE)

  # Render specs summary
  output$specs_summary <- renderUI({
    render_minimal_specs_summary(values$factor_setup, values$spec_output)
  })
}

# ==============================================================================
# Minimal UI Creation
# ==============================================================================

create_minimal_factor_ui <- function(data) {
  required_cols <- c("study", "es_id", "yi", "vi")
  potential_cols <- names(data)[!names(data) %in% required_cols]
  potential_cols <- potential_cols[!sapply(potential_cols, function(col) is.numeric(data[[col]]))]

  if (length(potential_cols) == 0) {
    return(div("No factor columns available"))
  }

  # Create UI for each potential factor
  factor_uis <- lapply(potential_cols, function(col_name) {
    unique_vals <- unique(data[[col_name]])
    create_single_factor_ui(col_name, unique_vals)
  })

  tagList(
    h4("Configure Factors for define_factors()"),
    factor_uis
  )
}

create_single_factor_ui <- function(col_name, unique_vals) {
  div(style = "border: 1px solid #ddd; padding: 20px; margin: 10px 0; border-radius: 8px;",

      # Factor name and include checkbox
      div(style = "margin-bottom: 15px;",
          checkboxInput(paste0("include_", col_name),
                        paste("Include", col_name),
                        value = FALSE),
          p(paste("Levels:", paste(unique_vals, collapse = ", ")),
            style = "color: #666; margin: 5px 0;")
      ),

      # Show configuration when included
      conditionalPanel(
        condition = paste0("input.include_", col_name),

        # Factor label (for define_factors)
        textInput(paste0("label_", col_name),
                  "Factor Label:",
                  value = col_name),

        # Decision type
        radioButtons(paste0("decision_", col_name),
                     "Decision Type:",
                     choices = list("E (Equivalent)" = "E",
                                    "U (Uncertain)" = "U",
                                    "N (Non-equivalent)" = "N"),
                     selected = "U",
                     inline = TRUE),

        # Grouping type
        radioButtons(paste0("grouping_", col_name),
                     "Grouping Type:",
                     choices = list("Simple (each level + all combined)" = "simple",
                                    "Custom Groups" = "custom"),
                     selected = "simple",
                     inline = TRUE),

        # Custom groups interface
        conditionalPanel(
          condition = paste0("input.grouping_", col_name, " == 'custom'"),
          create_custom_groups_ui(col_name, unique_vals)
        )
      )
  )
}

create_custom_groups_ui <- function(col_name, unique_vals) {
  div(style = "background: #f8f9fa; padding: 15px; border-radius: 5px; margin-top: 10px;",
      h5("Custom Groups"),
      p(paste("Available levels:", paste(unique_vals, collapse = ", ")),
        style = "color: #666; font-style: italic;"),
      p("Define groups with one group per line in format: group_name = level1, level2, ...",
        style = "color: #999; font-size: 0.9em; font-style: italic;"),

      # Simple text area for group definitions
      textAreaInput(
        paste0("custom_groups_", col_name),
        "Group Definitions:",
        value = "",
        rows = 5,
        width = "100%",
        placeholder = "Example:\nconservative = low risk\nmoderate = low risk, some concerns\nliberal = low risk, some concerns, high risk"
      )
  )
}

# ==============================================================================
# Specification Creation
# ==============================================================================

create_define_factors_specs <- function(input, values, session) {
  req(values$data, input$dependency_strategy)

  # Get included factors
  required_cols <- c("study", "es_id", "yi", "vi")
  potential_cols <- names(values$data)[!names(values$data) %in% required_cols]
  potential_cols <- potential_cols[!sapply(potential_cols, function(col) is.numeric(values$data[[col]]))]

  # Build define_factors arguments
  factor_definitions <- list()

  for (col_name in potential_cols) {
    include_input <- paste0("include_", col_name)

    if (!is.null(input[[include_input]]) && input[[include_input]]) {

      # Get configuration
      label <- input[[paste0("label_", col_name)]]
      decision <- input[[paste0("decision_", col_name)]]
      grouping <- input[[paste0("grouping_", col_name)]]

      if (grouping == "simple") {
        # v0.2.0 Simple factor: Just "column_name|decision"
        factor_definitions[[label]] <- paste0(col_name, "|", decision)

      } else if (grouping == "custom") {
        # v0.2.0 Custom factor: list(column, decision = X, groups = list(...))
        groups <- collect_dynamic_groups(col_name, session)

        cat("DEBUG: For factor", label, "(", col_name, ") collected", length(groups), "custom groups\n")

        if (length(groups) > 0) {
          factor_definitions[[label]] <- list(col_name, decision = decision, groups = groups)
          cat("  -> Added as CUSTOM factor\n")
        } else {
          # If no groups defined, fall back to simple
          factor_definitions[[label]] <- paste0(col_name, "|", decision)
          cat("  -> Fell back to SIMPLE factor (no groups found)\n")
        }
      }
    }
  }

  if (length(factor_definitions) == 0) {
    showNotification("Please select at least one factor to include.", type = "warning", duration = 5)
    return()
  }

  tryCatch({
    # v0.2.0 API: define_factors() %>% create_multiverse_specifications()
    values$factor_setup <- do.call(define_factors, c(list(values$data), factor_definitions))

    # Create specifications using v0.2.0 API
    values$spec_output <- create_multiverse_specifications(
      factor_setup = values$factor_setup,
      ma_methods = get_selected_methods(input),
      dependencies = input$dependency_strategy
    )

    showNotification(paste("Created", values$spec_output$number_specs, "specifications"),
                     type = "message", duration = 5)

  }, error = function(e) {
    showNotification(paste("Error:", e$message), type = "error", duration = 8)
    print(e)
  })
}

get_selected_methods <- function(input) {
  methods <- character(0)
  # v0.2.0 method names (from register_defaults.R)
  if (input$method_fe) methods <- c(methods, "fe")
  if (input$method_reml) methods <- c(methods, "reml")
  if (input$method_3_level) methods <- c(methods, "three-level")  # Fixed: was "3-level"
  if (input$method_rve) methods <- c(methods, "rve")
  if (input$method_pet_peese) methods <- c(methods, "pet-peese")  # Fixed: was "pet.peese"
  if (input$method_pet_peese_corr) methods <- c(methods, "pet-peese-corrected")  # Fixed: was "pet.peese.corrected"
  if (input$method_puni_star) methods <- c(methods, "p-uniform")  # Fixed: was "puni_star"
  if (input$method_uwls) methods <- c(methods, "uwls")
  if (input$method_waap) methods <- c(methods, "waap")
  if (input$method_bayesmeta) methods <- c(methods, "bayesmeta")
  return(methods)
}

render_minimal_specs_summary <- function(factor_setup, spec_output) {
  if (is.null(spec_output)) return(NULL)

  div(
    h4("Specifications Created"),
    p(paste("Total specifications:", spec_output$number_specs)),
    p(paste("Unique multiverses:", length(unique(spec_output$specifications$multiverse_id)))),

    if (!is.null(factor_setup) && !is.null(factor_setup$factors)) {
      div(
        h5("Factors:"),
        tags$ul(
          lapply(seq_len(nrow(factor_setup$factors)), function(i) {
            factor <- factor_setup$factors[i, ]
            tags$li(paste(factor$label, ":", factor$column, "(", factor$decision, ")"))
          })
        )
      )
    }
  )
}

# ==============================================================================
# Helper Functions
# ==============================================================================

collect_dynamic_groups <- function(col_name, session) {
  # Read groups text from text area
  groups_text <- session$input[[paste0("custom_groups_", col_name)]]

  cat("DEBUG collect_dynamic_groups for", col_name, "\n")
  cat("  groups_text:", groups_text, "\n")

  if (is.null(groups_text) || nchar(trimws(groups_text)) == 0) {
    cat("  -> No groups (empty)\n")
    return(list())
  }

  # Parse text format: group_name = level1, level2
  groups <- list()
  lines <- strsplit(groups_text, "\n")[[1]]

  for (line in lines) {
    line <- trimws(line)
    if (nchar(line) == 0 || !grepl("=", line)) next

    parts <- strsplit(line, "=")[[1]]
    if (length(parts) != 2) next

    group_name <- trimws(parts[1])
    levels_str <- trimws(parts[2])
    levels <- trimws(strsplit(levels_str, ",")[[1]])

    if (nchar(group_name) > 0 && length(levels) > 0) {
      groups[[group_name]] <- levels
    }
  }

  cat("  -> Parsed", length(groups), "groups:", paste(names(groups), collapse = ", "), "\n")
  return(groups)
}
