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
    create_define_factors_specs(input, values)
  })

  # Specs created status
  output$specs_created <- reactive({ !is.null(values$specifications) })
  outputOptions(output, "specs_created", suspendWhenHidden = FALSE)

  # Render specs summary
  output$specs_summary <- renderUI({
    render_minimal_specs_summary(values$factor_setup, values$specifications)
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

      # Dynamic groups container
      div(id = paste0("groups_container_", col_name)),

      # Add group button
      div(style = "margin-top: 10px;",
          actionButton(paste0("add_group_", col_name), "Add Group",
                       class = "btn-sm btn-primary",
                       onclick = paste0("addGroup('", col_name, "', ",
                                        jsonlite::toJSON(unique_vals), ")"))
      ),

      # JavaScript for dynamic group management
      tags$script(HTML(paste0("
        var groupCount_", gsub("[^A-Za-z0-9]", "_", col_name), " = 0;

        function addGroup(colName, availableLevels) {
          var cleanColName = colName.replace(/[^A-Za-z0-9]/g, '_');
          var groupNum = window['groupCount_' + cleanColName] + 1;
          window['groupCount_' + cleanColName] = groupNum;

          var container = document.getElementById('groups_container_' + colName);

          var groupDiv = document.createElement('div');
          groupDiv.style = 'border: 1px solid #ccc; padding: 10px; margin: 10px 0; border-radius: 5px;';
          groupDiv.id = 'group_' + colName + '_' + groupNum;

          var nameInput = '<label>Group Name:</label><input type=\"text\" id=\"group' + groupNum + '_name_' + colName + '\" placeholder=\"e.g., conservative\" style=\"width: 100%; margin: 5px 0;\">';

          var levelsHtml = '<label>Select Levels:</label><div style=\"margin: 5px 0;\">';
          for (var i = 0; i < availableLevels.length; i++) {
            var level = availableLevels[i];
            levelsHtml += '<label style=\"margin-right: 15px;\"><input type=\"checkbox\" value=\"' + level + '\" name=\"group' + groupNum + '_levels_' + colName + '\"> ' + level + '</label>';
          }
          levelsHtml += '</div>';

          var removeBtn = '<button type=\"button\" onclick=\"removeGroup(\\'' + colName + '\\', ' + groupNum + ')\" style=\"background: #dc3545; color: white; border: none; padding: 5px 10px; border-radius: 3px; margin-top: 10px;\">Remove Group</button>';

          groupDiv.innerHTML = nameInput + levelsHtml + removeBtn;
          container.appendChild(groupDiv);
        }

        function removeGroup(colName, groupNum) {
          var groupDiv = document.getElementById('group_' + colName + '_' + groupNum);
          if (groupDiv) {
            groupDiv.remove();
          }
        }
      ")))
  )
}

# ==============================================================================
# Specification Creation
# ==============================================================================

create_define_factors_specs <- function(input, values) {
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
        # Simple: list(column, decision, type="binary")
        factor_definitions[[label]] <- list(col_name, decision = decision, type = "binary")

      } else if (grouping == "custom") {
        # Custom groups: collect dynamically created groups
        groups <- collect_dynamic_groups(col_name, session)

        if (length(groups) > 0) {
          factor_definitions[[label]] <- list(col_name, decision = decision, groups = groups)
        }
      }
    }
  }

  if (length(factor_definitions) == 0) {
    showNotification("Please select at least one factor to include.", type = "warning", duration = 5)
    return()
  }

  tryCatch({
    # Call define_factors exactly like in your example
    values$factor_setup <- do.call(define_factors, c(list(values$data), factor_definitions))

    # Create specifications
    specs_result <- create_principled_multiverse_specifications(
      data = values$factor_setup$data,
      wf_vars = values$factor_setup$factors$wf_internal,
      ma_methods = get_selected_methods(input),
      dependencies = input$dependency_strategy,
      decision_map = values$factor_setup$decision_map,
      factor_groups = values$factor_setup$factor_groups
    )

    values$specifications <- specs_result$specifications

    showNotification(paste("Created", specs_result$number_specs, "specifications"),
                     type = "message", duration = 5)

  }, error = function(e) {
    showNotification(paste("Error:", e$message), type = "error", duration = 8)
    print(e)
  })
}

get_selected_methods <- function(input) {
  methods <- character(0)
  if (input$method_fe) methods <- c(methods, "fe")
  if (input$method_reml) methods <- c(methods, "reml")
  if (input$method_3_level) methods <- c(methods, "3-level")
  if (input$method_rve) methods <- c(methods, "rve")
  if (input$method_pet_peese) methods <- c(methods, "pet.peese")
  if (input$method_pet_peese_corr) methods <- c(methods, "pet.peese.corrected")
  if (input$method_puni_star) methods <- c(methods, "puni_star")
  if (input$method_uwls) methods <- c(methods, "uwls")
  if (input$method_waap) methods <- c(methods, "waap")
  if (input$method_bayesmeta) methods <- c(methods, "bayesmeta")
  return(methods)
}

render_minimal_specs_summary <- function(factor_setup, specifications) {
  if (is.null(specifications)) return(NULL)

  div(
    h4("Specifications Created"),
    p(paste("Total specifications:", nrow(specifications))),
    p(paste("Unique multiverses:", length(unique(specifications$multiverse_id)))),

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
  # This function would need to collect the dynamically created groups
  # For now, return empty list - this needs JavaScript integration
  groups <- list()

  # This is a placeholder - in a real implementation, you'd need to:
  # 1. Use JavaScript to collect all dynamically created group names and levels
  # 2. Send them back to Shiny via custom messages
  # 3. Parse them here

  return(groups)
}
