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

      # Hidden input to store groups data as JSON
      textInput(paste0("groups_data_", col_name), NULL, value = "{}",
                width = "0px",
                placeholder = NULL),
      tags$style(HTML(paste0("#groups_data_", col_name, " { display: none; }"))),

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

        function updateGroupsData(colName) {
          console.log('updateGroupsData called for:', colName);
          var container = document.getElementById('groups_container_' + colName);
          var groups = {};

          // Find all group divs
          var groupDivs = container.querySelectorAll('[id^=\"group_' + colName + '_\"]');
          console.log('Found', groupDivs.length, 'group divs');

          groupDivs.forEach(function(groupDiv) {
            var groupNum = groupDiv.getAttribute('data-groupnum');
            var nameInput = document.getElementById('group' + groupNum + '_name_' + colName);
            var groupName = nameInput ? nameInput.value : '';

            if (groupName) {
              var checkboxes = groupDiv.querySelectorAll('.group-level-checkbox:checked');
              var levels = [];
              checkboxes.forEach(function(cb) {
                levels.push(cb.value);
              });

              if (levels.length > 0) {
                groups[groupName] = levels;
              }
            }
          });

          // Update hidden input (Shiny textInput)
          var hiddenInputId = 'groups_data_' + colName;
          var $hiddenInput = $('#' + hiddenInputId);
          console.log('Updating hidden input:', hiddenInputId, 'with groups:', groups);
          if ($hiddenInput.length > 0) {
            var jsonStr = JSON.stringify(groups);
            console.log('Setting value:', jsonStr);
            $hiddenInput.val(jsonStr);
            $hiddenInput.trigger('change');
          } else {
            console.error('Hidden input not found:', hiddenInputId);
          }
        }

        function addGroup(colName, availableLevels) {
          var cleanColName = colName.replace(/[^A-Za-z0-9]/g, '_');
          var groupNum = window['groupCount_' + cleanColName] + 1;
          window['groupCount_' + cleanColName] = groupNum;

          var container = document.getElementById('groups_container_' + colName);

          var groupDiv = document.createElement('div');
          groupDiv.style = 'border: 1px solid #ccc; padding: 10px; margin: 10px 0; border-radius: 5px;';
          groupDiv.id = 'group_' + colName + '_' + groupNum;
          groupDiv.setAttribute('data-col', colName);
          groupDiv.setAttribute('data-groupnum', groupNum);

          var nameInput = '<label>Group Name:</label><input type=\"text\" id=\"group' + groupNum + '_name_' + colName + '\" placeholder=\"e.g., conservative\" style=\"width: 100%; margin: 5px 0;\" onchange=\"updateGroupsData(\\'' + colName + '\\')\">';

          var levelsHtml = '<label>Select Levels:</label><div style=\"margin: 5px 0;\">';
          for (var i = 0; i < availableLevels.length; i++) {
            var level = availableLevels[i];
            levelsHtml += '<label style=\"margin-right: 15px;\"><input type=\"checkbox\" class=\"group-level-checkbox\" value=\"' + level + '\" onchange=\"updateGroupsData(\\'' + colName + '\\')\">' + level + '</label>';
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
            updateGroupsData(colName);
          }
        }
      ")))
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
  # Read groups data from hidden input
  groups_json <- session$input[[paste0("groups_data_", col_name)]]

  cat("DEBUG collect_dynamic_groups for", col_name, "\n")
  cat("  groups_json:", groups_json, "\n")

  if (is.null(groups_json) || groups_json == "{}") {
    cat("  -> No groups (empty or null)\n")
    return(list())
  }

  # Parse JSON
  tryCatch({
    groups <- jsonlite::fromJSON(groups_json, simplifyVector = FALSE)
    cat("  -> Parsed groups:", paste(names(groups), collapse = ", "), "\n")
    return(groups)
  }, error = function(e) {
    warning("Failed to parse groups JSON for column ", col_name, ": ", e$message)
    return(list())
  })
}
