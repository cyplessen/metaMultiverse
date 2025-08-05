# ==============================================================================
# SERVER_MODULES/RESULTS_SERVER.R
# ==============================================================================

#' Results Server Module
results_server <- function(input, output, session, values) {

  # Results summary
  output$results_summary <- renderUI({
    create_results_summary(values$results)
  })

  # Specification curve plot
  output$spec_curve <- renderPlotly({
    create_spec_curve_plot(values$results)
  })

  # Vibration of effects plot
  output$voe_plot <- renderPlotly({
    create_voe_plot(values$results, input)
  })

  # Results table
  output$results_table <- DT::renderDataTable({
    create_results_table(values$results)
  })

  # Warnings output
  output$warnings_output <- renderText({
    get_warnings_text(values$results)
  })
}

# ==============================================================================
# Helper Functions
# ==============================================================================

#' Create Results Summary
create_results_summary <- function(results) {
  if (is.null(results) || is.null(results$results) || nrow(results$results) == 0) {
    return(div(class = "alert alert-warning", "No valid results generated."))
  }

  data <- results$results

  fluidRow(
    column(3,
           wellPanel(
             h5("Total Specifications"),
             h3(nrow(data), style = "margin: 0; color: #337ab7;"),
             p("analyses conducted")
           )
    ),
    column(3,
           wellPanel(
             h5("Mean Effect Size"),
             h3(sprintf("%.2f", mean(data$b, na.rm = TRUE)), style = "margin: 0; color: #337ab7;"),
             p(paste("Range:", sprintf("%.2f", min(data$b, na.rm = TRUE)), "to", sprintf("%.2f", max(data$b, na.rm = TRUE))))
           )
    ),
    column(3,
           wellPanel(
             h5("Significant Results"),
             h3(paste0(sprintf("%.1f", 100 * mean(data$pval < 0.05, na.rm = TRUE)), "%"),
                style = "margin: 0; color: #337ab7;"),
             p(paste(sum(data$pval < 0.05, na.rm = TRUE), "of", nrow(data), "specs"))
           )
    ),
    column(3,
           wellPanel(
             h5("Average Studies"),
             h3(sprintf("%.1f", mean(data$k, na.rm = TRUE)), style = "margin: 0; color: #337ab7;"),
             p("per analysis")
           )
    )
  )
}

#' Create Specification Curve Plot
create_enhanced_spec_curve_plot <- function(results, factor_setup) {
  if (is.null(results) || is.null(results$results) || nrow(results$results) == 0) {
    return(plotly_empty() %>% add_annotations(text = "No results to display", showarrow = FALSE))
  }

  tryCatch({
    # Create meaningful factor labels using the original factor setup
    factor_label_lookup <- list()

    if (!is.null(factor_setup) && !is.null(factor_setup$factors)) {
      for (i in seq_len(nrow(factor_setup$factors))) {
        wf_name <- factor_setup$factors$wf_internal[i]
        factor_label <- factor_setup$factors$label[i]
        factor_label_lookup[[wf_name]] <- factor_label
      }
    }

    # Add standard labels
    factor_label_lookup[["ma_method"]] <- "Meta-Analysis Method"
    factor_label_lookup[["dependency"]] <- "Dependency Handling"

    # Check if plotting function exists
    if (exists("plotly_descriptive_spec_curve")) {
      plotly_descriptive_spec_curve(
        data = results$results,
        factor_label_lookup = factor_label_lookup,
        colorblind_friendly = TRUE,
        interactive = TRUE
      )
    } else {
      # Enhanced fallback plot
      data <- results$results
      data$x_rank <- rank(data$b)

      plot_ly(data, x = ~x_rank, y = ~b, type = 'scatter', mode = 'lines+markers',
              error_y = list(array = ~(ci.ub - b), arrayminus = ~(b - ci.lb)),
              text = ~paste("Method:", ma_method,
                            "<br>Effect:", sprintf("%.3f", b),
                            "<br>p-value:", sprintf("%.4f", pval),
                            "<br>Studies:", k),
              hovertemplate = "%{text}<extra></extra>") %>%
        layout(title = "Enhanced Specification Curve with Custom Groupings",
               xaxis = list(title = "Specification Rank"),
               yaxis = list(title = "Effect Size"))
    }
  }, error = function(e) {
    plotly_empty() %>% add_annotations(text = paste("Plot error:", e$message), showarrow = FALSE)
  })
}

#' Create Vibration of Effects Plot
create_voe_plot <- function(results, input) {
  if (is.null(results) || is.null(results$results) || nrow(results$results) == 0) {
    return(plotly_empty() %>% add_annotations(text = "No results to display", showarrow = FALSE))
  }

  # Set default values if inputs are NULL
  voe_cutoff <- if (is.null(input$voe_cutoff)) 10 else input$voe_cutoff
  voe_hline <- if (is.null(input$voe_hline)) 0.05 else input$voe_hline
  voe_colorblind <- if (is.null(input$voe_colorblind)) TRUE else input$voe_colorblind

  tryCatch({
    # Check if plotting function exists
    if (exists("plotly_VoE")) {
      plotly_VoE(
        data = results$results,
        x = "b",
        y = "pval",
        colorblind_friendly = voe_colorblind,
        cutoff = voe_cutoff,
        hline_value = voe_hline,
        interactive = TRUE
      )
    } else {
      # Fallback plot
      data <- results$results
      data <- data[data$k >= voe_cutoff, ]

      if (nrow(data) == 0) {
        return(plotly_empty() %>% add_annotations(
          text = paste("No results with ≥", voe_cutoff, "studies"),
          showarrow = FALSE))
      }

      plot_ly(data, x = ~b, y = ~pval, type = 'scatter', mode = 'markers',
              text = ~paste("Method:", ma_method, "<br>Effect:", sprintf("%.2f", b),
                            "<br>p-value:", sprintf("%.3f", pval), "<br>Studies:", k),
              hovertemplate = "%{text}<extra></extra>") %>%
        add_lines(x = range(data$b), y = voe_hline,
                  line = list(dash = "dash", color = "red"),
                  showlegend = FALSE, hoverinfo = "none") %>%
        layout(title = paste("Vibration of Effects (", nrow(data), " analyses)"),
               xaxis = list(title = "Effect Size (b)"),
               yaxis = list(title = "P-value", type = "log"))
    }
  }, error = function(e) {
    plotly_empty() %>% add_annotations(text = paste("Plot error:", e$message), showarrow = FALSE)
  })
}

#' Create Results Table
create_results_table <- function(results) {
  if (is.null(results) || is.null(results$results) || nrow(results$results) == 0) {
    return(DT::datatable(data.frame(Message = "No results to display")))
  }

  DT::datatable(results$results,
                options = list(scrollX = TRUE, pageLength = 25)) %>%
    DT::formatRound(c("b", "ci.lb", "ci.ub"), 2) %>%
    DT::formatRound("pval", 3)
}

#' Get Warnings Text
get_warnings_text <- function(results) {
  if (is.null(results)) return("No analysis run yet.")

  if (length(results$multiverse_warnings) > 0) {
    paste(results$multiverse_warnings, collapse = "\n")
  } else {
    "No warnings generated."
  }
}
