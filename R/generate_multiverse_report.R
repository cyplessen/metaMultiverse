#' Generate Comprehensive Multiverse Analysis Report
#'
#' Creates a detailed text report and violin plots summarizing multiverse analysis results
#'
#' @param results Data frame containing multiverse analysis results
#' @param specifications Data frame containing analysis specifications
#' @param original_data Original dataset used for analysis
#' @param output_format Character: "markdown", "html", or "text" for report format
#' @param include_plots Logical: whether to generate violin plots
#' @param plot_output_dir Character: directory to save plots (if NULL, plots returned as list)
#'
#' @return List containing report text and plots (if requested)
#' @export
generate_multiverse_report <- function(results, specifications, original_data,
                                       output_format = "markdown",
                                       include_plots = TRUE,
                                       plot_output_dir = NULL) {

  # Load required packages
  require(dplyr)
  require(ggplot2)
  require(plotly)

  # Handle different result formats
  if (is.list(results) && "results" %in% names(results)) {
    results_df <- results$results
  } else if (is.data.frame(results)) {
    results_df <- results
  } else {
    stop("Results must be a data frame or a list containing a 'results' element")
  }

  # Check required columns exist
  required_cols <- c("b", "ci.lb", "ci.ub")
  missing_cols <- setdiff(required_cols, names(results_df))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns in results:", paste(missing_cols, collapse = ", ")))
  }

  # Prepare data
  results_clean <- results_df[complete.cases(results_df[required_cols]), ]
  wf_columns <- names(results_clean)[grepl("^wf_", names(results_clean))]

  # Calculate summary statistics
  summary_stats <- list(
    total_analyses = nrow(results_clean),
    unique_multiverses = if("multiverse_id" %in% names(results_clean)) length(unique(results_clean$multiverse_id)) else NA,
    effect_range = range(results_clean$b, na.rm = TRUE),
    mean_effect = mean(results_clean$b, na.rm = TRUE),
    median_effect = median(results_clean$b, na.rm = TRUE),
    sd_effect = sd(results_clean$b, na.rm = TRUE),
    significant_analyses = sum(results_clean$pval < 0.05, na.rm = TRUE),
    prop_significant = mean(results_clean$pval < 0.05, na.rm = TRUE),
    methods_used = if("ma_method" %in% names(results_clean)) unique(results_clean$ma_method) else "Unknown",
    dependencies_used = if("dependency" %in% names(results_clean)) unique(results_clean$dependency) else "Unknown",
    ci_widths = results_clean$ci.ub - results_clean$ci.lb
  )

  # Analyze by which factors
  wf_analysis <- list()
  if (length(wf_columns) > 0) {
    for (wf_col in wf_columns) {
      wf_name <- gsub("^wf_", "", wf_col)

      wf_summary <- results_clean %>%
        group_by(!!sym(wf_col)) %>%
        summarise(
          n_analyses = n(),
          mean_effect = mean(b, na.rm = TRUE),
          median_effect = median(b, na.rm = TRUE),
          sd_effect = sd(b, na.rm = TRUE),
          min_effect = min(b, na.rm = TRUE),
          max_effect = max(b, na.rm = TRUE),
          mean_ci_width = mean(ci.ub - ci.lb, na.rm = TRUE),
          prop_significant = mean(pval < 0.05, na.rm = TRUE),
          .groups = "drop"
        )

      wf_analysis[[wf_name]] <- wf_summary
    }
  }

  # Generate report text
  report_text <- generate_report_text(summary_stats, wf_analysis, wf_columns, output_format)

  # Generate plots if requested
  plots <- NULL
  if (include_plots) {
    plots <- generate_multiverse_plots(results_clean, wf_columns, plot_output_dir)
  }

  return(list(
    report = report_text,
    plots = plots,
    summary_stats = summary_stats,
    wf_analysis = wf_analysis
  ))
}

#' Generate Report Text
#' @keywords internal
generate_report_text <- function(summary_stats, wf_analysis, wf_columns, format) {

  # Method section
  method_text <- sprintf(
    "## Method\n\n### Multiverse Analysis Approach\n\nWe conducted a multiverse meta-analysis to explore how different analytical decisions affect our results. This approach systematically examines multiple reasonable analytical paths rather than relying on a single set of choices, addressing concerns about researcher degrees of freedom and providing transparency about the robustness of findings.\n\n### Analysis Specifications\n\nA total of %d unique analysis specifications were generated across %d distinct multiverses. These specifications varied across the following dimensions:\n\n**Meta-analytic methods:** %s\n\n**Dependency handling strategies:** %s\n\n",
    summary_stats$total_analyses,
    summary_stats$unique_multiverses,
    paste(summary_stats$methods_used, collapse = ", "),
    paste(summary_stats$dependencies_used, collapse = ", ")
  )

  # Add which factors section if present
  if (length(wf_analysis) > 0) {
    method_text <- paste0(method_text, "**Which factors explored:**\n")
    for (factor_name in names(wf_analysis)) {
      levels <- unique(wf_analysis[[factor_name]][[1]])
      method_text <- paste0(method_text, sprintf("- *%s*: %s\n",
                                                 factor_name,
                                                 paste(levels, collapse = ", ")))
    }
    method_text <- paste0(method_text, "\n")
  }

  # Results section
  results_text <- sprintf(
    "## Results\n\n### Overall Multiverse Results\n\nAcross all %d analysis specifications, effect sizes ranged from %.3f to %.3f (M = %.3f, SD = %.3f, Mdn = %.3f). Of all analyses, %d (%.1f%%) yielded statistically significant results (p < .05).\n\n",
    summary_stats$total_analyses,
    summary_stats$effect_range[1],
    summary_stats$effect_range[2],
    summary_stats$mean_effect,
    summary_stats$sd_effect,
    summary_stats$median_effect,
    summary_stats$significant_analyses,
    summary_stats$prop_significant * 100
  )

  # Add which factors results if present
  if (length(wf_analysis) > 0) {
    results_text <- paste0(results_text, "### Results by Which Factors\n\n")

    for (factor_name in names(wf_analysis)) {
      factor_data <- wf_analysis[[factor_name]]
      colnames(factor_data)[1] <- "level"

      results_text <- paste0(results_text, sprintf("**%s:**\n", tools::toTitleCase(factor_name)))

      for (i in 1:nrow(factor_data)) {
        row <- factor_data[i, ]
        results_text <- paste0(results_text, sprintf(
          "- *%s* (n = %d): M = %.3f, SD = %.3f, range = [%.3f, %.3f], CI width = %.3f, %d%% significant\n",
          row$level,
          row$n_analyses,
          row$mean_effect,
          row$sd_effect,
          row$min_effect,
          row$max_effect,
          row$mean_ci_width,
          round(row$prop_significant * 100)
        ))
      }
      results_text <- paste0(results_text, "\n")
    }
  }

  # Confidence intervals section
  ci_text <- sprintf(
    "### Confidence Interval Analysis\n\nConfidence interval widths across all analyses averaged %.3f (SD = %.3f), indicating %s precision in effect size estimates. The proportion of confidence intervals excluding zero was %.1f%%, suggesting %s evidence for a non-null effect across analytical choices.\n\n",
    mean(summary_stats$ci_widths, na.rm = TRUE),
    sd(summary_stats$ci_widths, na.rm = TRUE),
    ifelse(mean(summary_stats$ci_widths, na.rm = TRUE) < 0.5, "good",
           ifelse(mean(summary_stats$ci_widths, na.rm = TRUE) < 1, "moderate", "poor")),
    summary_stats$prop_significant * 100,
    ifelse(summary_stats$prop_significant > 0.8, "strong",
           ifelse(summary_stats$prop_significant > 0.5, "moderate", "weak"))
  )

  # Combine all sections
  full_report <- paste0(method_text, results_text, ci_text)

  # Format according to output type
  if (format == "html") {
    full_report <- markdown::markdownToHTML(text = full_report, fragment.only = TRUE)
  } else if (format == "text") {
    full_report <- gsub("##+ ", "", full_report)
    full_report <- gsub("\\*\\*(.+?)\\*\\*", "\\1", full_report)
    full_report <- gsub("\\*(.+?)\\*", "\\1", full_report)
  }

  return(full_report)
}

#' Generate Multiverse Plots
#' @keywords internal
generate_multiverse_plots <- function(results_clean, wf_columns, plot_output_dir) {

  plots <- list()

  # 1. Overall effect size distribution
  plots$overall_distribution <- ggplot(results_clean, aes(x = "All Analyses", y = b)) +
    geom_violin(fill = "lightblue", alpha = 0.7) +
    geom_boxplot(width = 0.1, outlier.shape = NA) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(title = "Distribution of Effect Sizes Across All Specifications",
         x = "", y = "Effect Size") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

  # 2. Effect sizes by meta-analytic method
  plots$by_method <- ggplot(results_clean, aes(x = ma_method, y = b, fill = ma_method)) +
    geom_violin(alpha = 0.7) +
    geom_boxplot(width = 0.1, outlier.shape = NA) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(title = "Effect Sizes by Meta-Analytic Method",
         x = "Meta-Analytic Method", y = "Effect Size") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none")

  # 3. Effect sizes by dependency handling
  plots$by_dependency <- ggplot(results_clean, aes(x = dependency, y = b, fill = dependency)) +
    geom_violin(alpha = 0.7) +
    geom_boxplot(width = 0.1, outlier.shape = NA) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(title = "Effect Sizes by Dependency Handling Strategy",
         x = "Dependency Strategy", y = "Effect Size") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none")

  # 4. P-value distribution
  plots$pvalue_distribution <- ggplot(results_clean, aes(x = pval)) +
    geom_histogram(bins = 30, fill = "lightcoral", alpha = 0.7, color = "black") +
    geom_vline(xintercept = 0.05, linetype = "dashed", color = "red", size = 1) +
    labs(title = "Distribution of P-values Across All Specifications",
         x = "P-value", y = "Frequency") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

  # 5. Confidence interval widths
  results_clean$ci_width <- results_clean$ci.ub - results_clean$ci.lb
  plots$ci_width_distribution <- ggplot(results_clean, aes(x = "All Analyses", y = ci_width)) +
    geom_violin(fill = "lightgreen", alpha = 0.7) +
    geom_boxplot(width = 0.1, outlier.shape = NA) +
    labs(title = "Distribution of Confidence Interval Widths",
         x = "", y = "CI Width") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

  # 6. Which factors plots (if any)
  if (length(wf_columns) > 0) {
    for (wf_col in wf_columns) {
      wf_name <- gsub("^wf_", "", wf_col)

      plots[[paste0("wf_", wf_name)]] <- ggplot(results_clean, aes_string(x = wf_col, y = "b", fill = wf_col)) +
        geom_violin(alpha = 0.7) +
        geom_boxplot(width = 0.1, outlier.shape = NA) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
        labs(title = paste("Effect Sizes by", tools::toTitleCase(wf_name)),
             x = tools::toTitleCase(wf_name), y = "Effect Size") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
              axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "none")
    }
  }

  # Save plots if directory specified
  if (!is.null(plot_output_dir)) {
    if (!dir.exists(plot_output_dir)) {
      dir.create(plot_output_dir, recursive = TRUE)
    }

    for (plot_name in names(plots)) {
      ggsave(file.path(plot_output_dir, paste0(plot_name, ".png")),
             plots[[plot_name]], width = 10, height = 6, dpi = 300)
    }
  }

  return(plots)
}

#' Create Interactive Plotly Versions of Multiverse Plots
#'
#' @param results Data frame containing multiverse analysis results
#' @param wf_columns Character vector of which factor column names
#' @return List of interactive plotly objects
#' @export
create_interactive_multiverse_plots <- function(results, wf_columns = NULL) {

  require(plotly)

  # Handle different result formats
  if (is.list(results) && "results" %in% names(results)) {
    results_df <- results$results
  } else if (is.data.frame(results)) {
    results_df <- results
  } else {
    stop("Results must be a data frame or a list containing a 'results' element")
  }

  # Check required columns exist
  required_cols <- c("b", "ci.lb", "ci.ub", "pval")
  missing_cols <- setdiff(required_cols, names(results_df))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns in results:", paste(missing_cols, collapse = ", ")))
  }

  # Clean results
  results_clean <- results_df[complete.cases(results_df[required_cols]), ]
  results_clean$ci_width <- results_clean$ci.ub - results_clean$ci.lb

  plots <- list()

  # 1. Interactive specification curve
  results_clean$rank <- rank(results_clean$b)
  plots$spec_curve <- plot_ly(results_clean, x = ~rank, y = ~b,
                              text = ~paste("Method:", ma_method,
                                            "<br>Effect:", round(b, 3),
                                            "<br>P-value:", round(pval, 3),
                                            "<br>CI: [", round(ci.lb, 3), ",", round(ci.ub, 3), "]"),
                              hovertemplate = "%{text}<extra></extra>") %>%
    add_markers(alpha = 0.6) %>%
    add_lines() %>%
    layout(title = "Interactive Specification Curve",
           xaxis = list(title = "Specification Rank"),
           yaxis = list(title = "Effect Size"))

  # 2. Interactive vibration of effects plot
  plots$vibration <- plot_ly(results_clean, x = ~b, y = ~pval,
                             text = ~paste("Method:", ma_method,
                                           "<br>Effect:", round(b, 3),
                                           "<br>P-value:", round(pval, 3),
                                           "<br>Studies:", k),
                             hovertemplate = "%{text}<extra></extra>") %>%
    add_markers(alpha = 0.6) %>%
    add_lines(x = range(results_clean$b), y = 0.05,
              line = list(dash = "dash", color = "red"),
              showlegend = FALSE, hoverinfo = "none") %>%
    layout(title = "Vibration of Effects",
           xaxis = list(title = "Effect Size"),
           yaxis = list(title = "P-value", type = "log"))

  # 3. Which factors interactive plots
  if (!is.null(wf_columns) && length(wf_columns) > 0) {
    for (wf_col in wf_columns) {
      wf_name <- gsub("^wf_", "", wf_col)

      plots[[paste0("wf_", wf_name)]] <- plot_ly(results_clean,
                                                 x = as.formula(paste0("~", wf_col)),
                                                 y = ~b,
                                                 type = "violin",
                                                 box = list(visible = TRUE),
                                                 text = ~paste("Level:", get(wf_col),
                                                               "<br>Effect:", round(b, 3),
                                                               "<br>Method:", ma_method),
                                                 hovertemplate = "%{text}<extra></extra>") %>%
        layout(title = paste("Effect Sizes by", tools::toTitleCase(wf_name)),
               xaxis = list(title = tools::toTitleCase(wf_name)),
               yaxis = list(title = "Effect Size"))
    }
  }

  return(plots)
}

# Example usage:
# report_results <- generate_multiverse_report(
#   results = your_results_df,
#   specifications = your_specs_df,
#   original_data = your_data_df,
#   output_format = "markdown",
#   include_plots = TRUE,
#   plot_output_dir = "multiverse_plots"
# )
#
# # Access report text
# cat(report_results$report)
#
# # View plots
# report_results$plots$overall_distribution
#
# # Create interactive versions
# interactive_plots <- create_interactive_multiverse_plots(your_results_df, wf_columns)
# interactive_plots$spec_curve
