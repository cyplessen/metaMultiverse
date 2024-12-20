# helpers_spec_curve.R
# --------------------
# This file contains helper functions used in the creation and visualization of
# specification curve plots for multiverse meta-analysis.
# - Dynamic label generation
# - Tooltip creation
# - Quantile-based coloring

# -----------------------------------------------------------------------------
# 1. Generate Dynamic Labels
# -----------------------------------------------------------------------------
#' @title Generate Dynamic Labels
#' @description Dynamically maps column names to human-readable labels using
#'   a lookup table.
#' @param data A data frame containing the specification data.
#' @param lookup_table A named list mapping column names to human-readable labels.
#' @return A named vector of human-readable labels for each relevant column.
#' @examples
#' factor_labels <- generate_dynamic_labels(data, lookup_table)
#'
generate_dynamic_labels <- function(data, lookup_table) {
  # Detect wf_ columns and include ma_method
  factor_cols <- c(
    colnames(data)[grep("^wf_", colnames(data))],
    "ma_method"
  )

  # Map columns to human-readable labels
  factor_labels <- sapply(factor_cols, function(col) {
    if (!is.null(lookup_table[[col]])) {
      lookup_table[[col]]  # Use lookup table if match exists
    } else {
      col  # Fallback to original column name
    }
  }, USE.NAMES = TRUE)

  return(factor_labels)
}

# -----------------------------------------------------------------------------
# 2. Generate Tooltip
# -----------------------------------------------------------------------------
#' @title Generate Tooltip
#' @description Dynamically generates tooltip text for specification curve plots,
#'   combining human-readable labels and row-level information.
#' @param data A data frame containing the specification data.
#' @param factor_label_lookup A named list mapping column names to human-readable labels.
#' @return The input data frame with an additional `tooltip` column.
#' @examples
#' data <- generate_tooltip(data, factor_label_lookup)
#'
#' @importFrom stringr str_to_sentence
#'
generate_tooltip <- function(data, factor_label_lookup) {
  # Select all columns starting with "wf_"
  wf_columns <- grep("^wf_", colnames(data), value = TRUE)

  # Helper function to split long 'set' strings into multiple lines
  split_set_values <- function(set_string, chunk_size = 10) {
    set_values <- unlist(strsplit(set_string, ","))  # Split by commas
    chunks <- split(set_values, ceiling(seq_along(set_values) / chunk_size))  # Split into groups of 10
    formatted_set <- paste(sapply(chunks, paste, collapse = ","), collapse = "<br>")
    return(formatted_set)
  }

  # Dynamically create tooltip text
  data$tooltip <- apply(data, 1, function(row) {
    wf_text <- paste(sapply(wf_columns, function(col) {
      value <- as.character(row[col])
      label <- factor_label_lookup[[col]]

      # Replace "total_" values with "Combined"
      if (grepl("total_", value)) {
        paste0(label, ": Combined")
      } else {
        paste0(label, ": ", stringr::str_to_sentence(value))
      }
    }), collapse = "<br>")

    # Format the study set
    set_formatted <- split_set_values(as.character(row["set"]))

    # Construct the final tooltip text
    paste0(
      wf_text,
      "<br><b>Mean:</b> ", round(as.numeric(row["b"]), 3),
      "<br><b>CI:</b> [", round(as.numeric(row["ci.lb"]), 3), ", ", round(as.numeric(row["ci.ub"]), 3), "]",
      "<br><b>Studies (k):</b> ", row["k"],
      "<br><b>Study set (ID):</b> ", set_formatted
    )
  })

  return(data)
}

# -----------------------------------------------------------------------------
# 3. Assign  Quantile  Levels
# -----------------------------------------------------------------------------
#' @title Assign Quantile Levels
#' @description Assigns quantile-based levels to a specified column for visualization.
#' @param data A data frame containing the specification data.
#' @param k_column A string specifying the column for quantile calculation (e.g., "k").
#' @param num_levels An integer specifying the number of quantile levels (default: 9).
#' @return The input data frame with an additional `fill_levels` column.
#' @examples
#' data <- assign_quantile_levels(data, k_column = "k", num_levels = 9)
#'
#' @importFrom dplyr mutate
#' @importFrom stats quantile
#' @importFrom base cut
assign_quantile_levels <- function(data, k_column, num_levels = 9) {
  # Ensure k is numeric
  if (!is.numeric(data[[k_column]])) stop("`k_column` must be numeric.")

  # Dynamically adjust breaks for unique quantiles
  fill_quantiles <- unique(stats::quantile(data[[k_column]], probs = seq(0, 1, length.out = num_levels + 1), na.rm = TRUE))

  while (length(fill_quantiles) <= num_levels) {
    num_levels <- num_levels - 1
    fill_quantiles <- unique(stats::quantile(data[[k_column]], probs = seq(0, 1, length.out = num_levels + 1), na.rm = TRUE))
    if (num_levels < 2) stop("Insufficient unique values in `k` to create quantile levels.")
  }

  # Assign quantile levels
  data$fill_levels <- cut(
    data[[k_column]],
    breaks = fill_quantiles,
    include.lowest = TRUE,
    labels = 1:num_levels
  )

  return(data)
}
