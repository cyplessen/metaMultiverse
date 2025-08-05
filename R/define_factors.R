#' Define Which Factors with Advanced Grouping Options
#'
#' Enhanced function for setting up which factors with sophisticated control over
#' how factor levels are grouped and analyzed in multiverse meta-analysis.
#'
#' @param data Your dataset
#' @param ... Factor definitions using flexible syntax (see details and examples)
#'
#' @details
#' Each factor can be defined using several approaches:
#'
#' **Simple factors:**
#' - `Population = "pop_column"` (multiverse with individual levels + all combined)
#' - `Population = "pop_column|U"` (same as above, U = uncertain which is best)
#' - `Population = "pop_column|N"` (separate analyses for each level)
#' - `Population = "pop_column|E"` (equivalent levels, includes all combined option)
#'
#' **Custom groupings with decision types:**
#' - `Risk_of_Bias = list("risk_column", decision = "U", groups = list(...))`
#' - `Risk_of_Bias = list("risk_column", decision = "N", groups = list(...))`
#'
#'   Where decision types mean:
#'   - **U** (Uncertain): "Which inclusion criteria should I use?" → creates multiverse
#'   - **N** (Non-equivalent): "These are different research questions" → separate analyses
#'   - **E** (Equivalent): "These measure the same thing" → creates multiverse (rare for custom groups)
#'
#' **Ordered factors with decision types:**
#' - `Study_Quality = list("quality_column", type = "ordered", decision = "U", levels = c("high", "medium", "low"))`
#'
#' **Binary combinations with decision types:**
#' - `Guidance = list("guidance_column", type = "binary", decision = "N")`
#'   Each level + all combined, with decision type controlling analysis structure
#'
#' @examples
#' # Simple usage
#' setup <- define_factors(data,
#'   Population = "population_type",
#'   Design = "study_design|N"
#' )
#'
#' # Advanced usage with custom groupings
#' setup <- define_factors(data,
#'   # Custom risk of bias combinations
#'   Risk_of_Bias = list("risk_of_bias",
#'     groups = list(
#'       "low_risk_only" = "low risk",
#'       "exclude_high_risk" = c("low risk", "some concerns"),
#'       "all_studies" = c("low risk", "some concerns", "high risk")
#'     )
#'   ),
#'
#'   # Ordered quality levels (cumulative)
#'   Study_Quality = list("methodological_quality",
#'     type = "ordered",
#'     levels = c("high", "medium", "low")
#'   ),
#'
#'   # Separate analyses for different populations
#'   Population = "population_type|N",
#'
#'   # Standard multiverse for guidance
#'   Guidance = "guidance_level"
#' )
#'
#' @export
define_factors <- function(data, ...) {

  if (!is.data.frame(data)) {
    stop("Please provide a data frame")
  }

  factor_args <- list(...)
  if (length(factor_args) == 0) {
    stop("Please specify at least one factor. Example:\n  define_factors(mydata, Population = 'pop_column')")
  }

  # Process each factor definition
  factor_info <- data.frame(
    label = character(),
    column = character(),
    decision = character(),
    wf_internal = character(),
    grouping_type = character(),
    stringsAsFactors = FALSE
  )

  factor_groups <- list()  # Store custom grouping information

  for (i in seq_along(factor_args)) {
    factor_name <- names(factor_args)[i]
    factor_def <- factor_args[[i]]

    if (is.character(factor_def) && length(factor_def) == 1) {
      # Simple definition: "column_name" or "column_name|X"
      result <- parse_simple_factor(factor_def, factor_name, data)

    } else if (is.list(factor_def)) {
      # Advanced definition with custom options
      result <- parse_advanced_factor(factor_def, factor_name, data)

    } else {
      stop("Factor '", factor_name, "' definition not recognized. Use string or list format.")
    }

    # Add to factor_info
    factor_info <- rbind(factor_info, result$info)

    # Store grouping information if present
    if (!is.null(result$groups)) {
      factor_groups[[paste0("wf_", i)]] <- result$groups  # Use temporary key
    }
  }

  # Add the wf_* columns to data FIRST
  data_with_wf <- data
  for (i in seq_len(nrow(factor_info))) {
    col_name <- factor_info$column[i]
    wf_name <- paste0("wf_", i)  # Create wf_1, wf_2, etc.
    data_with_wf[[wf_name]] <- as.character(data_with_wf[[col_name]])
  }

  # THEN create internal mapping
  factor_info$wf_internal <- paste0("wf_", seq_len(nrow(factor_info)))

  # Create decision map for internal use
  decision_map <- setNames(factor_info$decision, factor_info$wf_internal)

  result <- structure(
    list(
      data = data_with_wf,
      factors = factor_info,
      decision_map = decision_map,
      factor_groups = factor_groups
    ),
    class = "multiverse_factor_setup"
  )

  # CRITICAL FIX: Re-key factor_groups with correct wf_* names
  if (length(factor_groups) > 0) {
    # Create properly keyed factor_groups
    properly_keyed_groups <- list()
    for (i in seq_len(nrow(factor_info))) {
      temp_key <- paste0("wf_", i)  # This matches how we stored them
      if (temp_key %in% names(factor_groups)) {
        wf_name <- factor_info$wf_internal[i]  # This is the correct wf_* name
        properly_keyed_groups[[wf_name]] <- factor_groups[[temp_key]]
      }
    }
    result$factor_groups <- properly_keyed_groups
  }

  # User-friendly summary
  print_factor_summary(factor_info, result$factor_groups, data)

  return(result)
}

#' Parse simple factor definition
#' @keywords internal
parse_simple_factor <- function(factor_def, factor_name, data) {

  if (grepl("\\|.+$", factor_def)) {  # Changed: Match any character after |, not just [EUN]
    # Has decision type specified
    parts <- strsplit(factor_def, "\\|")[[1]]
    column_name <- parts[1]
    decision_type <- parts[2]

    # Validate decision type BEFORE checking column
    if (!decision_type %in% c("E", "U", "N")) {
      stop("Decision type must be 'E', 'U', or 'N' for factor '", factor_name, "'. Found: '", decision_type, "'")
    }
  } else {
    # Default to uncertain
    column_name <- factor_def
    decision_type <- "U"
  }

  # Check column exists
  if (!column_name %in% names(data)) {
    stop("Column '", column_name, "' not found in data")
  }

  info <- data.frame(
    label = factor_name,
    column = column_name,
    decision = decision_type,
    wf_internal = "",  # Will be filled later
    grouping_type = "simple",
    stringsAsFactors = FALSE
  )

  return(list(info = info, groups = NULL))
}


#' Parse advanced factor definition
#' @keywords internal
parse_advanced_factor <- function(factor_def, factor_name, data) {

  # Extract column name (first element)
  if (is.null(factor_def[[1]]) || !is.character(factor_def[[1]])) {
    stop("First element of list must be column name for factor '", factor_name, "'")
  }

  column_name <- factor_def[[1]]

  # Check column exists
  if (!column_name %in% names(data)) {
    stop("Column '", column_name, "' not found in data")
  }

  # Extract decision type (default to U if not specified)
  decision_type <- if ("decision" %in% names(factor_def)) {
    factor_def$decision
  } else {
    "U"  # Default to Uncertain
  }

  # Validate decision type
  if (!decision_type %in% c("E", "U", "N")) {
    stop("Decision type must be 'E', 'U', or 'N' for factor '", factor_name, "'")
  }

  # Get unique levels in data
  available_levels <- unique(data[[column_name]])
  available_levels <- available_levels[!is.na(available_levels)]

  # Parse different types of advanced definitions
  if ("groups" %in% names(factor_def)) {
    # Custom groupings specified
    result <- parse_custom_groups(factor_def, factor_name, column_name, available_levels, decision_type)

  } else if ("type" %in% names(factor_def)) {
    # Special type specified (ordered, binary, etc.)
    result <- parse_special_type(factor_def, factor_name, column_name, available_levels, decision_type)

  } else {
    stop("Advanced factor definition must include either 'groups' or 'type' parameter")
  }

  return(result)
}

#' Parse custom group definitions
#' @keywords internal
parse_custom_groups <- function(factor_def, factor_name, column_name, available_levels, decision_type) {

  groups <- factor_def$groups
  if (!is.list(groups) || is.null(names(groups))) {
    stop("'groups' must be a named list for factor '", factor_name, "'")
  }

  # Validate that all specified levels exist in data
  all_specified <- unlist(groups)
  missing_levels <- setdiff(all_specified, available_levels)
  if (length(missing_levels) > 0) {
    stop("Levels not found in data for '", factor_name, "': ", paste(missing_levels, collapse = ", "))
  }

  info <- data.frame(
    label = factor_name,
    column = column_name,
    decision = decision_type,  # Use the provided decision type
    wf_internal = "",
    grouping_type = "custom",
    stringsAsFactors = FALSE
  )

  return(list(info = info, groups = groups))
}

#' Parse special type definitions (ordered, binary, etc.)
#' @keywords internal
parse_special_type <- function(factor_def, factor_name, column_name, available_levels, decision_type) {

  type <- factor_def$type

  if (type == "ordered") {
    # Ordered/cumulative inclusion
    if ("levels" %in% names(factor_def)) {
      ordered_levels <- factor_def$levels
      # Validate levels exist
      missing <- setdiff(ordered_levels, available_levels)
      if (length(missing) > 0) {
        stop("Ordered levels not found in data: ", paste(missing, collapse = ", "))
      }
    } else {
      # Use all available levels (user should specify order)
      warning("No level order specified for ordered factor '", factor_name, "'. Using alphabetical order.")
      ordered_levels <- sort(available_levels)
    }

    # Create cumulative groups
    groups <- list()
    for (i in seq_along(ordered_levels)) {
      group_name <- paste("up_to", gsub(" ", "_", ordered_levels[i]), sep = "_")
      groups[[group_name]] <- ordered_levels[1:i]
    }

    info <- data.frame(
      label = factor_name,
      column = column_name,
      decision = decision_type,  # Use provided decision type
      wf_internal = "",
      grouping_type = "ordered",
      stringsAsFactors = FALSE
    )

    return(list(info = info, groups = groups))

  } else if (type == "binary") {
    # Each level individually + all combined
    groups <- as.list(available_levels)
    names(groups) <- paste0("only_", gsub(" ", "_", available_levels))
    groups[["all_levels"]] <- available_levels

    info <- data.frame(
      label = factor_name,
      column = column_name,
      decision = decision_type,  # Use provided decision type
      wf_internal = "",
      grouping_type = "binary",
      stringsAsFactors = FALSE
    )

    return(list(info = info, groups = groups))

  } else {
    stop("Unknown type '", type, "' for factor '", factor_name, "'")
  }
}

#' Print user-friendly summary of factor setup
#' @keywords internal
print_factor_summary <- function(factor_info, factor_groups, data) {

  cat("✓ Set up", nrow(factor_info), "factors for multiverse analysis:\n\n")

  for (i in seq_len(nrow(factor_info))) {
    factor <- factor_info[i, ]
    cat("  ", factor$label, ":", factor$column)

    # Show decision type meaning
    decision_meaning <- switch(factor$decision,
                               "E" = "(Equivalent - will include multiverse options)",
                               "U" = "(Uncertain - will create multiverse options)",
                               "N" = "(Non-equivalent - will create separate analyses)"
    )

    if (factor$grouping_type == "simple") {
      cat(" ", decision_meaning, "\n")

      # Show available levels
      levels <- unique(data[[factor$column]])
      levels <- levels[!is.na(levels)]
      cat("    Levels:", paste(levels, collapse = ", "), "\n")

    } else if (factor$grouping_type %in% c("custom", "ordered", "binary")) {
      cat(" ", decision_meaning, "\n")

      wf_name <- factor$wf_internal
      if (wf_name %in% names(factor_groups)) {
        groups <- factor_groups[[wf_name]]
        for (group_name in names(groups)) {
          cat("    ", group_name, ":", paste(groups[[group_name]], collapse = ", "), "\n")
        }
      }
    }

    cat("\n")
  }

  cat("Next step: run_multiverse(setup, methods = c('reml', 'fe'))\n")
}
