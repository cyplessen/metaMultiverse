#' Define Which Factors for Multiverse Analysis
#'
#' Defines study selection criteria ("which factors") with automatic detection
#' of simple vs. custom factor specifications. Creates the foundation for
#' multiverse analysis by mapping data columns to analytical choices.
#'
#' @param data Data frame containing meta-analysis data with which factor columns
#' @param ... Factor definitions using either simple or custom syntax (see Details)
#'
#' @return Object of class \code{multiverse_factor_setup} containing:
#' \describe{
#'   \item{data}{Original data with added wf_* columns}
#'   \item{factors}{Data frame describing each factor:
#'     \itemize{
#'       \item \code{label}: User-provided factor name
#'       \item \code{column}: Original data column
#'       \item \code{decision}: Decision type (E/U/N)
#'       \item \code{wf_internal}: Internal wf_* column name
#'       \item \code{grouping_type}: "simple" or "custom"
#'     }
#'   }
#'   \item{decision_map}{Named vector mapping wf_* columns to decision types}
#'   \item{factor_groups}{List of custom group definitions (if any)}
#' }
#'
#' @details
#' \strong{Simple factors} (string):
#' \itemize{
#'   \item \code{"column_name"} - Uses each unique level + "all combined"
#'   \item \code{"column_name|E"} - Adds decision type (E/U/N)
#' }
#'
#' \strong{Custom factors} (list):
#' \preformatted{list(
#'   "column_name",
#'   decision = "U",  # Optional, default: "U"
#'   groups = list(
#'     group_name = c("level1", "level2"),
#'     ...
#'   )
#' )}
#'
#' \strong{Decision types:}
#' \itemize{
#'   \item \code{E} (Equivalent): All options theoretically equivalent
#'   \item \code{U} (Uncertain): Unclear which option is best (default)
#'   \item \code{N} (Non-equivalent): Options represent different research questions
#' }
#'
#' Type N factors create separate multiverses; types E and U vary within multiverses.
#'
#' @examples
#' \dontrun{
#' # Simple factors
#' setup <- define_factors(
#'   data,
#'   Population = "population_type",      # Default: U
#'   Measure = "outcome_measure|E"        # Explicit: E
#' )
#'
#' # Mixed simple and custom
#' setup <- define_factors(
#'   data,
#'   Population = "population_type|E",
#'   Risk_of_Bias = list(
#'     "rob_rating",
#'     decision = "U",
#'     groups = list(
#'       low_only = "low",
#'       low_moderate = c("low", "moderate"),
#'       all_studies = c("low", "moderate", "high")
#'     )
#'   ),
#'   Study_Design = "design_type|N"  # Creates separate multiverses
#' )
#' }
#'
#' @seealso
#' \code{\link{create_multiverse_specifications}} for next pipeline step
#' \code{\link{check_data_multiverse}} for data validation
#'
#' @export
define_factors <- function(data, ...) {

  if (!is.data.frame(data)) {
    stop("Please provide a data frame")
  }

  factor_args <- list(...)
  if (length(factor_args) == 0) {
    stop("Please specify at least one factor")
  }

  # Initialize results
  factor_info <- data.frame(
    label = character(),
    column = character(),
    decision = character(),
    wf_internal = character(),
    grouping_type = character(),
    stringsAsFactors = FALSE
  )

  factor_groups <- list()
  data_with_wf <- data

  # Process each factor
  for (i in seq_along(factor_args)) {
    factor_name <- names(factor_args)[i]
    factor_def <- factor_args[[i]]

    # Detect type based on input
    if (is.character(factor_def) && length(factor_def) == 1) {
      # SIMPLE: Just a column name (with optional decision type)
      result <- process_simple_factor(factor_def, factor_name, data, i)

    } else if (is.list(factor_def)) {
      # CUSTOM: List with groups
      result <- process_custom_factor(factor_def, factor_name, data, i)

    } else {
      stop("Factor '", factor_name, "' must be either a string (simple) or list (custom)")
    }

    # Add to results
    factor_info <- rbind(factor_info, result$info)

    if (!is.null(result$groups)) {
      factor_groups[[result$info$wf_internal]] <- result$groups
    }

    # Add wf column to data
    data_with_wf[[result$info$wf_internal]] <- as.character(data[[result$info$column]])
  }

  # Create decision map
  decision_map <- setNames(factor_info$decision, factor_info$wf_internal)

  result <- list(
    data = data_with_wf,
    factors = factor_info,
    decision_map = decision_map,
    factor_groups = factor_groups
  )

  class(result) <- "multiverse_factor_setup"

  # Print summary
  print_factor_summary(result)

  return(result)
}


#' Process simple factor (internal)
#' @keywords internal
process_simple_factor <- function(column_def, factor_name, data, index) {

  # Parse column name and decision type
  parts <- strsplit(column_def, "\\|")[[1]]
  column_name <- trimws(parts[1])
  decision_type <- if (length(parts) > 1) trimws(parts[2]) else "U"

  # Validate
  if (!column_name %in% names(data)) {
    stop("Column '", column_name, "' not found in data")
  }

  if (!decision_type %in% c("E", "U", "N")) {
    stop("Decision type must be 'E', 'U', or 'N' for factor '", factor_name, "'")
  }

  info <- data.frame(
    label = factor_name,
    column = column_name,
    decision = decision_type,
    wf_internal = paste0("wf_", index),
    grouping_type = "simple",
    stringsAsFactors = FALSE
  )

  return(list(info = info, groups = NULL))
}


#' Process custom factor (internal)
#' @keywords internal
process_custom_factor <- function(factor_def, factor_name, data, index) {

  # Extract column name
  if (is.null(factor_def[[1]]) || !is.character(factor_def[[1]])) {
    stop("First element must be column name for factor '", factor_name, "'")
  }

  column_name <- factor_def[[1]]

  # Check column exists
  if (!column_name %in% names(data)) {
    stop("Column '", column_name, "' not found in data")
  }

  # Get decision type
  decision_type <- if (!is.null(factor_def$decision)) {
    factor_def$decision
  } else {
    "U"  # Default
  }

  if (!decision_type %in% c("E", "U", "N")) {
    stop("Decision type must be 'E', 'U', or 'N' for factor '", factor_name, "'")
  }

  # Get groups
  if (is.null(factor_def$groups)) {
    stop("Custom factor '", factor_name, "' must include 'groups'")
  }

  groups <- factor_def$groups
  if (!is.list(groups) || is.null(names(groups))) {
    stop("'groups' must be a named list for factor '", factor_name, "'")
  }

  # Validate group levels exist in data
  available_levels <- unique(data[[column_name]])
  available_levels <- available_levels[!is.na(available_levels)]

  all_specified <- unique(unlist(groups))
  missing_levels <- setdiff(all_specified, available_levels)

  if (length(missing_levels) > 0) {
    stop("Levels not found in data for '", factor_name, "': ",
         paste(missing_levels, collapse = ", "))
  }

  info <- data.frame(
    label = factor_name,
    column = column_name,
    decision = decision_type,
    wf_internal = paste0("wf_", index),
    grouping_type = "custom",
    stringsAsFactors = FALSE
  )

  return(list(info = info, groups = groups))
}


#' Print factor summary (internal)
#' @keywords internal
print_factor_summary <- function(setup) {
  cat("\n[OK] Factor setup complete\n")
  cat("========================================================\n")

  for (i in seq_len(nrow(setup$factors))) {
    factor <- setup$factors[i, ]

    # Factor name and type
    cat(sprintf("[*] %s (%s)\n", factor$label, factor$grouping_type))

    # Column and decision
    decision_desc <- if (factor$decision == "E") {
      "(Equivalent - will include multiverse options)"
    } else if (factor$decision == "U") {
      "(Uncertain - will create multiverse options)"
    } else if (factor$decision == "N") {
      "(Non-equivalent - will create separate analyses)"
    } else {
      ""
    }

    cat(sprintf("   Column: %s | Decision: %s %s\n",
                factor$column,
                factor$decision,
                decision_desc))

    # Show groups if custom
    if (factor$grouping_type == "custom" && !is.null(setup$factor_groups[[factor$wf_internal]])) {
      groups <- setup$factor_groups[[factor$wf_internal]]
      cat("   Groups:\n")
      for (g in names(groups)) {
        cat(sprintf("     - %s: %s\n", g, paste(groups[[g]], collapse = ", ")))
      }
    } else if (factor$grouping_type == "simple") {
      levels <- unique(setup$data[[factor$column]])
      levels <- levels[!is.na(levels)]
      cat(sprintf("   Levels: %s (+ all combined)\n", paste(levels, collapse = ", ")))
    }

    cat("\n")
  }

  # Summary
  n_simple <- sum(setup$factors$grouping_type == "simple")
  n_custom <- sum(setup$factors$grouping_type == "custom")

  cat("========================================================\n")
  cat(sprintf("Total: %d factors (%d simple, %d custom)\n\n",
              nrow(setup$factors), n_simple, n_custom))
}
