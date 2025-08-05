# Suppress warnings for dynamically evaluated variables
globalVariables(c("dependency"))

#' Create Specifications for Multiverse Meta-Analysis
#'
#' This function generates a grid of specifications for multiverse meta-analysis based on "Which" and "How" factors.
#' It ensures only valid combinations of meta-analytic methods and dependency handling strategies are included.
#' Supports both simple factor definitions and advanced custom factor groupings.
#' Data must be validated using \code{check_data_multiverse()} before using this function.
#'
#' @param data A data frame containing the dataset to base the specifications on.
#'   Must be previously validated using \code{check_data_multiverse()}.
#' @param wf_vars A character vector specifying the "Which factors" (e.g., columns like \code{wf_1}, \code{wf_2}) to include in the specifications.
#' @param ma_methods A character vector specifying the meta-analytic methods to consider (e.g., \code{"reml"}, \code{"fe"}, \code{"3-level"}, \code{"rve"}, \code{"p-uniform"}, etc.).
#' @param dependencies A character vector specifying dependency handling strategies to include (e.g., \code{"modeled"}, \code{"aggregate"}).
#' @param decision_map A named character vector or list mapping each wf_var to a decision type:
#'   \itemize{
#'     \item \code{"E"} (Equivalent): Different levels are theoretically equivalent -
#'           creates multiverse with individual levels + "total_wf_X" option
#'     \item \code{"U"} (Uncertain): Unclear which level is best -
#'           creates multiverse with individual levels + "total_wf_X" option
#'     \item \code{"N"} (Non-equivalent): Different levels represent distinct research questions -
#'           creates separate multiverse analyses (different multiverse_id values)
#'   }
#' @param factor_groups Optional list of custom factor groupings. When provided, uses custom group names
#'   instead of raw data levels for specification creation. Structure:
#'   \code{list(wf_1 = list(group_name = c(level1, level2), ...), ...)}.
#'   Enables sophisticated study selection criteria beyond simple factor levels.
#'
#' @export
create_principled_multiverse_specifications <- function(data,
                                                        wf_vars,
                                                        ma_methods,
                                                        dependencies,
                                                        decision_map,
                                                        factor_groups = NULL) {

  # Check if data has been validated (look for validation attribute)
  if (is.null(attr(data, "multiverse_validated"))) {
    stop("Data must be validated first. Please run:\n",
         "  validated_data <- check_data_multiverse(data)\n",
         "  # Then retry this function with validated_data")
  }

  # Basic sanity checks for immediate failures
  if (!is.data.frame(data)) stop("`data` must be a data frame")
  if (nrow(data) == 0) stop("`data` cannot be empty")
  if (length(wf_vars) == 0) stop("`wf_vars` cannot be empty")

  # Check wf_vars exist in data
  if (!all(wf_vars %in% colnames(data))) {
    missing <- setdiff(wf_vars, colnames(data))
    stop("wf_vars not found in data: ", paste(missing, collapse = ", "))
  }

  # Validate decision_map parameter
  if (!is.list(decision_map) && !is.character(decision_map)) {
    stop("`decision_map` must be a named list or character vector")
  }
  if (!all(wf_vars %in% names(decision_map))) {
    missing <- setdiff(wf_vars, names(decision_map))
    stop("`decision_map` must contain entries for all wf_vars: ", paste(missing, collapse = ", "))
  }
  if (!all(decision_map %in% c("E", "U", "N"))) {
    invalid <- unique(decision_map[!decision_map %in% c("E", "U", "N")])
    stop("`decision_map` values must be 'E', 'U', or 'N'. Found: ", paste(invalid, collapse = ", "))
  }

  # Validate methods and dependencies
  if (length(ma_methods) < 1) stop("need at least one ma_method")
  if (length(dependencies) < 1) stop("need at least one dependency")

  # Validate ma_methods against registry
  available_methods <- list_ma_methods()
  invalid_methods <- setdiff(ma_methods, available_methods)
  if (length(invalid_methods) > 0) {
    stop("Invalid ma_methods: ", paste(invalid_methods, collapse = ", "),
         "\nAvailable methods: ", paste(available_methods, collapse = ", "))
  }

  # Build wf_factors with custom group support
  wf_factors <- lapply(wf_vars, function(wf) {

    # Check if this factor has custom groups
    if (!is.null(factor_groups) && wf %in% names(factor_groups)) {
      # Use custom group names instead of data levels
      group_names <- names(factor_groups[[wf]])
      if (length(group_names) == 0) {
        stop("Factor '", wf, "' has empty custom groups")
      }
      return(group_names)
    }

    # Standard processing: use actual data levels
    vals <- unique(data[[wf]])

    # Check for empty or NA values
    if (length(vals) == 0) {
      stop("Variable '", wf, "' has no unique values")
    }
    if (any(is.na(vals))) {
      warning("Variable '", wf, "' contains NA values which will be included in specifications")
    }

    if (decision_map[[wf]] == "N") {
      # Non-equivalent: just the raw levels
      vals
    } else {
      # Equivalent or uncertain: raw + total_…
      c(vals, paste0("total_", wf))
    }
  })
  names(wf_factors) <- wf_vars

  # Generate the full cartesian grid
  grid_args <- c(wf_factors,
                 list(dependency   = dependencies,
                      ma_method    = ma_methods))
  specs <- do.call(expand.grid, grid_args)

  # Create multiverse IDs based on N-type factors
  type_N <- names(decision_map)[decision_map == "N"]
  if (length(type_N) > 0) {
    specs$multiverse_id <- apply(
      specs[ , type_N, drop = FALSE],
      1, paste, collapse = "|"
    )
  } else {
    specs$multiverse_id <- "all"
  }

  # Filter out invalid combinations based on registry
  dep_table <- purrr::map_dfr(list_ma_methods(), function(m) {
    tibble::tibble(
      ma_method  = m,
      dependency = .ma_method_registry[[m]]$deps
    )
  })

  specs <- specs |>
    dplyr::inner_join(dep_table, by = c("ma_method","dependency")) |>
    dplyr::mutate(row_id = dplyr::row_number())

  # Store factor_groups as attribute for downstream processing
  if (!is.null(factor_groups)) {
    attr(specs, "factor_groups") <- factor_groups
  }

  # Informative messages
  n_multiverses <- length(unique(specs$multiverse_id))
  message("Created ", nrow(specs), " specifications across ", n_multiverses, " multiverse(s)")

  if (length(type_N) > 0) {
    message("Non-equivalent factors (", paste(type_N, collapse = ", "),
            ") created ", n_multiverses, " separate multiverse analyses")
  }

  type_EU <- names(decision_map)[decision_map %in% c("E", "U")]
  if (length(type_EU) > 0) {
    message("Equivalent/Uncertain factors (", paste(type_EU, collapse = ", "),
            ") vary within each multiverse")
  }

  if (!is.null(factor_groups)) {
    message("Using custom factor groupings for ", length(factor_groups), " factor(s)")
  }

  list(
    specifications = specs,
    number_specs   = nrow(specs)
  )
}
