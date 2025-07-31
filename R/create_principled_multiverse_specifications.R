# Suppress warnings for dynamically evaluated variables
globalVariables(c("dependency"))

#' Create Specifications for Multiverse Meta-Analysis
#'
#' This function generates a grid of specifications for multiverse meta-analysis based on "Which" and "How" factors.
#' It ensures only valid combinations of meta-analytic methods and dependency handling strategies are included.
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
#'
#' @importFrom dplyr filter mutate row_number inner_join
#' @importFrom purrr map_dfr
#' @importFrom tibble tibble
#'
#' @return A list with two elements:
#' \itemize{
#'   \item \code{specifications}: A data frame where each row represents one analysis specification.
#'         Contains columns for each wf_var, plus \code{dependency}, \code{ma_method},
#'         \code{multiverse_id}, and \code{row_id}.
#'   \item \code{number_specs}: Integer. The total number of valid specifications created.
#' }
#'
#' @section Multiverse ID Logic:
#' The \code{multiverse_id} column groups specifications into separate multiverse analyses:
#' \itemize{
#'   \item Variables with decision_map = "N" create separate \code{multiverse_id} values
#'   \item Variables with decision_map = "E" or "U" vary within each multiverse
#'   \item Use \code{multiverse_id} to split results into separate analyses or plots
#' }
#'
#' @details
#' The function performs the following steps:
#' \itemize{
#'   \item Identifies all unique values for each "Which factor" in the dataset.
#'   \item For "E" and "U" factors: Appends a "total" value (e.g., \code{total_wf_1}) to allow for combinations that include all levels.
#'   \item For "N" factors: Uses only the raw levels (no "total" option) and creates separate multiverse_id values.
#'   \item Dynamically creates a grid of all possible combinations of "Which factors," meta-analytic methods (\code{ma_methods}), and dependency strategies (\code{dependencies}).
#'   \item Filters out invalid combinations based on the method registry (e.g., some methods only work with specific dependencies).
#'   \item Adds a unique row ID to each specification.
#' }
#'
#' @examples
#' # Example dataset
#' example_data <- data.frame(
#'   study = paste0("Study_", 1:6),
#'   es_id = 1:6,
#'   yi = c(0.3, 0.5, 0.7, 0.2, 0.6, 0.4),
#'   vi = c(0.02, 0.03, 0.01, 0.04, 0.02, 0.03),
#'   wf_1 = c("A", "B", "A", "C", "B", "C"),  # Population type (Non-equivalent)
#'   wf_2 = c("X", "Y", "X", "Y", "X", "Y")   # Measure type (Uncertain)
#' )
#'
#' # First validate the data
#' validated_data <- check_data_multiverse(example_data)
#'
#' # Decision map: wf_1 creates separate analyses, wf_2 creates multiverse options
#' decision_map <- c("wf_1" = "N", "wf_2" = "U")
#'
#' # Create specifications
#' specs <- create_principled_multiverse_specifications(
#'   data = validated_data,
#'   wf_vars = c("wf_1", "wf_2"),
#'   ma_methods = c("reml", "fe"),
#'   dependencies = c("aggregate"),
#'   decision_map = decision_map
#' )
#'
#' # Results: Multiple multiverse_id values for different wf_1 levels
#' table(specs$specifications$multiverse_id)
#' head(specs$specifications)
#'
#' @export
create_principled_multiverse_specifications <- function(data,
                                                        wf_vars,
                                                        ma_methods,
                                                        dependencies,
                                                        decision_map) {

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

  # Split wf_vars by their decision type
  type_N <- names(decision_map)[decision_map == "N"]
  type_EU <- names(decision_map)[decision_map %in% c("E","U")]

  # Build wf_factors differently for N vs E/U
  wf_factors <- lapply(wf_vars, function(wf) {
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

  # Informative messages
  n_multiverses <- length(unique(specs$multiverse_id))
  message("Created ", nrow(specs), " specifications across ", n_multiverses, " multiverse(s)")

  if (length(type_N) > 0) {
    message("Non-equivalent factors (", paste(type_N, collapse = ", "),
            ") created ", n_multiverses, " separate multiverse analyses")
  }

  if (length(type_EU) > 0) {
    message("Equivalent/Uncertain factors (", paste(type_EU, collapse = ", "),
            ") vary within each multiverse")
  }

  list(
    specifications = specs,
    number_specs   = nrow(specs)
  )
}
