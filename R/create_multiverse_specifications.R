# Suppress warnings for dynamically evaluated variables
globalVariables(c("dependency"))

#' Create Specifications for Multiverse Meta-Analysis
#'
#' This function generates a grid of specifications for multiverse meta-analysis based on "Which" and "How" factors.
#' It ensures only valid combinations of meta-analytic methods and dependency handling strategies are included.
#'
#' @param data A data frame containing the dataset to base the specifications on.
#' @param wf_vars A character vector specifying the "Which factors" (e.g., columns like `wf_1`, `wf_2`) to include in the specifications.
#' @param ma_methods A character vector specifying the meta-analytic methods to consider (e.g., `"reml"`, `"fe"`, `"3-level"`, `"rve"`, `"p-uniform"`, etc.).
#' @param dependencies A character vector specifying dependency handling strategies to include (e.g., `"modeled"`, `"aggregate"``).
#'
#' @importFrom dplyr filter mutate row_number %>%
#'
#' @return A list with two elements:
#' \itemize{
#'   \item \code{specifications}: A data frame containing the grid of valid specifications.
#'   \item \code{number_specs}: The total number of valid specifications.
#' }
#'
#' @details
#' The function performs the following steps:
#' \itemize{
#'   \item Identifies all unique values for each "Which factor" in the dataset.
#'   \item Appends a "total" value (e.g., `total_wf_1`) to each "Which factor" to allow for combinations that include all levels.
#'   \item Dynamically creates a grid of all possible combinations of "Which factors," meta-analytic methods (`ma_methods`), and dependency strategies (`dependencies`).
#'   \item Filters out invalid combinations based on logical constraints (e.g., some methods only work with specific dependencies).
#'   \item Adds a unique row ID to each specification.
#' }
#'
#' @examples
#' # Example dataset
#' example_data <- data.frame(
#'   wf_1 = c("A", "B", "C"),
#'   wf_2 = c("X", "Y", "Z")
#' )
#'
#' # Define parameters
#' wf_vars <- c("wf_1", "wf_2")
#' ma_methods <- c("reml", "fe", "3-level", "rve")
#' dependencies <- c("modeled", "aggregate")
#'
#' # Create specifications
#' specs <- create_multiverse_specifications(
#'   data = example_data,
#'   wf_vars = wf_vars,
#'   ma_methods = ma_methods,
#'   dependencies = dependencies
#' )
#'
#' # View results
#' head(specs$specifications)
#' print(specs$number_specs)
#'
#' @export
create_multiverse_specifications <- function(data, wf_vars, ma_methods, dependencies, decision_map) {
  # Input validation
  if (!is.data.frame(data)) stop("`data` must be a data frame.")
  if (!all(wf_vars %in% colnames(data))) stop("Not all `wf_vars` are present in the dataset.")

  # Create "Which factors"
  wf_factors <- lapply(wf_vars, function(wf) {
    unique_values <- unique(data[[wf]])
    c(unique_values, paste0("total_", wf))
  })

  names(wf_factors) <- wf_vars

  # Validate "How factors"
  if (length(ma_methods) == 0) stop("`ma_methods` must include at least one method.")
  if (length(dependencies) == 0) stop("`dependencies` must include at least one dependency.")

  # Dynamically build the expand.grid call
  grid_args <- c(wf_factors, list(dependency = dependencies, ma_method = ma_methods))

  # Generate the specifications grid
  specifications_grid <- do.call(expand.grid, grid_args)

  dep_table <- purrr::map_dfr(list_ma_methods(), function(m) {
    tibble::tibble(
      ma_method  = m,
      dependency = .ma_method_registry[[m]]$deps
    )
  })

  specifications_grid <- specifications_grid |>
    dplyr::mutate(
      ma_method  = as.character(ma_method),   # drop factor levels
      dependency = as.character(dependency)
    ) |>
    dplyr::inner_join(dep_table, by = c("ma_method", "dependency")) |>
    dplyr::mutate(row_id = dplyr::row_number())

  # Return final specifications grid
  specifications <- data.frame(specifications_grid)
  number_specs <- nrow(specifications)

  list(
    specifications = specifications,
    number_specs = number_specs
  )
}
