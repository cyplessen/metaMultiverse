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
#' specs <- create_principled_multiverse_specifications(
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
create_principled_multiverse_specifications <- function(data,
                                             wf_vars,
                                             ma_methods,
                                             dependencies,
                                             decision_map) {
  if (!is.data.frame(data)) stop("`data` must be a data frame.")
  if (!all(wf_vars %in% names(decision_map))) {
    stop("`decision_map` must name every wf_var")
  }
  # split wf_vars by their decision type
  type_N <- names(decision_map)[decision_map == "N"]
  type_EU <- names(decision_map)[decision_map %in% c("E","U")]

  # build wf_factors differently for N vs E/U
  wf_factors <- lapply(wf_vars, function(wf) {
    vals <- unique(data[[wf]])
    if (decision_map[[wf]] == "N") {
      # non-equivalent: just the raw levels
      vals
    } else {
      # equivalent or uncertain: raw + total_…
      c(vals, paste0("total_", wf))
    }
  })
  names(wf_factors) <- wf_vars

  # sanity‐checks
  if (length(ma_methods)      < 1) stop("need at least one ma_method")
  if (length(dependencies)    < 1) stop("need at least one dependency")

  # generate the **full** cartesian grid
  grid_args <- c(wf_factors,
                 list(dependency   = dependencies,
                      ma_method    = ma_methods))
  specs <- do.call(expand.grid, grid_args)

  # at this point, each row is one spec.  But for Type N we want
  # separate multiverses—so we tag them with a multiverse_id:
  if (length(type_N) > 0) {
    specs$multiverse_id <- apply(
      specs[ , type_N, drop = FALSE],
      1, paste, collapse = "|"
    )
  } else {
    specs$multiverse_id <- "all"
  }

  # now filter out any combination that your registry says is invalid
  dep_table <- purrr::map_dfr(list_ma_methods(), function(m) {
    tibble::tibble(
      ma_method  = m,
      dependency = .ma_method_registry[[m]]$deps
    )
  })
  specs <- specs |>
    dplyr::inner_join(dep_table, by = c("ma_method","dependency")) |>
    dplyr::mutate(row_id = dplyr::row_number())

  list(
    specifications = specs,
    number_specs   = nrow(specs)
  )
}
