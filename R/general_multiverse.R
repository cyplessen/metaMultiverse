#' General Multiverse Meta-Analysis Handler
#'
#' A core function for dynamically applying multiverse meta-analysis specifications to subsets
#' of data, applying "Which" factors and "How" factors based on user-specified configurations.
#' Supports both simple factor levels and custom factor groupings.
#'
#' @param i Integer. The row index in the \code{specifications} data frame, representing a single
#'   specification to be applied to the data.
#' @param data_multiverse Data frame. The dataset containing all potential studies and factors
#'   for the multiverse analysis.
#' @param specifications Data frame. Contains all specifications generated using
#'   \code{\link{create_multiverse_specifications}}. Includes columns for "Which" and "How" factors.
#'   May contain custom group names that require mapping to actual data levels.
#' @param factor_groups Optional list of custom factor groupings. When provided,
#'   enables mapping from group names in specifications to actual data levels.
#'   Structure: \code{list(wf_1 = list(group_name = c(level1, level2), ...), ...)}.
#' @param k_smallest_ma Integer. The smallest number of unique studies required for a valid meta-analysis.
#'   Defaults to 5.
#'
#' @return A data frame containing the results of the specified meta-analysis, including the:
#'   - Effect size estimate (\code{b}),
#'   - Confidence intervals (\code{ci.lb} and \code{ci.ub}),
#'   - P-value (\code{pval}),
#'   - Number of studies included in the subset (\code{k}),
#'   - Set of study IDs included in the analysis (\code{set}),
#'   - The corresponding specification parameters.
#'   Returns \code{NULL} if insufficient studies remain after filtering.
#'
#' @details
#' This function dynamically filters the input data based on "Which" factors (e.g.,
#' \code{wf_1}, \code{wf_2}, etc.) and applies "How" factors (\code{ma_method} and
#' \code{dependency}). The analysis only proceeds if the subset contains at least
#' \code{k_smallest_ma} unique studies. Supports both \code{aggregate} and \code{modeled} dependencies.
#'
#' When custom factor groupings are provided, the function maps group names in specifications
#' to actual data levels before filtering. For example, if a specification contains
#' "conservative" for \code{wf_1}, and the factor groups define
#' \code{conservative = "low risk"}, the function filters for studies where
#' \code{wf_1 == "low risk"}.
#'
#' @section Custom Factor Group Processing:
#' When custom factor groupings are provided:
#' \enumerate{
#'   \item Function examines each "Which factor" column in the specification
#'   \item If a value matches a custom group name, maps it to corresponding data levels
#'   \item Filters data to include only studies matching the mapped levels
#'   \item Falls back to direct matching for non-grouped factors
#'   \item Handles "total_*" values by including all levels (no filtering)
#' }
#'
#' @examples
#' # Standard usage without custom groupings
#' data_multiverse <- data.frame(
#'   study = rep(paste0("S", 1:5), each = 2),
#'   es_id = 1:10,
#'   yi = rnorm(10, mean = 0.5),
#'   vi = runif(10, min = 0.01, max = 0.1),
#'   wf_1 = rep(c("A", "B"), times = 5)
#' )
#' specifications <- data.frame(
#'   multiverse_id = 1:2,
#'   wf_1 = c("A", "B"),
#'   dependency = "aggregate",
#'   ma_method = "reml"
#' )
#' result <- general_multiverse(1, data_multiverse, specifications)
#'
#' # Usage with custom factor groupings
#' factor_groups <- list(
#'   wf_1 = list(
#'     "low_risk" = "low risk",
#'     "exclude_high" = c("low risk", "some concerns")
#'   )
#' )
#' specifications <- data.frame(
#'   multiverse_id = 1,
#'   wf_1 = "exclude_high",  # This will map to c("low risk", "some concerns")
#'   dependency = "aggregate",
#'   ma_method = "reml"
#' )
#' result <- general_multiverse(1, data_multiverse, specifications, factor_groups)
#'
#' @export
general_multiverse <- function(i, data_multiverse, specifications, factor_groups = NULL, k_smallest_ma = getOption("metaMultiverse.k_smallest_ma", 5)) {

  # Validate k_smallest_ma
  if (!is.numeric(k_smallest_ma) || k_smallest_ma <= 0) {
    stop("`k_smallest_ma` must be a positive numeric value.")
  }

  # Prepare output
  out <- list()
  dat <- as.data.frame(data_multiverse)

  # Apply "Which" factors dynamically with custom grouping support
  for (wf in names(specifications)[grepl("^wf_", names(specifications))]) {
    filter_value <- specifications[[wf]][i]

    if (!grepl("^total_", filter_value)) {
      # Check if this is a custom group or individual level
      if (!is.null(factor_groups) && wf %in% names(factor_groups)) {
        # Custom groupings exist for this factor
        groups <- factor_groups[[wf]]

        if (filter_value %in% names(groups)) {
          # This is a custom group - filter for multiple levels
          allowed_levels <- groups[[filter_value]]
          dat <- dat[dat[[wf]] %in% allowed_levels, ]
        } else {
          # This might be an individual level (fallback to original behavior)
          dat <- dat[dat[[wf]] == filter_value, ]
        }
      } else {
        # No custom groupings - use original behavior
        dat <- dat[dat[[wf]] == filter_value, ]
      }
    }
    # If it starts with "total_", include all levels (no filtering)
  }

  # Remove rows with missing `yi` or `vi` values
  dat <- tidyr::drop_na(dat, dplyr::any_of(c("yi", "vi")))
  set <- paste(dat$es_id, collapse = ",")

  # Only compute if at least `k_smallest_ma` unique studies
  if (length(unique(dat$study)) < k_smallest_ma) {
    warning(sprintf(
      "Specification %d skipped: only %d unique studies found (threshold = %d).",
      i, length(unique(dat$study)), k_smallest_ma
    ))
    return(NULL)
  }

  # Handle "How" factors dynamically (unchanged from original)
  dependency <- specifications$dependency[i]
  ma_method <- specifications$ma_method[i]

  if (dependency %in% c("select_max", "select_min")) {
    mod <- run_select_dependency(dat, ma_method, dependency)
  } else if (dependency == "aggregate") {
    mod <- run_aggregate_dependency(dat, ma_method)
  } else if (dependency == "modeled") {
    mod <- run_modeled_dependency(dat, ma_method)
  }

  # Return results (unchanged from original)
  out <- data.frame(
    multiverse_id = specifications$multiverse_id[i],
    specifications[i, ],
    b     = as.numeric(mod$b),
    ci.lb = as.numeric(mod$ci.lb),
    ci.ub = as.numeric(mod$ci.ub),
    pval  = as.numeric(mod$pval),
    k     = nrow(dat),
    set
  )

  return(out)
}
