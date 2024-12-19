#' General Multiverse Meta-Analysis Handler
#'
#' A core function for dynamically applying multiverse meta-analysis specifications to subsets
#' of data, applying "Which" factors and "How" factors based on user-specified configurations.
#'
#' @param i Integer. The row index in the `specifications` data frame, representing a single
#'   specification to be applied to the data.
#' @param data_multiverse Data frame. The dataset containing all potential studies and factors
#'   for the multiverse analysis.
#' @param specifications Data frame. Contains all specifications generated using
#'   \code{\link{create_multiverse_specifications}}. Includes columns for "Which" and "How" factors.
#' @param how_methods Character vector. A list of meta-analysis methods to apply for the "How" factors.
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
#'
#' @details
#' This function dynamically filters the input data based on "Which" factors (e.g.,
#' \code{wf_1}, \code{wf_2}, etc.) and applies "How" factors (\code{ma_method} and
#' \code{dependency}). The analysis only proceeds if the subset contains at least
#' \code{k_smallest_ma} unique studies. Supports both \code{aggregate} and \code{modeled} dependencies.
#'
#' @seealso \code{\link{create_multiverse_specifications}}, \code{\link{run_aggregate_dependency}},
#'   \code{\link{run_modeled_dependency}}
#'
#' @examples
#' # Example of applying a specification:
#' result <- general_multiverse(
#'   i = 1,
#'   data_multiverse = my_data,
#'   specifications = my_specifications,
#'   how_methods = c("reml", "fe", "p-uniform"),
#'   k_smallest_ma = 3
#' )
#'
#' @export
general_multiverse <- function(i, data_multiverse, specifications, how_methods, k_smallest_ma = 5) {
  # Validate k_smallest_ma
  if (!is.numeric(k_smallest_ma) || k_smallest_ma <= 0) {
    stop("`k_smallest_ma` must be a positive numeric value.")
  }

  # Prepare output
  out <- list()
  dat <- as.data.frame(data_multiverse)

  # Apply "Which" factors dynamically
  for (wf in names(specifications)[grepl("^wf_", names(specifications))]) {
    filter_value <- specifications[[wf]][i]
    if (!grepl("^total_", filter_value)) {
      dat <- dat[dat[[wf]] == filter_value, ]
    }
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

  # Handle "How" factors dynamically
  dependency <- specifications$dependency[i]
  ma_method <- specifications$ma_method[i]

  # Aggregating dependency
  if (dependency == "aggregate") {
    mod <- run_aggregate_dependency(dat, ma_method, how_methods)
  }

  # Modeling dependency
  else if (dependency == "modeled") {
    mod <- run_modeled_dependency(dat, ma_method, how_methods)
  }

  # Return results
  out <- data.frame(
    specifications[i, ],
    b = mod$b[[1]],
    ci.lb = mod$ci.lb[[1]],
    ci.ub = mod$ci.ub[[1]],
    pval = mod$pval[[1]],
    k = nrow(dat),
    set
  )

  return(out)
}
