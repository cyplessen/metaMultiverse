# =============================================================================
# SHARED INPUT HANDLING FOR plot_spec_curve() AND plot_voe()
# =============================================================================
#
# Both plot functions accept (a) a multiverse_result from
# run_multiverse_analysis(), (b) a pre_post_multiverse from
# run_pre_post_multiverse(), or (c) a plain data frame of results. This helper
# extracts the results table, decides which columns are the which/how factors
# and builds a display label for each of them.

# Default display labels for the how-factor columns the package creates
.plot_factor_default_labels <- c(
  ma_method       = "Meta-Analysis Method",
  dependency      = "Dependency Handling",
  es_metric       = "Effect-Size Metric",
  imputed_post_sd = "Imputed Post SD"
)

# Columns that the package's own results tables use as factors, in the order
# they are drawn: which factors (wf_*) first, then the how factors.
.plot_detect_factors <- function(data) {
  wf_cols <- colnames(data)[grep("^wf_", colnames(data))]
  how_cols <- colnames(data)[colnames(data) %in% c("ma_method", "dependency")]
  c(wf_cols, how_cols)
}

#' Resolve the results table and factor columns for a plot (internal helper)
#'
#' @param x A \code{multiverse_result}, a \code{pre_post_multiverse} or a data
#'   frame of results.
#' @param factors \code{NULL} or a character vector of factor column names,
#'   optionally named with display labels.
#' @param required Character vector of result columns that must be present.
#' @param use_default Logical. When \code{factors} is \code{NULL}, use the
#'   input's own factor columns (\code{TRUE}) or no factors at all
#'   (\code{FALSE}).
#' @param require_factors Logical. Error when no factor column can be found.
#'
#' @return A list with \code{data} (the results data frame, factor columns
#'   coerced to character), \code{factors} (character vector of factor column
#'   names), \code{labels} (named list of display labels, one per factor) and
#'   \code{factor_groups} (custom group definitions, or \code{NULL}).
#' @keywords internal
resolve_plot_input <- function(x, factors = NULL,
                               required = c("b", "ci.lb", "ci.ub", "pval", "k"),
                               use_default = TRUE,
                               require_factors = TRUE) {
  label_lookup <- as.list(.plot_factor_default_labels)
  factor_groups <- NULL

  if (inherits(x, "multiverse_result")) {
    data <- x$results
    default_factors <- if (is.null(data)) character(0) else .plot_detect_factors(data)
    if (!is.null(x$specifications)) {
      factor_info <- attr(x$specifications, "factor_info")
      if (!is.null(factor_info)) {
        label_lookup[factor_info$wf_internal] <- as.list(factor_info$label)
      }
    }
    factor_groups <- x$factor_groups
  } else if (inherits(x, "pre_post_multiverse")) {
    data <- x$results
    default_factors <- x$factors
  } else if (is.data.frame(x)) {
    data <- x
    default_factors <- .plot_detect_factors(data)
  } else {
    stop("Input must be a multiverse_result object from run_multiverse_analysis(), ",
         "a pre_post_multiverse object from run_pre_post_multiverse(), ",
         "or a data frame of results")
  }

  if (is.null(data) || nrow(data) == 0) {
    stop("No results to plot")
  }
  data <- as.data.frame(data)

  missing_cols <- setdiff(required, colnames(data))
  if (length(missing_cols) > 0) {
    stop("Results are missing the column(s): ", paste(missing_cols, collapse = ", "))
  }

  if (is.null(factors)) {
    factors <- if (use_default) intersect(default_factors, colnames(data)) else character(0)
    if (require_factors && length(factors) == 0) {
      stop("No factor columns found. Pass `factors = c(...)` naming the columns ",
           "that hold the which/how factor levels.")
    }
    labels <- setNames(rep(NA_character_, length(factors)), factors)
  } else {
    if (!is.character(factors) || length(factors) == 0 || anyNA(factors)) {
      stop("`factors` must be a character vector of column names")
    }
    labels <- names(factors)
    if (is.null(labels)) labels <- rep("", length(factors))
    labels[labels == ""] <- NA_character_
    factors <- unname(factors)
    unknown <- setdiff(factors, colnames(data))
    if (length(unknown) > 0) {
      stop("`factors` names column(s) not present in the results: ",
           paste(unknown, collapse = ", "))
    }
    if (anyDuplicated(factors)) {
      stop("`factors` must not contain duplicated column names")
    }
    names(labels) <- factors
  }

  factor_labels <- list()
  for (col in factors) {
    factor_labels[[col]] <- if (!is.na(labels[[col]])) {
      labels[[col]]
    } else if (!is.null(label_lookup[[col]])) {
      label_lookup[[col]]
    } else {
      stringr::str_to_title(gsub("_", " ", col))
    }
    data[[col]] <- as.character(data[[col]])
  }

  list(data = data, factors = factors, labels = factor_labels, factor_groups = factor_groups)
}
