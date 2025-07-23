# helpers_dependencies.R
# This file contains functions to handle aggregate and modeled dependencies in multiverse meta-analysis.

## Aggregate Dependency -------------------------------------------------------

#' Run Aggregate Dependency
#'
#' Aggregates the data by clusters (studies) and applies the selected meta-analysis method.
#'
#' @param dat A data frame containing `yi` (effect size), `vi` (variance), and `study` columns.
#' @param ma_method The meta-analysis method to apply (e.g., "fe", "reml", "uwls").
#' @return A list containing the meta-analysis results or a standardized NA list if the method fails.
run_aggregate_dependency <- function(dat, ma_method, how_methods) {
  # Step 1: Compute effect sizes
  dat <- metafor::escalc(yi = yi, vi = vi, data = dat)

  # Step 2: Aggregate data by cluster (study)
  dat <- metafor::aggregate.escalc(
    dat,
    cluster = study,
    struct = "CS",  # Compound symmetric structure
    rho = 0.5
  )

  entry <- .ma_method_registry[[ma_method]]
  if (is.null(entry) || !("aggregate" %in% entry$deps))
    return(list(b = NA, ci.lb = NA, ci.ub = NA, pval = NA))

  entry$fun(dat)
}

## Modeled Dependency ---------------------------------------------------------

#' Run Modeled Dependency
#'
#' Applies a multilevel (modeled) dependency approach using a random-effects model.
#'
#' @param dat A data frame containing `yi` (effect size), `vi` (variance), `es_id` (effect size ID), and `study` columns.
#' @param ma_method The meta-analysis method to apply (e.g., "3-level", "rve").
#' @return A list containing the meta-analysis results or a standardized NA list if the method fails.
run_modeled_dependency <- function(dat, ma_method) {

  # need ≥2 effect sizes in at least one study, otherwise nothing to model
  if (sum(duplicated(dat$study)) < 1)
    return(list(b = NA, ci.lb = NA, ci.ub = NA, pval = NA))

  entry <- .ma_method_registry[[ma_method]]

  # either the key is unknown, or the method isn't flagged for "modeled"
  if (is.null(entry) || !("modeled" %in% entry$deps))
    return(list(b = NA, ci.lb = NA, ci.ub = NA, pval = NA))

  # run the estimator; any try-catch lives inside the registered fun
  entry$fun(dat)
}

# Collapse data to one effect per study --------------------------------------
collapse_one <- function(dat,
                         rule = c("max", "min"),
                         abs_cols = c("yi")) {
  rule <- match.arg(rule)
  dplyr::group_by(dat, study) |>
    dplyr::slice({
      if (rule == "max") which.max(abs(.data[[abs_cols[1]]]))
      else               which.min(abs(.data[[abs_cols[1]]]))
    }) |>
    dplyr::ungroup()
}

run_select_dependency <- function(dat, ma_method, dependency) {
  if (dependency == "select_max")      dat <- collapse_one(dat, "max")
  else if (dependency == "select_min") dat <- collapse_one(dat, "min")
  else stop("Unknown dependency: ", dependency)

  entry <- .ma_method_registry[[ as.character(ma_method) ]]

  ## NEW test: does this estimator claim to support *this* dependency?
  if (is.null(entry) || !(dependency %in% entry$deps))
    return(list(b = NA, ci.lb = NA, ci.ub = NA, pval = NA))

  entry$fun(dat)
}
