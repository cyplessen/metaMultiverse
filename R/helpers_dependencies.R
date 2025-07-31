#' Dependency Handling for Multiverse Meta-Analysis
#'
#' These functions handle different strategies for dealing with statistical
#' dependencies between effect sizes from the same study.
#'
#' @name dependency_handlers
#' @keywords internal

# =============================================================================
# AGGREGATE DEPENDENCY HANDLING
# =============================================================================

#' Handle Dependencies by Aggregating Effect Sizes
#'
#' Combines multiple effect sizes from the same study into a single composite
#' effect size using a compound symmetry correlation structure.
#'
#' @param dat Data frame containing effect sizes with \code{yi}, \code{vi}, and \code{study} columns
#' @param ma_method Character string specifying the meta-analytic method to apply
#' @param rho Numeric. Assumed correlation between effect sizes within studies (default: 0.5)
#'
#' @return A \code{universe_result} object with the meta-analysis results, or \code{universe_NA} if failed
#'
#' @details
#' This approach assumes that multiple effect sizes from the same study are
#' correlated with a compound symmetry structure (constant correlation \code{rho}).
#' The method:
#' \enumerate{
#'   \item Uses \code{metafor::escalc()} to ensure proper effect size formatting
#'   \item Applies \code{metafor::aggregate.escalc()} with compound symmetry structure
#'   \item Executes the specified meta-analytic method on the aggregated data
#' }
#'
#' \strong{When to use:} When studies contribute multiple related effect sizes
#' and you want to preserve information from all effects while avoiding
#' dependencies. Common in educational and psychological meta-analyses.
#'
#' \strong{Assumptions:}
#' \itemize{
#'   \item Effect sizes within studies are correlated at level \code{rho}
#'   \item The correlation structure is compound symmetry (constant correlation)
#'   \item Studies contributing multiple effects are substantively similar to
#'         studies contributing single effects
#' }
#'
#' @keywords internal
run_aggregate_dependency <- function(dat, ma_method, rho = 0.5) {
  # Step 1: Ensure proper escalc format
  dat <- metafor::escalc(yi = yi, vi = vi, data = dat)

  # Step 2: Aggregate using compound symmetry structure
  dat <- metafor::aggregate.escalc(
    dat,
    cluster = study,
    struct = "CS",  # Compound symmetry
    rho = rho       # Assumed within-study correlation
  )

  # Step 3: Validate method compatibility
  entry <- .ma_method_registry[[as.character(ma_method)]]
  if (is.null(entry) || !("aggregate" %in% entry$deps)) {
    return(universe_NA)
  }

  # Step 4: Execute meta-analysis safely
  res <- tryCatch(
    entry$fun(dat),
    error = function(e) {
      message("[aggregate:", ma_method, "] ", e$message)
      universe_NA
    }
  )

  # Step 5: Ensure method attribution
  if (is.null(attr(res, "method", exact = TRUE))) {
    attr(res, "method") <- paste0(ma_method, " (aggregated)")
  }

  return(res)
}

# =============================================================================
# MODELED DEPENDENCY HANDLING
# =============================================================================

#' Handle Dependencies by Modeling Hierarchical Structure
#'
#' Explicitly models the hierarchical structure of effect sizes nested within
#' studies using multilevel meta-analysis or robust variance estimation.
#'
#' @param dat Data frame with \code{yi}, \code{vi}, \code{es_id}, and \code{study} columns
#' @param ma_method Character string specifying the multilevel method ("three-level" or "rve")
#'
#' @return A \code{universe_result} object with results, or \code{universe_NA} if inappropriate/failed
#'
#' @details
#' This approach directly models the dependency structure without aggregation:
#' \enumerate{
#'   \item Checks if multiple studies contribute multiple effect sizes
#'   \item Validates that the method supports dependency modeling
#'   \item Applies multilevel modeling (3-level) or robust variance estimation (RVE)
#' }
#'
#' \strong{Available methods:}
#' \itemize{
#'   \item \code{"three-level"}: Three-level meta-analysis with random effects for
#'         studies and effect sizes within studies
#'   \item \code{"rve"}: Robust variance estimation with cluster-robust standard errors
#' }
#'
#' \strong{When to use:} When you want to model the dependency structure explicitly
#' and have sufficient studies with multiple effect sizes. Particularly useful
#' when the dependency structure is complex or heterogeneous across studies.
#'
#' \strong{Requirements:}
#' \itemize{
#'   \item At least some studies must contribute multiple effect sizes
#'   \item Sufficient number of studies for stable estimation (typically ≥10)
#'   \item Proper study and effect size identifiers (\code{study}, \code{es_id})
#' }
#'
#' @keywords internal
run_modeled_dependency <- function(dat, ma_method) {
  # Step 1: Check if dependency modeling is needed/possible
  if (sum(duplicated(dat$study)) < 1) {
    # No studies contribute multiple effect sizes
    return(universe_NA)
  }

  # Step 2: Validate method compatibility
  entry <- .ma_method_registry[[as.character(ma_method)]]
  if (is.null(entry) || !("modeled" %in% entry$deps)) {
    return(universe_NA)
  }

  # Step 3: Execute multilevel analysis
  res <- tryCatch(
    entry$fun(dat),
    error = function(e) {
      message("[modeled:", ma_method, "] ", e$message)
      universe_NA
    }
  )

  # Step 4: Ensure method attribution
  if (is.null(attr(res, "method", exact = TRUE))) {
    attr(res, "method") <- paste0(ma_method, " (multilevel)")
  }

  return(res)
}

# =============================================================================
# SELECTION-BASED DEPENDENCY HANDLING
# =============================================================================

#' Select One Effect Size Per Study
#'
#' Reduces dependencies by selecting a single effect size from each study
#' based on specified criteria (largest or smallest absolute magnitude).
#'
#' @param dat Data frame containing effect sizes
#' @param rule Character string: "max" (largest absolute effect) or "min" (smallest absolute effect)
#' @param abs_cols Character vector of column names to use for selection criteria (default: "yi")
#'
#' @return Data frame with one row per study (the selected effect size)
#'
#' @details
#' This function implements a selection strategy where only one effect size
#' per study is retained, eliminating dependencies entirely.
#'
#' \strong{Selection rules:}
#' \itemize{
#'   \item \code{"max"}: Selects the effect size with the largest absolute value
#'   \item \code{"min"}: Selects the effect size with the smallest absolute value
#' }
#'
#' \strong{When to use:}
#' \itemize{
#'   \item \code{"select_max"}: When you want to focus on the strongest effects,
#'         assuming larger effects are more reliable or theoretically important
#'   \item \code{"select_min"}: When you want conservative estimates, or when
#'         smaller effects might be less biased
#' }
#'
#' @keywords internal
collapse_one <- function(dat, rule = c("max", "min"), abs_cols = "yi") {
  rule <- match.arg(rule)

  dat |>
    dplyr::group_by(study) |>
    dplyr::slice({
      if (rule == "max") {
        which.max(abs(.data[[abs_cols[1]]]))
      } else {
        which.min(abs(.data[[abs_cols[1]]]))
      }
    }) |>
    dplyr::ungroup()
}

#' Handle Dependencies by Selecting One Effect Per Study
#'
#' Applies selection-based dependency handling followed by meta-analysis.
#'
#' @param dat Data frame containing effect sizes
#' @param ma_method Character string specifying the meta-analytic method
#' @param dependency Character string: "select_max" or "select_min"
#'
#' @return A \code{universe_result} object with results, or \code{universe_NA} if failed
#'
#' @details
#' This approach eliminates dependencies by selecting only one effect size
#' per study, then applies standard meta-analysis methods to the reduced dataset.
#'
#' \strong{Strategy comparison:}
#' \itemize{
#'   \item \bold{select_max}: May introduce optimistic bias but captures
#'         strongest evidence. Useful when effect heterogeneity within studies
#'         reflects meaningful variation you want to capture.
#'   \item \bold{select_min}: More conservative, may underestimate true effects
#'         but could be less susceptible to selection bias. Useful for
#'         robustness checks.
#' }
#'
#' \strong{Advantages:} Simple, eliminates dependencies completely, compatible
#' with all meta-analytic methods.
#'
#' \strong{Disadvantages:} Discards information, selection may introduce bias,
#' studies contributing multiple effects may not be comparable to those
#' contributing single effects.
#'
#' @keywords internal
run_select_dependency <- function(dat, ma_method, dependency) {
  # Step 1: Apply selection rule
  dat <- switch(
    dependency,
    "select_max" = collapse_one(dat, "max"),
    "select_min" = collapse_one(dat, "min"),
    stop("Unknown selection dependency: ", dependency)
  )

  # Step 2: Validate method compatibility
  entry <- .ma_method_registry[[as.character(ma_method)]]
  if (is.null(entry) || !(dependency %in% entry$deps)) {
    return(universe_NA)
  }

  # Step 3: Execute meta-analysis
  res <- tryCatch(
    entry$fun(dat),
    error = function(e) {
      message("[", dependency, ":", ma_method, "] ", e$message)
      universe_NA
    }
  )

  # Step 4: Ensure method attribution
  if (is.null(attr(res, "method", exact = TRUE))) {
    selection_type <- ifelse(dependency == "select_max", "max-selected", "min-selected")
    attr(res, "method") <- paste0(ma_method, " (", selection_type, ")")
  }

  return(res)
}

# =============================================================================
# DEPENDENCY STRATEGY COMPARISON
# =============================================================================

#' @section Dependency Strategy Guide:
#'
#' \strong{When to use each approach:}
#'
#' \describe{
#'   \item{\bold{Aggregate}}{
#'     \itemize{
#'       \item Multiple related effects per study (e.g., different outcomes)
#'       \item Want to preserve all information while handling dependencies
#'       \item Have reasonable estimate of within-study correlations
#'       \item Most methods support aggregated data
#'     }
#'   }
#'   \item{\bold{Modeled}}{
#'     \itemize{
#'       \item Complex dependency structures
#'       \item Sufficient studies with multiple effects (≥10 studies)
#'       \item Want to model heterogeneity at multiple levels
#'       \item Interested in variance components
#'     }
#'   }
#'   \item{\bold{Select Max}}{
#'     \itemize{
#'       \item Want to focus on strongest effects
#'       \item Exploratory analyses or when seeking upper bounds
#'       \item Robustness checks for optimistic scenarios
#'     }
#'   }
#'   \item{\bold{Select Min}}{
#'     \itemize{
#'       \item Want conservative estimates
#'       \item Concerned about selection bias toward larger effects
#'       \item Robustness checks for pessimistic scenarios
#'     }
#'   }
#' }
#'
#' \strong{Computational considerations:}
#' \itemize{
#'   \item \bold{Speed}: select > aggregate > modeled
#'   \item \bold{Stability}: aggregate > select > modeled
#'   \item \bold{Information retention}: modeled ≈ aggregate > select
#' }
NULL
