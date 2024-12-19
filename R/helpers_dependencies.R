# helpers_dependencies.R
# This file contains functions to handle aggregate and modeled dependencies in multiverse meta-analysis.

## Aggregate Dependency -------------------------------------------------------

#' Run Aggregate Dependency
#'
#' Aggregates the data by clusters (studies) and applies the selected meta-analysis method.
#'
#' @param dat A data frame containing `yi` (effect size), `vi` (variance), and `study` columns.
#' @param ma_method The meta-analysis method to apply (e.g., "fe", "reml", "uwls").
#' @param how_methods A vector of allowed methods for the analysis.
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

  # Step 3: Apply the selected method
  if (ma_method == "fe" & "fe" %in% how_methods) {
    return(metafor::rma(yi = dat$yi, vi = dat$vi, method = "FE"))
  } else if (ma_method == "reml" & "reml" %in% how_methods) {
    return(metafor::rma(
      yi = dat$yi, vi = dat$vi, method = "REML",
      control = list(stepadj = 0.5, maxiter = 2000)
    ))
  } else if (ma_method == "uwls" & "uwls" %in% how_methods) {
    return(calculate_uwls(dat))
  } else if (ma_method == "waap" & "waap" %in% how_methods) {
    return(calculate_waap(dat))
  } else if (ma_method == "pet-peese" & "pet-peese" %in% how_methods) {
    return(calculate_pet.peese(dat))
  } else if (ma_method == "p-uniform" & "p-uniform" %in% how_methods) {
    return(calculate_puni_star(dat))
  } else {
    # Default response if method is unsupported
    return(list(b = NA, pval = NA, ci.lb = NA, ci.ub = NA))
  }
}

## Modeled Dependency ---------------------------------------------------------

#' Run Modeled Dependency
#'
#' Applies a multilevel (modeled) dependency approach using a random-effects model.
#'
#' @param dat A data frame containing `yi` (effect size), `vi` (variance), `es_id` (effect size ID), and `study` columns.
#' @param ma_method The meta-analysis method to apply (e.g., "3-level", "rve").
#' @param how_methods A vector of allowed methods for the analysis.
#' @return A list containing the meta-analysis results or a standardized NA list if the method fails.
run_modeled_dependency <- function(dat, ma_method, how_methods) {
  # Check if multiple effect sizes per study exist
  if (sum(duplicated(dat$study)) > 1) {
    # Try to fit the model
    mod_modeled <- tryCatch({
      metafor::rma.mv(
        data = dat, yi = yi, V = vi, method = "REML",
        random = list(~1 | es_id, ~1 | study), sparse = TRUE
      )
    }, error = function(e) list(error = TRUE))  # Add an error indicator

    # Check if the model fitting was successful
    if (!is.list(mod_modeled) || is.null(mod_modeled$error)) {
      if (ma_method == "3-level" & "3-level" %in% how_methods) {
        return(mod_modeled)
      } else if (ma_method == "rve" & "rve" %in% how_methods) {
        return(tryCatch({
          metafor::robust(mod_modeled,
                          cluster = dat$study,
                          clubSandwich = TRUE)
        }, error = function(e) list(b = NA, pval = NA, ci.lb = NA, ci.ub = NA)))
      }
    }
  }

  # Default return if no valid model or unsupported method
  list(b = NA, pval = NA, ci.lb = NA, ci.ub = NA)
}
