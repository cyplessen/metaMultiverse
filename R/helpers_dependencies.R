# helpers_dependencies.R ----------------------------------------------------
# Standard NA row every runner must return on failure
safe_out <- list(b = NA_real_, ci.lb = NA_real_, ci.ub = NA_real_, pval = NA_real_)

# ---------------------------------------------------------------------------
# Aggregate dependency
# ---------------------------------------------------------------------------
run_aggregate_dependency <- function(dat, ma_method) {

  # escalc + cluster aggregation
  dat <- metafor::escalc(yi = yi, vi = vi, data = dat)
  dat <- metafor::aggregate.escalc(dat, cluster = study,
                                   struct = "CS", rho = 0.5)

  entry <- .ma_method_registry[[as.character(ma_method)]]
  if (is.null(entry) || !("aggregate" %in% entry$deps))
    return(safe_out)

  tryCatch(entry$fun(dat),
           error = function(e) {
             message("[aggregate:", ma_method, "] ", e$message)
             safe_out
           })
}

# ---------------------------------------------------------------------------
# Modeled dependency  (multi-level / RVE)
# ---------------------------------------------------------------------------
run_modeled_dependency <- function(dat, ma_method) {

  if (sum(duplicated(dat$study)) < 1)                # nothing to model
    return(safe_out)

  entry <- .ma_method_registry[[as.character(ma_method)]]
  if (is.null(entry) || !("modeled" %in% entry$deps))
    return(safe_out)

  tryCatch(entry$fun(dat),
           error = function(e) {
             message("[modeled:", ma_method, "] ", e$message)
             safe_out
           })
}

# ---------------------------------------------------------------------------
# Select-one-effect per study  (select_max / select_min)
# ---------------------------------------------------------------------------
collapse_one <- function(dat, rule = c("max", "min"), abs_cols = "yi") {
  rule <- match.arg(rule)
  dplyr::group_by(dat, study) |>
    dplyr::slice({
      if (rule == "max") which.max(abs(.data[[abs_cols[1]]]))
      else               which.min(abs(.data[[abs_cols[1]]]))
    }) |>
    dplyr::ungroup()
}

run_select_dependency <- function(dat, ma_method, dependency) {

  dat <- switch(
    dependency,
    "select_max" = collapse_one(dat, "max"),
    "select_min" = collapse_one(dat, "min"),
    stop("Unknown dependency: ", dependency)
  )

  entry <- .ma_method_registry[[as.character(ma_method)]]
  if (is.null(entry) || !(dependency %in% entry$deps))
    return(safe_out)

  tryCatch(entry$fun(dat),
           error = function(e) {
             message("[", dependency, ":", ma_method, "] ", e$message)
             safe_out
           })
}
