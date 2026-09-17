# ABOUTME: universe_result -- the per-specification result object. Carries the
# ABOUTME: estimate plus SE, tau2, I2, k and diagnostics needed for the
# ABOUTME: worst-universe audit (never just 4 scalars).

#' Build a universe result (internal helper)
#'
#' @param b,ci.lb,ci.ub,pval Numeric scalars.
#' @param se Standard error of the pooled estimate (NA if the method does not
#'   provide one).
#' @param tau2 Between-study variance estimate. Named tau2 -- never tau -- so
#'   the historical tau/tau-squared mislabeling cannot recur.
#' @param i2 I-squared in percent (0-100).
#' @param k Number of units (studies/effects) contributing to the fit.
#' @param convergence Logical; FALSE when the fit failed or hit limits.
#' @param notes Optional character notes from the fitting function.
#' @return An object of class \code{c("universe_result","list")}.
#' @keywords internal
new_universe_result <- function(b, ci.lb, ci.ub, pval,
                                se = NA_real_, tau2 = NA_real_,
                                i2 = NA_real_, k = NA_integer_,
                                convergence = TRUE, notes = NULL) {
  structure(
    list(
      b     = as.numeric(b),
      ci.lb = as.numeric(ci.lb),
      ci.ub = as.numeric(ci.ub),
      pval  = as.numeric(pval),
      se    = as.numeric(se),
      tau2  = as.numeric(tau2),
      i2    = as.numeric(i2),
      k     = as.integer(k),
      convergence = isTRUE(convergence),
      notes = notes
    ),
    class = c("universe_result", "list")
  )
}

# Friendly console output
#' @method print universe_result
#' @export
print.universe_result <- function(x, ...) {
  lab <- attr(x, "method", exact = TRUE)
  if (!is.null(lab)) cat("[", lab, "] ", sep = "")
  cat(sprintf(
    "b = %.3f  [%.3f, %.3f]   p = %s",
    x$b, x$ci.lb, x$ci.ub,
    if (is.na(x$pval)) "NA" else formatC(x$pval, digits = 3, format = "g")
  ))
  if (!is.na(x$tau2)) cat(sprintf("   tau2 = %.3f", x$tau2))
  if (!is.na(x$i2)) cat(sprintf("   I2 = %.1f%%", x$i2))
  if (!x$convergence) cat("   [NOT CONVERGED]")
  cat("\n")
  invisible(x)
}

# reusable all-NA sentinel
universe_NA <- new_universe_result(NA_real_, NA_real_, NA_real_, NA_real_,
                                   convergence = FALSE)

#' @method summary universe_result
#' @export
summary.universe_result <- function(object, ...) {
  tibble::tibble(
    b      = object$b,
    ci.lb  = object$ci.lb,
    ci.ub  = object$ci.ub,
    pval   = object$pval,
    se     = object$se,
    tau2   = object$tau2,
    i2     = object$i2,
    k      = object$k,
    convergence = object$convergence,
    method = attr(object, "method", exact = TRUE) %||% NA_character_
  )
}
