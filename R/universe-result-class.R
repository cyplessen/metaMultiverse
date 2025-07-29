#' Build a universe result (internal helper)
#'
#' @param b,ci.lb,ci.ub,pval Numeric scalars.
#' @return An object of class \code{c("universe_result","list")}.
#' @keywords internal
new_universe_result <- function(b, ci.lb, ci.ub, pval) {
  structure(
    list(
      b     = as.numeric(b),
      ci.lb = as.numeric(ci.lb),
      ci.ub = as.numeric(ci.ub),
      pval  = as.numeric(pval)
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
    "b = %.3f  [%.3f, %.3f]   p = %s\n",
    x$b, x$ci.lb, x$ci.ub,
    if (is.na(x$pval)) "NA" else formatC(x$pval, digits = 3, format = "g")
  ))
  invisible(x)
}

# reusable all-NA sentinel
universe_NA <- new_universe_result(NA_real_, NA_real_, NA_real_, NA_real_)

#' @method summary universe_result
#' @export
summary.universe_result <- function(object, ...) {
  tibble::tibble(
    b      = object$b,
    ci.lb  = object$ci.lb,
    ci.ub  = object$ci.ub,
    pval   = object$pval,
    method = attr(object, "method", exact = TRUE) %||% NA_character_
  )
}
