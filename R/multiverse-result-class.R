#' Build a multiverse result (internal helper)
#'
#' @param b,ci.lb,ci.ub,pval Numeric scalars.
#' @return An object of class \code{c("multiverse_result","list")}.
#' @keywords internal
new_multiverse_result <- function(b, ci.lb, ci.ub, pval) {
  structure(
    list(
      b     = as.numeric(b),
      ci.lb = as.numeric(ci.lb),
      ci.ub = as.numeric(ci.ub),
      pval  = as.numeric(pval)
    ),
    class = c("multiverse_result", "list")
  )
}

# Friendly console output
#' @method print multiverse_result
#' @export
print.multiverse_result <- function(x, ...) {
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
multiverse_NA <- new_multiverse_result(NA_real_, NA_real_, NA_real_, NA_real_)

#' @method summary multiverse_result
#' @export
summary.multiverse_result <- function(object, ...) {
  tibble::tibble(
    b      = object$b,
    ci.lb  = object$ci.lb,
    ci.ub  = object$ci.ub,
    pval   = object$pval,
    method = attr(object, "method", exact = TRUE) %||% NA_character_
  )
}
