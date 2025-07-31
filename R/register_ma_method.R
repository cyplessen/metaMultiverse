# R/ma_registry.R
.ma_method_registry <- new.env(parent = emptyenv())

#' Register a meta-analysis estimator
#' @param name Character scalar (key used in `ma_methods`)
#' @param fun  Function(dat) → list(b, ci.lb, ci.ub, pval)
#' @param dependencies Character vector: "select_min", "select_max", "aggregate", "modeled"
#' @keywords internal
register_ma_method <- function(name,
                               fun,
                               dependencies = c("select_min",
                                                "select_max",
                                                "aggregate",
                                                "modeled")) {
  stopifnot(is.character(name), length(name) == 1, is.function(fun))
  .ma_method_registry[[name]] <- list(fun = fun, deps = dependencies)
}

#' List available estimators
#' @export
list_ma_methods <- function() names(as.list(.ma_method_registry))
