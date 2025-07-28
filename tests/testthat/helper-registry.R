## fresh registry each test session
safe_out <- list(b = NA_real_, ci.lb = NA_real_, ci.ub = NA_real_, pval = NA_real_)

register_default_ma_methods <- function(fast_only = FALSE) {
  .ma_method_registry <- get(".ma_method_registry", envir = asNamespace("metaMultiverse"))

  # clear any previous definitions
  rm(list = ls(envir = .ma_method_registry), envir = .ma_method_registry)

  ## always-included fast estimators
  register_ma_method("fe",    fun = fit_fe,    dependencies = c("aggregate","select_min","select_max"))
  register_ma_method("reml",  fun = fit_reml,  dependencies = c("aggregate","select_min","select_max"))
  register_ma_method("uwls",  fun = fit_uwls, dependencies = c("aggregate"))
  register_ma_method("paule-mandel", fun = fit_pm, dependencies = c("aggregate"))

  if (!fast_only) {
    register_ma_method("bayesmeta", fun = fit_bayesmeta, dependencies = c("aggregate"))
    register_ma_method("paule-mandel", fun = fit_pm, dependencies = c("aggregate"))
    ## add any other slow estimators here
  }
}

## honour the runtime flag
register_default_ma_methods(
  fast_only = getOption("metaMultiverse.skip_slow", FALSE)
)
