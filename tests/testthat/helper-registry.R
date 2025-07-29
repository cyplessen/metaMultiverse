## fresh registry each test session
safe_out <- list(b = NA_real_, ci.lb = NA_real_, ci.ub = NA_real_, pval = NA_real_)

# Add this to your package's registry setup
register_default_ma_methods <- function(fast_only = FALSE) {
  .ma_method_registry <- get(".ma_method_registry", envir = asNamespace("metaMultiverse"))
  # clear any previous definitions
  rm(list = ls(envir = .ma_method_registry), envir = .ma_method_registry)

  ## Basic estimators (always included)
  register_ma_method("fe", fun = fit_fe, dependencies = c("aggregate","select_min","select_max"))
  register_ma_method("reml", fun = fit_reml, dependencies = c("aggregate","select_min","select_max"))
  register_ma_method("pm", fun = fit_pm, dependencies = c("aggregate","select_min","select_max"))
  register_ma_method("uwls", fun = fit_uwls, dependencies = c("aggregate","select_min","select_max"))
  register_ma_method("pet.peese", fun = fit_pet.peese, dependencies = c("aggregate","select_min","select_max"))
  register_ma_method("pet.peese.corrected", fun = pet_peese_corr, dependencies = c("aggregate","select_min","select_max"))
  register_ma_method("puni_star", fun = fit_puni_star, dependencies = c("select_min","select_max"))
  register_ma_method("waap", fun = fit_waap, dependencies = c("aggregate","select_min","select_max"))
  register_ma_method("hk_sj", fun = fit_hk_sj, dependencies = c("aggregate","select_min","select_max"))

  # modeled-dependency helpers
  register_ma_method("3-level",
                     fun = function(dat)
                       metafor::rma.mv(data = dat, yi = yi, V = vi,
                                       random = list(~1 | es_id, ~1 | study),
                                       method = "REML", sparse = TRUE),
                     dependencies = "modeled")

  register_ma_method("rve",
                     fun = function(dat) {
                       mod <- metafor::rma.mv(data = dat, yi = yi, V = vi,
                                              random = list(~1 | es_id, ~1 | study),
                                              method = "REML", sparse = TRUE)
                       metafor::robust(mod, cluster = dat$study,
                                       clubSandwich = TRUE)
                     },
                     dependencies = "modeled")
  if (!fast_only) {
    register_ma_method("bayesmeta", fun = fit_bayesmeta, dependencies = c("aggregate"))
  }
}
## honour the runtime flag
register_default_ma_methods(
  fast_only = getOption("metaMultiverse.skip_slow", FALSE)
)
