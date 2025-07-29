.onLoad <- function(lib, pkg) {
  register_ma_method("fe", fit_fe, dependencies = c("select_max", "select_min", "aggregate"))
  register_ma_method("reml", fit_reml, dependencies = c("select_max", "select_min", "aggregate"))
  register_ma_method("uwls",  fit_uwls, dependencies = c("select_max", "select_min", "aggregate"))
  register_ma_method("waap",  fit_waap,  dependencies = c("select_max", "select_min", "aggregate"))
  register_ma_method("pet-peese", fit_pet.peese, dependencies = c("select_max", "select_min", "aggregate"))
  register_ma_method("pet-peese-corr", pet_peese_corr, dependencies = c("select_max", "select_min", "aggregate"))
  register_ma_method("pm", fit_pm, dependencies = c("select_max", "select_min", "aggregate"))
  register_ma_method("hk-sj", fit_hk_sj, dependencies = c("select_max", "select_min", "aggregate"))
  #register_ma_method("robma", fit_robma, dependencies = c("select_max", "select_min", "aggregate"))
  register_ma_method("bayesmeta", fit_bayesmeta, dependencies = c("select_max", "select_min", "aggregate")
                     )
  # no aggregate methods for selection based estimators
  register_ma_method("p-uniform", fit_puni_star, dependencies = c("select_max", "select_min"))

  # modeled-dependency helpers
  # modeled-dependency helpers
  register_ma_method("3-level", fit_three_level, dependencies = "modeled")
  register_ma_method("rve", fit_rve, dependencies = "modeled")

}
