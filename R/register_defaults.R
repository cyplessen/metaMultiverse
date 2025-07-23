.onLoad <- function(lib, pkg) {
  register_ma_method("fe",
                     fun = function(dat)
                       metafor::rma(yi = dat$yi, vi = dat$vi, method = "FE"),
                     dependencies = "ignore")

  register_ma_method("reml",
                     fun = function(dat)
                       metafor::rma(yi = dat$yi, vi = dat$vi,
                                    method = "REML",
                                    control = list(stepadj = 0.5,
                                                   maxiter = 2000)),
                     dependencies = c("ignore", "aggregate"))

  # aggregate-only helpers
  register_ma_method("uwls",  calculate_uwls,  "aggregate")
  register_ma_method("waap",  calculate_waap,  "aggregate")
  register_ma_method("pet-peese", calculate_pet.peese, "aggregate")
  register_ma_method("p-uniform", calculate_puni_star, "aggregate")

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
}
