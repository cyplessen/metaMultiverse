skip_if(getOption("metaMultiverse.skip_slow", FALSE))

est_funs <- list(
  fit_fe          = metaMultiverse:::fit_fe,
  fit_reml        = metaMultiverse:::fit_reml,
  fit_pet_peese   = metaMultiverse:::fit_pet.peese,
  fit_puni_star   = metaMultiverse:::fit_puni_star,
  fit_uwls        = metaMultiverse:::fit_uwls,
  fit_waap        = metaMultiverse:::fit_waap,
  fit_pm          = metaMultiverse:::fit_pm,
  fit_hk_sj       = metaMultiverse:::fit_hk_sj
)

core <- c("b","ci.lb","ci.ub","pval")

test_that("all estimator helpers return at least the 4 core fields", {

  for (fn_name in names(est_funs)) {

    res <- est_funs[[fn_name]](digDep_tiny)

    expect_type(res, "list")
    expect_true(all(core %in% names(res)))

    expect_true(is.numeric(res$b)     && length(res$b)     == 1,
                label = paste0(fn_name, "$b"))
    expect_true(is.numeric(res$ci.lb) && length(res$ci.lb) == 1,
                label = paste0(fn_name, "$ci.lb"))
    expect_true(is.numeric(res$ci.ub) && length(res$ci.ub) == 1,
                label = paste0(fn_name, "$ci.ub"))
    expect_true(is.numeric(res$pval)  && length(res$pval)  == 1,
                label = paste0(fn_name, "$pval"))
  }
})
