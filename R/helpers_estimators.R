# helpers_estimators.R

# This file contains functions to perform various estimation methods used in multiverse meta-analysis.
# Suppress warnings for dynamically evaluated variables
globalVariables(c("b", "ci.lb", "ci.ub", "pval", "ma_method", "vi", "yi"))

## PET-PEESE Estimation -------------------------------------------------------

#' Fit PET-PEESE Estimates
#'
#' Computes the PET-PEESE estimates based on the provided data.
#' PET is applied first, and if the PET p-value is below 0.1, PEESE is used.
#'
#' @param data A data frame containing `yi` (effect size) and `vi` (variance).
#' @return A list containing `b`, `ci.lb`, `ci.ub`, `pval`, and `type` (either "PET" or "PEESE").
#' @importFrom stats lm coef confint
#' @importFrom dplyr %>%
fit_pet.peese <- function(data) {
  mod <- list()

  # Try PET estimation
  pet_fit <- tryCatch({
    stats::lm(yi ~ sqrt(vi), weights = 1 / vi, data = data)
  }, error = function(e) return(NULL))

  if (is.null(pet_fit)) {
    return(list(b = NA, ci.lb = NA, ci.ub = NA, pval = NA, type = "PET (failed)"))
  }

  # Extract PET p-value
  pet_p <- stats::coef(summary(pet_fit))["(Intercept)", "Pr(>|t|)"]

  if (is.na(pet_p) || pet_p >= 0.1) {
    # PET is valid or PEESE is not triggered
    mod <- list(
      b = stats::coef(summary(pet_fit))["(Intercept)", "Estimate"],
      ci.lb = stats::confint(pet_fit)["(Intercept)", "2.5 %"],
      ci.ub = stats::confint(pet_fit)["(Intercept)", "97.5 %"],
      pval = pet_p,
      type = "PET"
    )
  } else {
    # PEESE estimation
    peese_fit <- tryCatch({
      stats::lm(yi ~ vi, weights = 1 / vi, data = data)
    }, error = function(e) return(NULL))

    if (is.null(peese_fit)) {
      return(list(b = NA, ci.lb = NA, ci.ub = NA, pval = NA, type = "PEESE (failed)"))
    }

    mod <- list(
      b = stats::coef(summary(peese_fit))["(Intercept)", "Estimate"],
      ci.lb = stats::confint(peese_fit)["(Intercept)", "2.5 %"],
      ci.ub = stats::confint(peese_fit)["(Intercept)", "97.5 %"],
      pval = stats::coef(summary(peese_fit))["(Intercept)", "Pr(>|t|)"],
      type = "PEESE"
    )
  }

  return(mod)
}

#' PET-PEESE (corrected)
#'
#' Runs PET-PEESE, then sets b = 0 whenever the estimate is negative
#' (common sensitivity correction).
#' @keywords internal
pet_peese_corr <- function(dat) {
  out <- fit_pet.peese(dat)
  if (!is.na(out$b) && out$b < 0) out$b <- 0
  out
}

## P-Uniform Estimation ------------------------------------------------------

#' Fit P-Uniform Star
#'
#' Computes the P-Uniform Star estimates for a dataset.
#'
#' @param dat A data frame containing `yi` (effect size) and `vi` (variance).
#' @return A list containing `b`, `ci.lb`, `ci.ub`, and `pval`.
#' @importFrom puniform puni_star
fit_puni_star <- function(dat) {
  mod <- tryCatch({
    mod.puni <- puniform::puni_star(
      yi = dat$yi,
      vi = dat$vi,
      side = "right"
    )

    list(
      b = mod.puni$est,
      pval = mod.puni$pval.0,
      ci.lb = mod.puni$ci.lb,
      ci.ub = mod.puni$ci.ub
    )
  }, error = function(e) {
    list(
      b = NA,
      pval = NA,
      ci.lb = NA,
      ci.ub = NA
    )
  })

  return(mod)
}

## UWLS and WAAP Estimation --------------------------------------------------

#' Fit UWLS Estimates
#'
#' Performs Unweighted Least Squares (UWLS) estimation.
#'
#' @param dat A data frame containing `yi` (effect size) and `vi` (variance).
#' @return A list containing `b`, `ci.lb`, `ci.ub`, and `pval`.
#' @importFrom stats lm coef confint
fit_uwls <- function(dat) {
  d <- dat$yi
  sed <- sqrt(dat$vi)
  Precision <- 1 / sed
  reg_uwls <- stats::lm(d / sed ~ 0 + Precision)

  list(
    b = stats::coef(summary(reg_uwls))["Precision", "Estimate"],
    ci.lb = stats::confint(reg_uwls)["Precision", "2.5 %"],
    ci.ub = stats::confint(reg_uwls)["Precision", "97.5 %"],
    pval = stats::coef(summary(reg_uwls))["Precision", "Pr(>|t|)"]
  )
}

#' Fit WAAP Estimates
#'
#' Performs Weighted Average of Adequately Powered (WAAP) estimation.
#'
#' @param dat A data frame containing `yi` (effect size) and `vi` (variance).
#' @return A list containing `b`, `ci.lb`, `ci.ub`, and `pval`.
#' @importFrom stats lm coef confint
fit_waap <- function(dat) {
  d <- dat$yi
  sed <- sqrt(dat$vi)
  Precision <- 1 / sed
  reg_uwls <- stats::lm(d / sed ~ 0 + Precision)
  UWLS <- stats::coef(reg_uwls)["Precision"]
  powered <- sed < abs(UWLS) / 2.8

  if (sum(powered) < 2) {
    return(list(b = NA, ci.lb = NA, ci.ub = NA, pval = NA))
  }

  reg_waap <- stats::lm(d[powered] / sed[powered] ~ 0 + Precision[powered])
  list(
    b = stats::coef(summary(reg_waap))["Precision[powered]", "Estimate"],
    ci.lb = stats::confint(reg_waap)["Precision[powered]", "2.5 %"],
    ci.ub = stats::confint(reg_waap)["Precision[powered]", "97.5 %"],
    pval = stats::coef(summary(reg_waap))["Precision[powered]", "Pr(>|t|)"]
  )
}

# Paule–Mandel estimator (frequentist REML alternative)
fit_pm <- function(dat) {
  mod <- tryCatch(
    metafor::rma(yi = dat$yi,
                 vi = dat$vi,
                 method = "PM",
                 test   = "z"),
    error = function(e) NULL
  )

  if (is.null(mod))
    return(list(b = NA, ci.lb = NA, ci.ub = NA, pval = NA))

  list(b     = unname(mod$b),
       ci.lb = unname(mod$ci.lb),
       ci.ub = unname(mod$ci.ub),
       pval  = unname(mod$pval))
}

fit_hk_sj <- function(dat) {
  mod <- tryCatch(
    meta::metagen(TE      = dat$yi,
                  seTE    = sqrt(dat$vi),
                  method.random.ci = "HK", # Hartung–Knapp
                  method.tau = "SJ"),      # Sidik–Jonkman tau²
    error = function(e) NULL
  )

  if (is.null(mod))
    return(list(b = NA, ci.lb = NA, ci.ub = NA, pval = NA))

  list(b     = mod$TE.random,
       ci.lb = mod$lower.random,
       ci.ub = mod$upper.random,
       pval  = mod$pval.random)
}

#' Fit Bayesian model-averaged meta-analysis (RoBMA ≥ 3.0)
#' Returns list(b, ci.lb, ci.ub, pval), or all NA if the fit fails.
#' @keywords internal
# fit_robma <- function(dat) {
#
#   # RoBMA needs SEs, not variances
#   sei_vec <- sqrt(dat$vi)
#
#   fit <- tryCatch(
#     RoBMA::RoBMA(
#       y          = dat$yi,
#       se         = sei_vec,
#       transformation = "none",
#       prior_scale    = "none",
#       parallel   = FALSE,      # safer for CRAN / Windows
#       silent     = TRUE
#     ),
#     error = function(e) {
#       message("[RoBMA] Fit failed: ", e$message)
#       return(NULL)
#     }
#   )
#
#   # If the fit object is NULL or MCMC didn’t converge,
#   # return the standard NA row so the multiverse keeps running.
#   if (is.null(fit) || isFALSE(fit$convergence))
#     return(list(b = NA, ci.lb = NA, ci.ub = NA, pval = NA))
#
#   ens <- summary(fit)$ensemble   # data.frame with estimate + CI
#
#   list(
#     b     = ens$estimate,
#     ci.lb = ens$CI_low,
#     ci.ub = ens$CI_high,
#     pval  = ens$p_value
#   )
# }

fit_bayesmeta <- function(dat) {

  tryCatch({

    bm <- bayesmeta::bayesmeta(y = dat$yi,
                                 sigma = sqrt(dat$vi))

    post <- bm$summary                   # posterior summary data.frame
    list(
      b     = post["median",     "mu"],
      ci.lb = post["95% lower",  "mu"],
      ci.ub = post["95% upper",  "mu"],
      pval  = NA_real_                    # not defined for Bayesian fit
    )

  }, error = function(e) {
    message("[bayesmeta] ", e$message)
    safe_out
  })
}
