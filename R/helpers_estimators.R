# helpers_estimators.R

# This file contains functions to perform various estimation methods used in multiverse meta-analysis.
# Suppress warnings for dynamically evaluated variables
globalVariables(c("b", "ci.lb", "ci.ub", "pval", "ma_method", "vi", "yi"))


## Safe wrapper
safe_call <- function(expr) {
  tryCatch(expr, error = function(e) {
    message("[estimator] ", e$message)
    multiverse_NA
  })
}

## Standard fixed effect estimates
fit_fe  <- function(data) safe_call({
  mod <- metafor::rma(yi = data$yi, vi = data$vi, method = "FE")
  out <- new_universe_result(mod$b, mod$ci.lb, mod$ci.ub, mod$pval)
  attr(out, "method") <- "FE"
  out
})

## Standard random effects estimates (REML)
fit_reml <- function(data) safe_call({
  mod <- metafor::rma(yi = data$yi, vi = data$vi, method = "REML",
                      control = list(stepadj = 0.5, maxiter = 2000))
  out <- new_universe_result(mod$b, mod$ci.lb, mod$ci.ub, mod$pval)
  attr(out, "method") <- "REML"
  out
})

## helpers_estimators.R  (replace old version)

#------------------- PET-PEESE ----------------------------------------------

fit_pet.peese <- function(dat) safe_call({

  # ---------- PET ----------
  pet_fit <- tryCatch(
    stats::lm(yi ~ sqrt(vi), weights = 1 / vi, data = dat),
    error = function(e) NULL
  )

  if (is.null(pet_fit)) {
    out <- multiverse_NA
    attr(out, "method") <- "PET (failed)"
    return(out)
  }

  pet_p <- stats::coef(summary(pet_fit))["(Intercept)", "Pr(>|t|)"]

  # PEESE decision rule
  if (is.na(pet_p) || pet_p >= 0.10) {
    ## keep PET
    out <- new_universe_result(
      b     = stats::coef(pet_fit)["(Intercept)"],
      ci.lb = stats::confint(pet_fit)["(Intercept)", "2.5 %"],
      ci.ub = stats::confint(pet_fit)["(Intercept)", "97.5 %"],
      pval  = pet_p
    )
    attr(out, "method") <- "PET"
    return(out)
  }

  # ---------- PEESE ----------
  peese_fit <- tryCatch(
    stats::lm(yi ~ vi, weights = 1 / vi, data = dat),
    error = function(e) NULL
  )

  if (is.null(peese_fit)) {
    out <- multiverse_NA
    attr(out, "method") <- "PEESE (failed)"
    return(out)
  }

  out <- new_universe_result(
    b     = stats::coef(peese_fit)["(Intercept)"],
    ci.lb = stats::confint(peese_fit)["(Intercept)", "2.5 %"],
    ci.ub = stats::confint(peese_fit)["(Intercept)", "97.5 %"],
    pval  = stats::coef(summary(peese_fit))["(Intercept)", "Pr(>|t|)"]
  )
  attr(out, "method") <- "PEESE"
  out
})


# ----------------- PET-PEESE (corrected) ------------------------------------

pet_peese_corr <- function(dat) {
  out <- fit_pet.peese(dat)
  if (!is.na(out$b) && out$b < 0) out$b <- 0
  attr(out, "method") <- paste(attr(out, "method"), "(corrected)")
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
# ----------------- p-uniform* ----------------------------------------------

fit_puni_star <- function(dat) safe_call({

  pu <- tryCatch(
    puniform::puni_star(yi = dat$yi,
                        vi = dat$vi,
                        side = "right"),
    error = function(e) NULL
  )

  if (is.null(pu)) {
    out <- multiverse_NA
    attr(out, "method") <- "p-uniform* (failed)"
    return(out)
  }

  out <- new_universe_result(
    b     = pu$est,
    ci.lb = pu$ci.lb,
    ci.ub = pu$ci.ub,
    pval  = pu$pval.0
  )
  attr(out, "method") <- "p-uniform*"
  out
})

## UWLS and WAAP Estimation --------------------------------------------------

#' Fit UWLS Estimates
#'
#' Performs Unweighted Least Squares (UWLS) estimation.
#'
#' @param dat A data frame containing `yi` (effect size) and `vi` (variance).
#' @return A list containing `b`, `ci.lb`, `ci.ub`, and `pval`.
#' @importFrom stats lm coef confint
# ----------------- UWLS -----------------------------------------------------

fit_uwls <- function(dat) safe_call({

  d        <- dat$yi
  sed      <- sqrt(dat$vi)
  Precision <- 1 / sed

  reg <- stats::lm(d / sed ~ 0 + Precision)

  out <- new_universe_result(
    b     = stats::coef(reg)["Precision"],
    ci.lb = stats::confint(reg)["Precision", "2.5 %"],
    ci.ub = stats::confint(reg)["Precision", "97.5 %"],
    pval  = stats::coef(summary(reg))["Precision", "Pr(>|t|)"]
  )
  attr(out, "method") <- "UWLS"
  out
})

# ----------------- WAAP -----------------------------------------------------

fit_waap <- function(dat) safe_call({

  d        <- dat$yi
  sed      <- sqrt(dat$vi)
  Precision <- 1 / sed

  # first UWLS step
  reg_uwls <- stats::lm(d / sed ~ 0 + Precision)
  UWLS     <- stats::coef(reg_uwls)["Precision"]

  powered <- sed < abs(UWLS) / 2.8
  if (sum(powered) < 2) {
    out <- multiverse_NA
    attr(out, "method") <- "WAAP (failed - <2 powered)"
    return(out)
  }

  # WAAP regression
  reg <- stats::lm(d[powered] / sed[powered] ~ 0 + Precision[powered])

  out <- new_universe_result(
    b     = stats::coef(reg)["Precision[powered]"],
    ci.lb = stats::confint(reg)["Precision[powered]", "2.5 %"],
    ci.ub = stats::confint(reg)["Precision[powered]", "97.5 %"],
    pval  = stats::coef(summary(reg))["Precision[powered]", "Pr(>|t|)"]
  )
  attr(out, "method") <- "WAAP"
  out
})

# ---------- Paule-Mandel -----------------------------------------------

fit_pm <- function(dat) safe_call({

  mod <- tryCatch(
    metafor::rma(yi = dat$yi,
                 vi = dat$vi,
                 method = "PM",
                 test   = "z"),
    error = function(e) NULL
  )

  if (is.null(mod)) {
    out <- multiverse_NA
    attr(out, "method") <- "PM (failed)"
    return(out)
  }

  out <- new_universe_result(
    b     = unname(mod$b),
    ci.lb = unname(mod$ci.lb),
    ci.ub = unname(mod$ci.ub),
    pval  = unname(mod$pval)
  )
  attr(out, "method") <- "PM"
  out
})


# ---------- Hartung-Knapp / Sidik-Jonkman --------------------------------

fit_hk_sj <- function(dat) safe_call({

  mod <- tryCatch(
    meta::metagen(TE      = dat$yi,
                  seTE    = sqrt(dat$vi),
                  method.random.ci = "HK",
                  method.tau       = "SJ"),
    error = function(e) NULL
  )

  if (is.null(mod)) {
    out <- multiverse_NA
    attr(out, "method") <- "HK/SJ (failed)"
    return(out)
  }

  out <- new_universe_result(
    b     = mod$TE.random,
    ci.lb = mod$lower.random,
    ci.ub = mod$upper.random,
    pval  = mod$pval.random
  )
  attr(out, "method") <- "HK/SJ"
  out
})


# ---------- bayesmeta ----------------------------------------------------

fit_bayesmeta <- function(dat) safe_call({

  bm <- tryCatch(
    bayesmeta::bayesmeta(y = dat$yi,
                         sigma = sqrt(dat$vi)),
    error = function(e) NULL
  )

  if (is.null(bm)) {
    out <- multiverse_NA
    attr(out, "method") <- "bayesmeta (failed)"
    return(out)
  }

  s <- bm$summary   # posterior summary data.frame

  out <- new_universe_result(
    b     = s["median",    "mu"],
    ci.lb = s["95% lower", "mu"],
    ci.ub = s["95% upper", "mu"],
    pval  = NA_real_              # Bayesian fit: no p-value
  )
  attr(out, "method") <- "bayesmeta"
  out
})
