# helpers_estimators.R
# This file contains functions to perform various estimation methods used in multiverse meta-analysis.

## PET-PEESE Estimation -------------------------------------------------------

#' Calculate PET-PEESE Estimates
#'
#' Computes the PET-PEESE estimates based on the provided data.
#' PET is applied first, and if the PET p-value is below 0.1, PEESE is used.
#'
#' @param data A data frame containing `yi` (effect size) and `vi` (variance).
#' @return A list containing `b`, `ci.lb`, `ci.ub`, `pval`, and `type` (either "PET" or "PEESE").
calculate_pet.peese <- function(data) {
  mod <- list()

  # Try PET estimation
  pet_fit <- tryCatch({
    lm(yi ~ sqrt(vi), weights = 1 / vi, data = data)
  }, error = function(e) return(NULL))

  if (is.null(pet_fit)) {
    return(list(b = NA, ci.lb = NA, ci.ub = NA, pval = NA, type = "PET (failed)"))
  }

  # Extract PET p-value
  pet_p <- coef(summary(pet_fit))["(Intercept)", "Pr(>|t|)"]

  if (is.na(pet_p) || pet_p >= 0.1) {
    # PET is valid or PEESE is not triggered
    mod <- list(
      b = coef(summary(pet_fit))["(Intercept)", "Estimate"],
      ci.lb = confint(pet_fit)["(Intercept)", "2.5 %"],
      ci.ub = confint(pet_fit)["(Intercept)", "97.5 %"],
      pval = pet_p,
      type = "PET"
    )
  } else {
    # PEESE estimation
    peese_fit <- tryCatch({
      lm(yi ~ vi, weights = 1 / vi, data = data)
    }, error = function(e) return(NULL))

    if (is.null(peese_fit)) {
      return(list(b = NA, ci.lb = NA, ci.ub = NA, pval = NA, type = "PEESE (failed)"))
    }

    mod <- list(
      b = coef(summary(peese_fit))["(Intercept)", "Estimate"],
      ci.lb = confint(peese_fit)["(Intercept)", "2.5 %"],
      ci.ub = confint(peese_fit)["(Intercept)", "97.5 %"],
      pval = coef(summary(peese_fit))["(Intercept)", "Pr(>|t|)"],
      type = "PEESE"
    )
  }

  return(mod)
}

#' Correct PET-PEESE Estimates
#'
#' Filters the results of PET-PEESE and sets negative effect sizes to zero.
#'
#' @param data A data frame of results, including `ma_method` and `b` columns.
#' @return A data frame with added corrected rows for PET-PEESE.
add_pet_peese_corrected <- function(data) {
  pet_peese_corrected <- data %>%
    filter(ma_method == "pet-peese" & b < 0) %>%
    mutate(b = 0, ma_method = "pet-peese (corrected)")

  data_combined <- bind_rows(data, pet_peese_corrected)
  data_combined$ma_method <- factor(data_combined$ma_method, levels = unique(data_combined$ma_method))

  return(data_combined)
}

## P-Uniform Estimation ------------------------------------------------------

#' Calculate P-Uniform Star
#'
#' Computes the P-Uniform Star estimates for a dataset.
#'
#' @param dat A data frame containing `yi` (effect size) and `vi` (variance).
#' @return A list containing `b`, `ci.lb`, `ci.ub`, and `pval`.
calculate_puni_star <- function(dat) {
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

#' Calculate UWLS Estimates
#'
#' Performs Unweighted Least Squares (UWLS) estimation.
#'
#' @param dat A data frame containing `yi` (effect size) and `vi` (variance).
#' @return A list containing `b`, `ci.lb`, `ci.ub`, and `pval`.
calculate_uwls <- function(dat) {
  d <- dat$yi
  sed <- sqrt(dat$vi)
  Precision <- 1 / sed
  reg_uwls <- lm(d / sed ~ 0 + Precision)

  list(
    b = coef(summary(reg_uwls))["Precision", "Estimate"],
    ci.lb = confint(reg_uwls)["Precision", "2.5 %"],
    ci.ub = confint(reg_uwls)["Precision", "97.5 %"],
    pval = coef(summary(reg_uwls))["Precision", "Pr(>|t|)"]
  )
}

#' Calculate WAAP Estimates
#'
#' Performs Weighted Average of Adequately Powered (WAAP) estimation.
#'
#' @param dat A data frame containing `yi` (effect size) and `vi` (variance).
#' @return A list containing `b`, `ci.lb`, `ci.ub`, and `pval`.
calculate_waap <- function(dat) {
  d <- dat$yi
  sed <- sqrt(dat$vi)
  Precision <- 1 / sed
  reg_uwls <- lm(d / sed ~ 0 + Precision)
  UWLS <- coef(reg_uwls)["Precision"]
  powered <- sed < abs(UWLS) / 2.8

  if (sum(powered) < 2) {
    return(list(b = NA, ci.lb = NA, ci.ub = NA, pval = NA))
  }

  reg_waap <- lm(d[powered] / sed[powered] ~ 0 + Precision[powered])
  list(
    b = coef(summary(reg_waap))["Precision[powered]", "Estimate"],
    ci.lb = confint(reg_waap)["Precision[powered]", "2.5 %"],
    ci.ub = confint(reg_waap)["Precision[powered]", "97.5 %"],
    pval = coef(summary(reg_waap))["Precision[powered]", "Pr(>|t|)"]
  )
}
