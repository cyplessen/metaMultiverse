#' Meta-Analysis Estimators for Multiverse Analysis
#'
#' This file contains standardized estimator functions for various meta-analytic methods
#' used in multiverse analysis. All estimators follow a consistent interface and
#' error handling strategy.
#'
#' @name estimators
#' @keywords internal

# Suppress warnings for dynamically evaluated variables
globalVariables(c("b", "ci.lb", "ci.ub", "pval", "ma_method", "vi", "yi"))

#' Safe wrapper for estimator functions
#'
#' Provides consistent error handling across all estimators, returning
#' a standardized NA result when estimation fails.
#'
#' @param expr Expression to evaluate safely
#' @param method_name Character string identifying the method (for error messages)
#' @return Either the result of expr or universe_NA on error
#' @keywords internal
safe_call <- function(expr, method_name = "unknown") {
  tryCatch(expr, error = function(e) {
    message("[estimator:", method_name, "] ", e$message)
    universe_NA
  })
}

# =============================================================================
# STANDARD META-ANALYSIS METHODS
# =============================================================================

#' Fixed Effects Meta-Analysis
#'
#' Conducts fixed effects meta-analysis using the inverse variance method.
#' Assumes all studies estimate the same true effect size and that observed
#' differences are due to sampling error only.
#'
#' @param data Data frame containing yi (effect sizes) and vi (variances)
#' @return A universe_result object with estimate, confidence interval, and p-value
#' @details
#' Uses the metafor package with method = "FE". This method weights studies
#' by the inverse of their variance, giving more weight to precise studies.
#' Appropriate when heterogeneity is minimal or when testing for average effect
#' under the assumption of a common true effect.
#'
#' @references
#' Harrer, M., Cuijpers, P., Furukawa, T.A., & Ebert, D.D. (2021).
#' Doing Meta-Analysis with R: A Hands-On Guide.
#' Boca Raton, FL and London: Chapmann & Hall/CRC Press.
#' https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/
#' @keywords internal
fit_fe <- function(data) safe_call({
  mod <- metafor::rma(yi = data$yi, vi = data$vi, method = "FE")
  out <- new_universe_result(mod$b, mod$ci.lb, mod$ci.ub, mod$pval)
  attr(out, "method") <- "FE"
  out
}, method_name = "Fixed Effects")

#' Random Effects Meta-Analysis (REML)
#'
#' Conducts random effects meta-analysis using Restricted Maximum Likelihood (REML)
#' for between-study variance estimation. Accounts for heterogeneity between studies.
#'
#' @param data Data frame containing yi (effect sizes) and vi (variances)
#' @return A universe_result object with estimate, confidence interval, and p-value
#' @details
#' Uses REML estimation with adjusted step size (0.5) and increased iterations (2000)
#' for improved convergence. REML is generally preferred over method-of-moments
#' estimators as it produces less biased estimates of τ².
#'
#' @references
#' Harrer, M., Cuijpers, P., Furukawa, T.A., & Ebert, D.D. (2021).
#' Doing Meta-Analysis with R: A Hands-On Guide. Chapter 4.2.1.
#' https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/pooling-es.html#reml
#' @keywords internal
fit_reml <- function(data) safe_call({
  mod <- metafor::rma(yi = data$yi, vi = data$vi, method = "REML",
                      control = list(stepadj = 0.5, maxiter = 2000))
  out <- new_universe_result(mod$b, mod$ci.lb, mod$ci.ub, mod$pval)
  attr(out, "method") <- "REML"
  out
}, method_name = "REML")

# =============================================================================
# PUBLICATION BIAS CORRECTION METHODS
# =============================================================================

#' PET-PEESE Analysis
#'
#' Implements the Precision-Effect Test and Precision-Effect Estimate with
#' Standard Error (PET-PEESE) procedure for publication bias correction.
#'
#' @param data Data frame containing yi (effect sizes) and vi (variances)
#' @return A universe_result object with bias-corrected estimate
#' @details
#' PET-PEESE uses a two-stage procedure:
#' \enumerate{
#'   \item PET: Regresses effect sizes on standard errors to test for bias
#'   \item If PET p-value ≥ 0.10, use PET estimate (more conservative)
#'   \item If PET p-value < 0.10, switch to PEESE (regress on variances)
#' }
#'
#' The method assumes small-study effects are due to publication bias and
#' estimates the bias-corrected effect as the regression intercept.
#'
#' @references
#' Harrer, M., Cuijpers, P., Furukawa, T.A., & Ebert, D.D. (2021).
#' Doing Meta-Analysis with R: A Hands-On Guide. Chapter 9.3.
#' https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/pub-bias.html#pet-peese
#' @keywords internal
fit_pet.peese <- function(data) safe_call({

  # ========== PET (Precision-Effect Test) ==========
  pet_fit <- tryCatch(
    stats::lm(yi ~ sqrt(vi), weights = 1 / vi, data = data),
    error = function(e) NULL
  )

  if (is.null(pet_fit)) {
    out <- universe_NA
    attr(out, "method") <- "PET (failed)"
    return(out)
  }

  pet_p <- stats::coef(summary(pet_fit))["(Intercept)", "Pr(>|t|)"]

  # PET-PEESE decision rule: if PET p ≥ 0.10, use PET (more conservative)
  if (is.na(pet_p) || pet_p >= 0.10) {
    out <- new_universe_result(
      b     = stats::coef(pet_fit)["(Intercept)"],
      ci.lb = stats::confint(pet_fit)["(Intercept)", "2.5 %"],
      ci.ub = stats::confint(pet_fit)["(Intercept)", "97.5 %"],
      pval  = pet_p
    )
    attr(out, "method") <- "PET"
    return(out)
  }

  # ========== PEESE (Precision-Effect Estimate with Standard Error) ==========
  peese_fit <- tryCatch(
    stats::lm(yi ~ vi, weights = 1 / vi, data = data),
    error = function(e) NULL
  )

  if (is.null(peese_fit)) {
    out <- universe_NA
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
}, method_name = "PET-PEESE")

#' PET-PEESE with Negative Effect Correction
#'
#' Implements PET-PEESE with the constraint that negative bias-corrected
#' effects are set to zero, as recommended by Stanley & Doucouliagos (2014).
#'
#' @param data Data frame containing yi (effect sizes) and vi (variances)
#' @return A universe_result object with corrected estimate (minimum of 0)
#' @details
#' When PET-PEESE produces a negative estimate, this suggests that the
#' entire observed effect may be due to publication bias. The corrected
#' version sets such estimates to zero as a conservative approach.
#'
#' @references
#' Stanley, T. D., & Doucouliagos, H. (2014). Meta‐regression approximations
#' to reduce publication selection bias. Research Synthesis Methods, 5(1), 60-78.
#' @keywords internal
fit_pet.peese_corrected <- function(data) safe_call({
  out <- fit_pet.peese(data)
  if (!is.na(out$b) && out$b < 0) {
    out$b <- 0
    # Note: CI and p-value remain from original analysis for transparency
  }
  attr(out, "method") <- paste(attr(out, "method"), "(corrected)")
  out
}, method_name = "PET-PEESE Corrected")

#' P-Uniform* Analysis
#'
#' Conducts p-uniform* analysis for publication bias correction and effect
#' size estimation based on the distribution of significant p-values.
#'
#' @param data Data frame containing yi (effect sizes) and vi (variances)
#' @return A universe_result object with p-uniform* estimate
#' @details
#' P-uniform* method estimates effect size by examining the distribution of
#' statistically significant p-values. It assumes that in the absence of
#' publication bias, p-values should be uniformly distributed.
#'
#' The function automatically determines the appropriate testing direction (left vs. right)
#' based on the inverse-variance weighted mean of effect sizes. This data-driven
#' approach avoids arbitrary direction choices and is computationally efficient
#' for multiverse analyses involving thousands of specifications.
#'
#' Direction selection:
#' \itemize{
#'   \item If weighted mean ≥ 0: uses right-sided testing (H1: θ > 0)
#'   \item If weighted mean < 0: uses left-sided testing (H1: θ < 0)
#' }
#'
#' @references
#' van Aert, R. C., Wicherts, J. M., & van Assen, M. A. (2019).
#' Publication bias examined in meta-analyses from psychology and medicine:
#' A meta-meta-analysis. PLoS One, 14(4), e0215052.
#'
#' Aert, R. C. M. van, Wicherts, J. M., & Assen, M. A. L. M. van. (2022).
#' puniform: Meta-Analysis Methods Correcting for Publication Bias.
#' R package. https://cran.r-project.org/web/packages/puniform/index.html
#' @keywords internal
fit_puni_star <- function(data) safe_call({
  # Determine testing direction based on inverse-variance weighted mean
  weights <- 1 / data$vi
  weighted_mean <- sum(data$yi * weights, na.rm = TRUE) / sum(weights, na.rm = TRUE)
  side <- if (weighted_mean >= 0) "right" else "left"

  pu <- tryCatch(
    puniform::puni_star(yi = data$yi,
                        vi = data$vi,
                        side = side),
    error = function(e) NULL
  )

  if (is.null(pu)) {
    out <- universe_NA
    attr(out, "method") <- paste0("p-uniform* (failed, side=", side, ")")
    return(out)
  }

  out <- new_universe_result(
    b     = pu$est,
    ci.lb = pu$ci.lb,
    ci.ub = pu$ci.ub,
    pval  = pu$pval.0
  )
  attr(out, "method") <- paste0("p-uniform* (", side, ")")
  out
}, method_name = "p-uniform*")

# =============================================================================
# SELECTION MODEL METHODS
# =============================================================================

#' Unweighted Least Squares (UWLS)
#'
#' Implements the Unweighted Least Squares approach for meta-analysis,
#' which gives equal weight to all studies regardless of precision.
#'
#' @param data Data frame containing yi (effect sizes) and vi (variances)
#' @return A universe_result object with UWLS estimate
#' @details
#' UWLS can be more robust to publication bias than inverse-variance weighting
#' when small studies are systematically biased. The method regresses
#' standardized effect sizes on precision (1/SE).
#'
#' @references
#' Stanley, T. D., Doucouliagos, H., & Ioannidis, J. P. A. (2022).
#' Beyond random effects: When small-study findings are more heterogeneous.
#' Advances in Methods and Practices in Psychological Science, 5(4).
#' https://doi.org/10.1177/25152459221120427
#' @keywords internal
fit_uwls <- function(data) safe_call({
  d         <- data$yi
  sed       <- sqrt(data$vi)
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
}, method_name = "UWLS")

#' Weighted Average of Adequately Powered Studies (WAAP)
#'
#' Implements WAAP, which restricts analysis to studies with adequate
#' statistical power, potentially reducing publication bias effects.
#'
#' @param data Data frame containing yi (effect sizes) and vi (variances)
#' @param power_threshold Numeric. The divisor for power calculation (default: 2.8)
#' @return A universe_result object with WAAP estimate
#' @details
#' WAAP first estimates effect size using UWLS, then identifies "adequately powered"
#' studies as those with SE < |UWLS estimate|/2.8. The final estimate uses only
#' these powered studies. Requires at least 2 powered studies for estimation.
#'
#' The power threshold of 2.8 corresponds to approximately 80% power to detect
#' the UWLS estimate at α = 0.05.
#'
#' @references
#' Stanley, T. D., Doucouliagos, H., & Ioannidis, J. P. A. (2022).
#' Beyond random effects: When small-study findings are more heterogeneous.
#' Advances in Methods and Practices in Psychological Science, 5(4).
#' https://doi.org/10.1177/25152459221120427
#' @keywords internal
fit_waap <- function(data, power_threshold = 2.8) safe_call({
  d         <- data$yi
  sed       <- sqrt(data$vi)
  Precision <- 1 / sed

  # Step 1: UWLS estimation to determine power threshold
  reg_uwls <- stats::lm(d / sed ~ 0 + Precision)
  UWLS     <- stats::coef(reg_uwls)["Precision"]

  # Step 2: Identify adequately powered studies
  powered <- sed < abs(UWLS) / power_threshold

  if (sum(powered) < 2) {
    out <- universe_NA
    attr(out, "method") <- paste0("WAAP (failed - <2 powered studies out of ", length(powered), ")")
    return(out)
  }

  # Step 3: WAAP regression using only powered studies
  reg <- stats::lm(d[powered] / sed[powered] ~ 0 + Precision[powered])

  out <- new_universe_result(
    b     = stats::coef(reg)["Precision[powered]"],
    ci.lb = stats::confint(reg)["Precision[powered]", "2.5 %"],
    ci.ub = stats::confint(reg)["Precision[powered]", "97.5 %"],
    pval  = stats::coef(summary(reg))["Precision[powered]", "Pr(>|t|)"]
  )
  attr(out, "method") <- paste0("WAAP (", sum(powered), "/", length(powered), " powered)")
  out
}, method_name = "WAAP")

# =============================================================================
# ALTERNATIVE HETEROGENEITY ESTIMATORS
# =============================================================================

#' Paule-Mandel Random Effects
#'
#' Implements random effects meta-analysis using the Paule-Mandel method
#' for between-study variance estimation.
#'
#' @param data Data frame containing yi (effect sizes) and vi (variances)
#' @return A universe_result object with Paule-Mandel estimate
#' @details
#' The Paule-Mandel method estimates τ² by equating the weighted sum of squares
#' to its expected value. It can perform better than DerSimonian-Laird in
#' certain conditions and provides an exact solution.
#'
#' @references
#' Paule, R. C., & Mandel, J. (1982). Consensus values and weighting factors.
#' Journal of Research of the National Bureau of Standards, 87(5), 377-385.
#' https://doi.org/10.6028/jres.087.022
#' @keywords internal
fit_pm <- function(data) safe_call({
  mod <- tryCatch(
    metafor::rma(yi = data$yi,
                 vi = data$vi,
                 method = "PM",
                 test   = "z"),
    error = function(e) NULL
  )

  if (is.null(mod)) {
    out <- universe_NA
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
}, method_name = "Paule-Mandel")

#' Hartung-Knapp with Sidik-Jonkman
#'
#' Implements random effects meta-analysis with Hartung-Knapp confidence intervals
#' and Sidik-Jonkman variance estimation.
#'
#' @param data Data frame containing yi (effect sizes) and vi (variances)
#' @return A universe_result object with HK/SJ estimate
#' @details
#' The Hartung-Knapp method provides improved confidence interval coverage
#' by using a t-distribution instead of normal distribution. Combined with
#' Sidik-Jonkman τ² estimation, this can provide better small-sample properties.
#'
#' @references
#' Knapp, G., & Hartung, J. (2003). Improved tests for a random effects meta-regression
#' with a single covariate. Statistics in Medicine, 22(17), 2693-2710.
#' https://doi.org/10.1002/sim.1482
#'
#' Sidik, K., & Jonkman, J. N. (2005). Simple heterogeneity variance estimation for meta-analysis.
#' Journal of the Royal Statistical Society, Series C, 54(2), 367-384.
#' https://doi.org/10.1111/j.1467-9876.2005.00489.x
#' @keywords internal
fit_hk_sj <- function(data) safe_call({
  mod <- tryCatch(
    meta::metagen(TE      = data$yi,
                  seTE    = sqrt(data$vi),
                  method.random.ci = "HK",
                  method.tau       = "SJ"),
    error = function(e) NULL
  )

  if (is.null(mod)) {
    out <- universe_NA
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
}, method_name = "Hartung-Knapp/Sidik-Jonkman")

# =============================================================================
# DEPENDENCY MODELING METHODS
# =============================================================================

#' Three-Level Meta-Analysis
#'
#' Implements three-level meta-analysis to model dependency between effect sizes
#' from the same study using multilevel modeling.
#'
#' @param data Data frame containing yi, vi, es_id (effect size ID), and study
#' @return A universe_result object with three-level estimate
#' @details
#' Three-level models account for:
#' \enumerate{
#'   \item Level 1: Sampling variance within effect sizes
#'   \item Level 2: Variance between effect sizes within studies
#'   \item Level 3: Variance between studies
#' }
#'
#' Uses sparse matrix methods for computational efficiency with large datasets.
#'
#' @references
#' Harrer, M., Cuijpers, P., Furukawa, T.A., & Ebert, D.D. (2021).
#' Doing Meta-Analysis with R: A Hands-On Guide. Chapter 10.
#' https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/multilevel-ma.html
#' @keywords internal
fit_three_level <- function(data) safe_call({
  mod <- metafor::rma.mv(data = data, yi = yi, V = vi,
                         random = ~ 1 | study/es_id,
                         method = "REML",
                         sparse = TRUE)
  out <- new_universe_result(mod$b, mod$ci.lb, mod$ci.ub, mod$pval)
  attr(out, "method") <- "3-level"
  out
}, method_name = "Three-level")

#' Robust Variance Estimation (RVE)
#'
#' Implements robust variance estimation with cluster-robust standard errors
#' to handle dependency between effect sizes.
#'
#' @param data Data frame containing yi, vi, es_id, and study
#' @return A universe_result object with RVE estimate
#' @details
#' RVE uses sandwich-type variance estimators that are robust to model
#' misspecification. The clubSandwich package provides bias-corrected
#' standard errors that perform well even with small numbers of clusters.
#'
#' Studies are used as clusters, making this approach suitable when
#' multiple effect sizes come from the same study.
#'
#' @references
#' Tipton, E. (2015). Small sample adjustments for robust variance estimation with meta-regression.
#' Psychological Methods, 20(3), 375-393. https://doi.org/10.1037/met0000011
#'
#' Tipton, E., & Pustejovsky, J. E. (2015). Small-sample adjustments for tests of moderators
#' and model fit using robust variance estimation in meta-regression. Journal of Educational
#' and Behavioral Statistics, 40(6), 604-634. https://doi.org/10.3102/1076998615606099
#' @keywords internal
fit_rve <- function(data) safe_call({
  mod <- metafor::rma.mv(data = data, yi = yi, V = vi,
                         random = ~ 1 | study/es_id,
                         method = "REML",
                         sparse = TRUE)

  # Apply robust variance estimation with cluster-robust standard errors
  robust_results <- metafor::robust(mod, cluster = data$study, clubSandwich = TRUE)

  out <- new_universe_result(
    b     = robust_results$b,
    ci.lb = robust_results$ci.lb,
    ci.ub = robust_results$ci.ub,
    pval  = robust_results$pval
  )
  attr(out, "method") <- "RVE"
  out
}, method_name = "Robust Variance Estimation")

# =============================================================================
# BAYESIAN METHODS
# =============================================================================

#' Bayesian Meta-Analysis
#'
#' Implements Bayesian meta-analysis using default priors for effect size
#' and between-study heterogeneity.
#'
#' @param data Data frame containing yi (effect sizes) and vi (variances)
#' @return A universe_result object with Bayesian posterior estimates
#' @details
#' Uses the bayesmeta package with default priors:
#' \itemize{
#'   \item Effect size: Improper uniform prior
#'   \item Heterogeneity (τ): Half-Cauchy(0, 0.5) prior
#' }
#'
#' Returns posterior median as point estimate with 95% credible intervals.
#' No p-value is provided as this is not typically meaningful in Bayesian analysis.
#'
#' @references
#' Röver, C. (2020). Bayesian random‐effects meta‐analysis using the bayesmeta R package.
#' Journal of Statistical Software, 93(6), 1-51.
#' @keywords internal
fit_bayesmeta <- function(data) safe_call({
  bm <- tryCatch(
    bayesmeta::bayesmeta(y = data$yi,
                         sigma = sqrt(data$vi)),
    error = function(e) NULL
  )

  if (is.null(bm)) {
    out <- universe_NA
    attr(out, "method") <- "bayesmeta (failed)"
    return(out)
  }

  s <- bm$summary   # Posterior summary data.frame

  out <- new_universe_result(
    b     = s["median",    "mu"],
    ci.lb = s["95% lower", "mu"],
    ci.ub = s["95% upper", "mu"],
    pval  = NA_real_              # Bayesian analysis: no p-value
  )
  attr(out, "method") <- "bayesmeta"
  out
}, method_name = "Bayesian Meta-Analysis")
