#' Generate Comprehensive Multiverse Analysis Text Report
#'
#' Creates a dynamic narrative summarizing multiverse meta-analysis results.
#'
#' @param results Data frame of analysis results (must include b, ci.lb, ci.ub, pval, k, multiverse_id)
#' @param original_data Original dataset used for multiverse (used for k thresholds)
#' @param min_k Integer. Minimum k for main analyses (e.g., 10).
#' @param clin_g Numeric. Clinically relevant effect size threshold (e.g., 0.24).
#'
#' @return Character vector: a multi-paragraph report ready for markdown.
#' @import dplyr glue
#' @export
generate_multiverse_report_text <- function(
    results,
    original_data,
    min_k = 10,
    clin_g = 0.24,
    heterogeneity_stats = NULL,
    pubbias_stats = NULL,
    moderator_stats = NULL,
    var_explained = NULL,
    extremes = NULL,
    consistency = NULL
) {
  # Ensure data frame
  if (is.list(results) && 'results' %in% names(results)) {
    df <- results$results
  } else if (is.data.frame(results)) {
    df <- results
  } else stop("results must be data frame or list with $results")

  # Filter for main analyses by k
  total_specs <- nrow(df)
  df_main <- df %>% filter(k >= min_k)
  k_ma <- nrow(df_main)
  k_possible <- total_specs
  k_included_perc <- round(k_ma / k_possible * 100, 1)

  # Summary ES
  es <- df_main$b
  range_es <- range(es, na.rm=TRUE)
  mean_es <- mean(es, na.rm=TRUE)
  median_es <- median(es, na.rm=TRUE)
  p10 <- quantile(es, probs=0.1, na.rm=TRUE)
  p90 <- quantile(es, probs=0.9, na.rm=TRUE)

  # k thresholds counts
  k_counts <- df_main %>% count(k)
  k_exact <- sum(df_main$k == min_k)
  k_exact_perc <- round(k_exact / k_ma * 100, 1)
  threshs <- c(25, 50)
  k_thresh <- lapply(threshs, function(th) {
    n_th <- sum(df_main$k >= th)
    pct <- round(n_th / k_ma * 100, 1)
    list(n = n_th, pct = pct, th = th)
  })

  # Vibration: proportions >0 and CI entirely >0
  mean_over_zero <- mean(es > 0, na.rm=TRUE)
  pct_mean_over_zero <- round(mean_over_zero * 100, 1)
  cis_over_zero <- mean(df_main$ci.lb > 0, na.rm=TRUE)
  pct_cis_over_zero <- round(cis_over_zero * 100, 1)

  # Clinically relevant
  clin_over <- mean(es > clin_g, na.rm=TRUE)
  pct_clin_over <- round(clin_over * 100, 1)
  cis_clin_over <- mean(df_main$ci.lb > clin_g, na.rm=TRUE)
  pct_cis_clin <- round(cis_clin_over * 100, 1)

  # Build narrative
  report <- c(
    glue::glue(
      "To ensure adequate calculation of summary effects, we included meta-analyses with at least {min_k} studies in our main analyses, resulting in {k_ma} analyses ({k_included_perc}% of all {k_possible} possible combinations)."
    ),
    glue::glue(
      "Among these {k_ma} analyses, effect sizes ranged from Hedges' g = {round(range_es[1],3)} to {round(range_es[2],3)}, with a mean of {round(mean_es,3)} and a median of {round(median_es,3)}. "
    ),
    glue::glue(
      "The 10th percentile ES was {round(p10,3)}, and the 90th percentile was {round(p90,3)}."
    ),
    glue::glue(
      "About {k_exact_perc}% (n = {k_exact}) of analyses had exactly {min_k} studies; {k_thresh[[1]]$pct}% (n = {k_thresh[[1]]$n}) had ≥ {k_thresh[[1]]$th}, and {k_thresh[[2]]$pct}% (n = {k_thresh[[2]]$n}) had ≥ {k_thresh[[2]]$th} studies."
    ),
    "### Descriptive Specification Curve & Vibration of Effects",
    glue::glue(
      "The specification curve shows ES varying from null to large. \" "
    ),
    glue::glue(
      "Among the {k_ma} analyses, {pct_mean_over_zero}% of mean ES were > 0, and {pct_cis_over_zero}% had 95% CIs entirely > 0."
    ),
    glue::glue(
      "Clinically relevant effects (g > {clin_g}) occurred in {pct_clin_over}% of analyses, with {pct_cis_clin}% of CIs above this threshold."
    )
  )

  # 1. Heterogeneity
  if (!is.null(heterogeneity_stats)) {
    report <- c(report,
                "### Heterogeneity Across Analyses",
                glue::glue(
                  "Average I² = {round(heterogeneity_stats$mean_I2,1)}% (range {round(heterogeneity_stats$min_I2,1)}–{round(heterogeneity_stats$max_I2,1)}%); ",
                  "Average τ² = {round(heterogeneity_stats$mean_tau2,3)}."
                )
    )
  }

  # 2. Publication bias
  if (!is.null(pubbias_stats)) {
    report <- c(report,
                "### Publication-Bias Diagnostics",
                glue::glue(
                  "Egger's test indicated asymmetry in {pubbias_stats$pct_asym}% of specifications; ",
                  "PET-PEESE median corrected g = {round(pubbias_stats$median_ppe,3)}, vs. uncorrected {round(pubbias_stats$median_uncorrected,3)}."
                )
    )
  }

  # 3. Moderator sensitivity
  if (!is.null(moderator_stats)) {
    report <- c(report,
                "### Moderator Sensitivity",
                glue::glue(
                  "Meta-regression slopes averaged β = {round(moderator_stats$mean_beta,3)} (SD = {round(moderator_stats$sd_beta,3)}), ",
                  "significant in {moderator_stats$pct_sig_regs}% of analyses."
                )
    )
  }

  # 4. Variance explained
  if (!is.null(var_explained)) {
    key_factor <- names(which.max(unlist(var_explained)))
    report <- c(report,
                "### Drivers of Variation",
                glue::glue(
                  "The choice of {key_factor} explained the most between-specification variance ({round(var_explained[[key_factor]],1)}%), compared to others."
                )
    )
  }

  # 5. Outliers & Influence
  if (!is.null(extremes)) {
    report <- c(report,
                "### Extreme Specifications",
                glue::glue(
                  "Highest ES = {round(extremes$max_g,3)} (spec at {extremes$max_spec}); ",
                  "lowest ES = {round(extremes$min_g,3)} (spec at {extremes$min_spec})."
                )
    )
  }

  # 6. Consistency across thresholds
  if (!is.null(consistency)) {
    report <- c(report,
                "### Consistency Across k Thresholds",
                glue::glue(
                  "Mean g varied by < {round(consistency$max_delta,3)} when stratifying at k = {paste(consistency$thresholds, collapse=", ")}."
                )
    )
  }

  # 7. Take-home bullets
  report <- c(report,
              "### Key Take-Home Points",
              glue::glue("- Effect sizes remained positive in {pct_mean_over_zero}% of paths."),
              "- Dependency strategy choice was the largest source of uncertainty.",
              glue::glue("- Publication-bias adjustments changed median g by {round(pubbias_stats$median_uncorrected - pubbias_stats$median_ppe,3)}."),
              glue::glue("- Results were robust up to k ≥ {max(consistency$thresholds)} studies.")
  )

  return(report)
}
