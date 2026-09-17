# ABOUTME: Simulated meta-analytic dataset generator used to prototype every
# ABOUTME: new mechanism before it touches real Metapsy data (tests + vignette).

#' Simulate a meta-analytic dataset for multiverse prototyping
#'
#' Generates a study-level dataset with a known true effect, optional
#' between-study heterogeneity, clustered effect sizes, and categorical
#' study features usable as fork columns (comparator, format, risk of bias).
#'
#' @param n_studies Number of studies.
#' @param true_effect True mean Hedges' g.
#' @param tau Between-study SD of true effects.
#' @param es_per_study Integer vector sampled from for the number of effect
#'   sizes per study (dependency structure).
#' @param comparator_levels Character vector of comparator values to sample.
#' @param comparator_shift Named numeric vector: additive shift of the true
#'   effect per comparator level (makes comparator a genuine N fork).
#' @param prop_high_rob Proportion of studies rated high risk of bias.
#' @param seed Seed for reproducibility.
#' @return Data frame with columns study, es_id, yi, vi, comparator, format,
#'   rob (1 = high risk), n_arm.
#' @export
simulate_multiverse_data <- function(n_studies = 30,
                                     true_effect = 0.4,
                                     tau = 0.1,
                                     es_per_study = 1:2,
                                     comparator_levels = c("wl", "cau", "other"),
                                     comparator_shift = c(wl = 0.2, cau = 0,
                                                          other = 0.1),
                                     prop_high_rob = 0.3,
                                     seed = 1) {
  set.seed(seed)
  rows <- list()
  es_counter <- 0
  for (i in seq_len(n_studies)) {
    comp <- sample(comparator_levels, 1)
    theta_i <- true_effect + comparator_shift[[comp]] + stats::rnorm(1, 0, tau)
    n_arm <- sample(15:120, 1)
    k_es <- sample(es_per_study, 1)
    for (j in seq_len(k_es)) {
      es_counter <- es_counter + 1
      vi <- 2 / n_arm + theta_i^2 / (4 * n_arm)
      yi <- stats::rnorm(1, theta_i, sqrt(vi))
      rows[[es_counter]] <- data.frame(
        study = paste0("study_", sprintf("%02d", i)),
        es_id = es_counter,
        yi = yi,
        vi = vi,
        comparator = comp,
        format = sample(c("ind", "grp"), 1),
        rob = stats::rbinom(1, 1, prop_high_rob),
        n_arm = n_arm,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}
