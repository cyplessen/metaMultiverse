# =============================================================================
# PRE/POST (CHANGE-SCORE) DESIGNS: EFFECT-SIZE COMPUTATION AS A "HOW" FACTOR
# =============================================================================
#
# metaMultiverse starts from precomputed yi / vi. When the raw data are
# arm-level pre/post means and SDs, the choice of effect-size metric (post-test
# SMD, change-score SMD, standardized mean change with an assumed pre-post
# correlation, adjusted between-group differences) is itself an analytic
# decision. The functions in this file add that decision as a "how" factor on
# top of the existing which-factor, dependency and estimator machinery.
#
# Sign convention: positive means group 1 has the HIGHER post-test score, or
# LESS pre-to-post change, than group 2. For symptom scales this means
# "favours group 2".

# -----------------------------------------------------------------------------
# helpers
# -----------------------------------------------------------------------------

.pp_required_cols <- c(
  "study", "n_1", "n_2",
  "pre_1_m", "pre_1_sd", "pre_2_m", "pre_2_sd",
  "post_1_m", "post_1_sd", "post_2_m", "post_2_sd"
)
.pp_optional_cols <- c(
  "n_1_post", "n_2_post",
  "change_1_m", "change_1_sd", "change_2_m", "change_2_sd",
  "adj_diff", "adj_diff_lb", "adj_diff_ub", "adj_ci_level",
  "post_sd_imputed"
)
.pp_post_metrics <- c("post_smd", "adjusted_post_smd")
.pp_imputed_rules <- c("borrow", "exclude", "change_smd")
.pp_metafor_keys <- c("dl", "dl_hksj", "reml_hksj", "pm_hksj")

.pp_fill_optional <- function(data) {
  for (col in .pp_optional_cols) {
    if (!col %in% names(data)) {
      data[[col]] <- if (col == "post_sd_imputed") FALSE else NA_real_
    }
  }
  data$post_sd_imputed <- data$post_sd_imputed %in% TRUE
  data
}

# Parse a metric name into its kind and (where applicable) the assumed
# pre-post correlation r. Returns NULL for an unknown metric.
.pp_parse_metric <- function(metric) {
  if (!is.character(metric) || length(metric) != 1 || is.na(metric)) return(NULL)
  if (metric %in% .pp_post_metrics) return(list(kind = metric, r = NA_real_))
  m <- regmatches(metric, regexec("^(smc|change_smd)_r([0-9]*\\.?[0-9]+)$", metric))[[1]]
  if (length(m) != 3) return(NULL)
  r <- as.numeric(m[3])
  if (is.na(r) || r <= -1 || r >= 1) return(NULL)
  list(kind = m[2], r = r)
}

.pp_stop_unknown_metric <- function(metric) {
  stop("compute_pre_post_es(): unknown metric '", metric, "'. Use 'post_smd', ",
       "'change_smd_r<r>' (e.g. 'change_smd_r0.5'), 'smc_r<r>' (e.g. 'smc_r0.5') ",
       "or 'adjusted_post_smd'.", call. = FALSE)
}

.pp_es_post_smd <- function(dat) {
  n1 <- ifelse(is.na(dat$n_1_post), dat$n_1, dat$n_1_post)
  n2 <- ifelse(is.na(dat$n_2_post), dat$n_2, dat$n_2_post)
  es <- metafor::escalc("SMD", m1i = dat$post_1_m, sd1i = dat$post_1_sd, n1i = n1,
                        m2i = dat$post_2_m, sd2i = dat$post_2_sd, n2i = n2)
  dat$yi <- as.numeric(es$yi); dat$vi <- as.numeric(es$vi)
  dat
}

.pp_es_change_smd_reported <- function(dat) {
  # SMD of change scores with REPORTED change means and SDs; group 2 change
  # minus group 1 change so that positive = group 2 improved more (same sign
  # as the post-test SMD when lower scores are better)
  es <- metafor::escalc("SMD", m1i = dat$change_2_m, sd1i = dat$change_2_sd, n1i = dat$n_2,
                        m2i = dat$change_1_m, sd2i = dat$change_1_sd, n2i = dat$n_1)
  dat$yi <- as.numeric(es$yi); dat$vi <- as.numeric(es$vi)
  dat
}

.pp_es_change_smd <- function(dat, r) {
  # SMD of change scores for every row. Change mean = pre - post unless a
  # change mean is reported; change SD = reported change SD where available,
  # otherwise imputed from the pre and post SDs under pre-post correlation r
  # (Cochrane Handbook 6.5.2.8): sqrt(sd_pre^2 + sd_post^2 - 2 r sd_pre sd_post).
  # Group 2 change minus group 1 change, as in .pp_es_change_smd_reported().
  impute <- function(sd_pre, sd_post) sqrt(sd_pre^2 + sd_post^2 - 2 * r * sd_pre * sd_post)
  chg_1 <- ifelse(is.na(dat$change_1_m), dat$pre_1_m - dat$post_1_m, dat$change_1_m)
  chg_2 <- ifelse(is.na(dat$change_2_m), dat$pre_2_m - dat$post_2_m, dat$change_2_m)
  sd_1 <- ifelse(is.na(dat$change_1_sd), impute(dat$pre_1_sd, dat$post_1_sd), dat$change_1_sd)
  sd_2 <- ifelse(is.na(dat$change_2_sd), impute(dat$pre_2_sd, dat$post_2_sd), dat$change_2_sd)
  es <- metafor::escalc("SMD", m1i = chg_2, sd1i = sd_2, n1i = dat$n_2,
                        m2i = chg_1, sd2i = sd_1, n2i = dat$n_1)
  dat$yi <- as.numeric(es$yi); dat$vi <- as.numeric(es$vi)
  dat
}

.pp_es_smc <- function(dat, r) {
  # difference of standardized mean changes, (post - pre) / sd_pre per arm,
  # Becker (1988) variance; group 1 minus group 2
  e1 <- metafor::escalc("SMCR", m1i = dat$post_1_m, m2i = dat$pre_1_m, sd1i = dat$pre_1_sd,
                        ni = dat$n_1, ri = rep(r, nrow(dat)))
  e2 <- metafor::escalc("SMCR", m1i = dat$post_2_m, m2i = dat$pre_2_m, sd1i = dat$pre_2_sd,
                        ni = dat$n_2, ri = rep(r, nrow(dat)))
  dat$yi <- as.numeric(e1$yi) - as.numeric(e2$yi)
  dat$vi <- as.numeric(e1$vi) + as.numeric(e2$vi)
  dat
}

.pp_es_adjusted <- function(dat) {
  # adjusted between-group difference at post (group 1 minus group 2, raw
  # units) standardized by the pooled post SD; SE from the reported CI
  z <- stats::qnorm(1 - (1 - dat$adj_ci_level) / 2)
  sd_pool <- sqrt(((dat$n_1 - 1) * dat$post_1_sd^2 + (dat$n_2 - 1) * dat$post_2_sd^2) /
                    (dat$n_1 + dat$n_2 - 2))
  se_raw <- (dat$adj_diff_ub - dat$adj_diff_lb) / (2 * z)
  dat$yi <- dat$adj_diff / sd_pool
  dat$vi <- (se_raw / sd_pool)^2
  dat
}

# -----------------------------------------------------------------------------
# exported
# -----------------------------------------------------------------------------

#' Compute effect sizes from arm-level pre/post data
#'
#' Turns a data frame of arm-level pre/post summary statistics into
#' \code{yi}/\code{vi} under a chosen effect-size metric, so that the metric
#' can be varied as a "how" factor in a multiverse (see
#' \code{\link{run_pre_post_multiverse}}).
#'
#' @param data Data frame with one row per study (or study x outcome).
#'   Required columns: \code{study}, \code{n_1}, \code{n_2}, \code{pre_1_m},
#'   \code{pre_1_sd}, \code{pre_2_m}, \code{pre_2_sd}, \code{post_1_m},
#'   \code{post_1_sd}, \code{post_2_m}, \code{post_2_sd} (means and SDs of
#'   group 1 and group 2 at pre and post). Optional columns:
#'   \itemize{
#'     \item \code{n_1_post}, \code{n_2_post}: analysed n at post, used for the
#'       post-test SMD when present (otherwise \code{n_1}, \code{n_2}).
#'     \item \code{change_1_m}, \code{change_1_sd}, \code{change_2_m},
#'       \code{change_2_sd}: reported change scores (pre minus post, so that a
#'       positive change is an improvement on a symptom scale). Used by the
#'       \code{"change_smd"} rule for \code{imputed_post_sd} and, where
#'       present, by the \code{"change_smd_r<r>"} metric.
#'     \item \code{adj_diff}, \code{adj_diff_lb}, \code{adj_diff_ub},
#'       \code{adj_ci_level}: a reported adjusted between-group difference at
#'       post (group 1 minus group 2, raw units) with its confidence interval
#'       and the interval's level (e.g. 0.95). Used by
#'       \code{"adjusted_post_smd"}.
#'     \item \code{post_sd_imputed}: logical, \code{TRUE} when the post SD was
#'       not reported and has been filled in (e.g. borrowed from the
#'       change-score SD). Governs how the row enters the post-test metrics;
#'       see \code{imputed_post_sd}.
#'   }
#' @param metric Effect-size metric, one of:
#'   \describe{
#'     \item{\code{"post_smd"}}{Post-test standardized mean difference
#'       (Hedges' g via \code{metafor::escalc("SMD")}), group 1 minus group 2.}
#'     \item{\code{"change_smd_r<r>"}}{Standardized mean difference of the
#'       pre-to-post change scores, e.g. \code{"change_smd_r0.5"}. The change
#'       mean is \code{pre - post} (or the reported change mean where present)
#'       and the change SD is the reported change SD where present, otherwise
#'       imputed from the pre and post SDs under pre-post correlation
#'       \code{r} as \code{sqrt(sd_pre^2 + sd_post^2 - 2 r sd_pre sd_post)}
#'       (Cochrane Handbook, section 6.5.2.8). Group 2 change minus group 1
#'       change, so the sign matches \code{"post_smd"} on a symptom scale.}
#'     \item{\code{"smc_r<r>"}}{Difference in standardized mean change,
#'       \code{(post - pre) / sd_pre} per arm with Becker's (1988) variance
#'       under pre-post correlation \code{r}, e.g. \code{"smc_r0.5"}; group 1
#'       minus group 2 (Morris, 2008).}
#'     \item{\code{"adjusted_post_smd"}}{The reported adjusted between-group
#'       difference at post divided by the pooled post SD, with the standard
#'       error taken from the reported confidence interval, for rows where
#'       \code{adj_diff} is available; the post-test SMD (\code{"post_smd"})
#'       for all other rows.}
#'   }
#' @param imputed_post_sd How rows flagged \code{post_sd_imputed = TRUE} enter
#'   the post-test metrics (\code{"post_smd"}, \code{"adjusted_post_smd"}):
#'   \code{"borrow"} (default) uses the imputed post SD as given,
#'   \code{"exclude"} drops those rows, \code{"change_smd"} replaces the
#'   post-test SMD by the SMD of the reported change scores for those rows
#'   only. Ignored for the \code{"change_smd_r<r>"} and \code{"smc_r<r>"}
#'   metrics, which do not use the post SD in that way; the output column then
#'   reads \code{"not_applicable"}.
#'
#' @return \code{data} with columns \code{yi}, \code{vi}, \code{es_metric} and
#'   \code{imputed_post_sd} appended, sorted by \code{study}. Rows for which
#'   the metric cannot be computed (missing inputs, non-positive variance) are
#'   dropped.
#'
#' @details
#' Sign convention: positive means group 1 scored higher at post-test (or
#' improved less) than group 2. For symptom scales, positive favours group 2.
#'
#' For a trial that reports only baseline and change scores, the post-test
#' and change-score metrics can differ in sign when the groups differ at
#' baseline; varying \code{metric} and \code{imputed_post_sd} across the
#' multiverse makes that dependence visible.
#'
#' @references
#' Becker, B. J. (1988). Synthesizing standardized mean-change measures.
#' British Journal of Mathematical and Statistical Psychology, 41(2), 257-278.
#' \doi{10.1111/j.2044-8317.1988.tb00901.x}
#'
#' Higgins, J. P. T., Li, T., & Deeks, J. J. (2023). Choosing effect measures
#' and computing estimates of effect. In: Cochrane Handbook for Systematic
#' Reviews of Interventions, version 6.4, section 6.5.2.8.
#'
#' Morris, S. B. (2008). Estimating effect sizes from pretest-posttest-control
#' group designs. Organizational Research Methods, 11(2), 364-386.
#' \doi{10.1177/1094428106291059}
#'
#' @seealso \code{\link{run_pre_post_multiverse}} to vary the metric across a
#'   multiverse; \code{\link{register_metafor_estimators}} for the
#'   DerSimonian-Laird and Hartung-Knapp estimators.
#'
#' @examples
#' d <- data.frame(
#'   study = c("A", "B", "C"),
#'   n_1 = c(30, 40, 25), n_2 = c(32, 38, 27),
#'   pre_1_m = c(24, 22, 25), pre_1_sd = c(6, 5, 7),
#'   pre_2_m = c(23, 23, 24), pre_2_sd = c(6, 5, 6),
#'   post_1_m = c(14, 12, 15), post_1_sd = c(8, 7, 9),
#'   post_2_m = c(13, 12, 12), post_2_sd = c(8, 7, 8)
#' )
#' compute_pre_post_es(d, "post_smd")[, c("study", "yi", "vi")]
#' compute_pre_post_es(d, "change_smd_r0.5")[, c("study", "yi", "vi")]
#' compute_pre_post_es(d, "smc_r0.5")[, c("study", "yi", "vi")]
#' @export
compute_pre_post_es <- function(data, metric = "post_smd",
                                imputed_post_sd = c("borrow", "exclude", "change_smd")) {
  imputed_post_sd <- match.arg(imputed_post_sd)
  if (!is.data.frame(data)) stop("compute_pre_post_es(): 'data' must be a data frame")
  missing_cols <- setdiff(.pp_required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("compute_pre_post_es(): missing columns: ", paste(missing_cols, collapse = ", "))
  }
  parsed <- .pp_parse_metric(metric)
  if (is.null(parsed)) .pp_stop_unknown_metric(metric)

  dat <- .pp_fill_optional(as.data.frame(data))
  imputed <- dat$post_sd_imputed

  if (parsed$kind %in% .pp_post_metrics) {
    out <- switch(
      imputed_post_sd,
      exclude = .pp_es_post_smd(dat[!imputed, , drop = FALSE]),
      borrow = .pp_es_post_smd(dat),
      change_smd = rbind(
        .pp_es_post_smd(dat[!imputed, , drop = FALSE]),
        .pp_es_change_smd_reported(dat[imputed, , drop = FALSE])
      )
    )
    if (parsed$kind == "adjusted_post_smd") {
      has_adj <- !is.na(out$adj_diff)
      if (any(has_adj)) {
        out <- rbind(out[!has_adj, , drop = FALSE], .pp_es_adjusted(out[has_adj, , drop = FALSE]))
      }
    }
  } else if (parsed$kind == "change_smd") {
    out <- .pp_es_change_smd(dat, parsed$r)
  } else {
    out <- .pp_es_smc(dat, parsed$r)
  }

  out <- out[!is.na(out$yi) & !is.na(out$vi) & out$vi > 0, , drop = FALSE]
  out$es_metric <- metric
  out$imputed_post_sd <- if (parsed$kind %in% .pp_post_metrics) imputed_post_sd else "not_applicable"
  rownames(out) <- NULL
  out[order(out$study), , drop = FALSE]
}

#' Register DerSimonian-Laird and Hartung-Knapp estimators
#'
#' Adds four metafor-based random-effects estimators to the estimator
#' registry so that they can be named in \code{ma_methods}:
#' \describe{
#'   \item{\code{"dl"}}{DerSimonian-Laird with the usual normal-theory
#'     (z) confidence interval.}
#'   \item{\code{"dl_hksj"}}{DerSimonian-Laird with the Hartung-Knapp-Sidik-Jonkman
#'     adjustment (metafor's \code{test = "knha"}: t-distribution with the
#'     Knapp-Hartung variance).}
#'   \item{\code{"reml_hksj"}}{REML with the Hartung-Knapp-Sidik-Jonkman
#'     adjustment.}
#'   \item{\code{"pm_hksj"}}{Paule-Mandel with the Hartung-Knapp-Sidik-Jonkman
#'     adjustment.}
#' }
#' All four accept the \code{"aggregate"}, \code{"select_min"} and
#' \code{"select_max"} dependency strategies.
#'
#' These estimators are \emph{not} registered when the package loads, so that
#' the default method set used by \code{\link{create_multiverse_specifications}}
#' (all registered methods) is unchanged for existing analyses. Call this
#' function once per session before naming any of the four keys in
#' \code{ma_methods}; calling it again is harmless.
#'
#' The package's built-in \code{"hk-sj"} method is the Hartung-Knapp interval
#' with a Sidik-Jonkman tau-squared estimate (via the meta package); the
#' \code{*_hksj} keys registered here pair the same interval with the
#' DerSimonian-Laird, REML or Paule-Mandel tau-squared estimate instead.
#'
#' @return Invisibly, \code{\link{list_ma_methods}()} after registration.
#' @references
#' IntHout, J., Ioannidis, J. P. A., & Borm, G. F. (2014). The Hartung-Knapp-Sidik-Jonkman
#' method for random effects meta-analysis is straightforward and considerably
#' outperforms the standard DerSimonian-Laird method. BMC Medical Research
#' Methodology, 14, 25. \doi{10.1186/1471-2288-14-25}
#' @examples
#' register_metafor_estimators()
#' all(c("dl", "dl_hksj", "reml_hksj", "pm_hksj") %in% list_ma_methods())
#' @export
register_metafor_estimators <- function() {
  make <- function(method, test, label) {
    force(method); force(test); force(label)
    function(data) safe_call({
      mod <- metafor::rma(yi = data$yi, vi = data$vi, method = method, test = test,
                          control = list(stepadj = 0.5, maxiter = 2000))
      out <- new_universe_result(mod$b, mod$ci.lb, mod$ci.ub, mod$pval)
      attr(out, "method") <- label
      out
    }, method_name = label)
  }
  deps <- c("select_max", "select_min", "aggregate")
  register_ma_method("dl", make("DL", "z", "DL"), dependencies = deps)
  register_ma_method("dl_hksj", make("DL", "knha", "DL + HKSJ"), dependencies = deps)
  register_ma_method("reml_hksj", make("REML", "knha", "REML + HKSJ"), dependencies = deps)
  register_ma_method("pm_hksj", make("PM", "knha", "PM + HKSJ"), dependencies = deps)
  invisible(list_ma_methods())
}

#' Multiverse meta-analysis from arm-level pre/post data
#'
#' Runs the standard \code{define_factors()} ->
#' \code{create_multiverse_specifications()} -> \code{run_multiverse_analysis()}
#' pipeline once for every effect-size variant in \code{es_grid}, and stacks
#' the results. The effect-size metric and the handling of imputed post SDs
#' thereby become "how" factors alongside \code{ma_method} and
#' \code{dependency}.
#'
#' @param data Arm-level data as described in \code{\link{compute_pre_post_es}},
#'   plus any columns used as which factors.
#' @param which_factors Either a named list of factor definitions, passed as
#'   the \code{...} arguments of \code{\link{define_factors}}, or a function
#'   of the per-variant effect-size data (the output of
#'   \code{compute_pre_post_es()} for that variant) returning such a list.
#'   Use the function form when custom groups name levels that may be absent
#'   in some variants, because \code{define_factors()} validates group levels
#'   against the data it is given; see Examples.
#' @param es_grid Data frame with columns \code{es_metric} and
#'   \code{imputed_post_sd}, one row per effect-size variant (see
#'   \code{\link{compute_pre_post_es}} for the admissible values). Defaults
#'   to post-test SMD and SMC at r = 0.3, 0.5, 0.7, with imputed post SDs
#'   borrowed. Additional columns are ignored by the pipeline but are passed
#'   to \code{spec_filter}.
#' @param ma_methods,dependencies Passed to
#'   \code{\link{create_multiverse_specifications}}. Methods registered by
#'   \code{\link{register_metafor_estimators}} can be named here once that
#'   function has been called.
#' @param spec_filter Optional function \code{(specs, es_row, factor_info)}
#'   returning a filtered specification grid, applied before running each
#'   variant (e.g. to restrict modeled dependencies to the "all outcomes"
#'   level of an instrument factor). \code{specs} is the
#'   \code{specifications} data frame from
#'   \code{create_multiverse_specifications()}, \code{es_row} the current row
#'   of \code{es_grid}, and \code{factor_info} the \code{factors} table from
#'   \code{define_factors()}, which maps factor labels to \code{wf_*}
#'   columns.
#' @param k_smallest_ma Minimum number of unique studies per meta-analysis
#'   (sets the \code{metaMultiverse.k_smallest_ma} option for the duration of
#'   the call).
#' @param verbose Print one progress line per effect-size variant.
#'
#' @return A list of class \code{pre_post_multiverse} with:
#' \describe{
#'   \item{results}{Data frame with one row per unique meta-analysis across
#'     all variants: the columns of \code{run_multiverse_analysis()$results}
#'     with the \code{wf_*} columns renamed to the factor labels, plus
#'     \code{es_metric}, \code{imputed_post_sd}, \code{k_studies} (number of
#'     distinct studies in the set; \code{k} counts effect sizes) and
#'     \code{studies_in_set} (their names, \code{"; "}-separated).
#'     \code{NULL} if no specification produced a result.}
#'   \item{specifications}{The stacked specification grids, with the same
#'     renaming and the \code{es_metric} and \code{imputed_post_sd} columns.}
#'   \item{factors}{Character vector naming the factor columns of
#'     \code{results}: the which-factor labels, then \code{ma_method},
#'     \code{dependency}, \code{es_metric} and \code{imputed_post_sd}.
#'     This is what \code{\link{plot_spec_curve}} draws by default.}
#'   \item{n_attempted}{Number of specifications attempted across variants.}
#'   \item{warnings}{Character vector of warnings and failures collected from
#'     \code{run_multiverse_analysis()}.}
#' }
#'
#' @details
#' Each variant is computed with \code{compute_pre_post_es()}, given a fresh
#' \code{es_id} (row number), validated with \code{check_data_multiverse()}
#' and run through the standard pipeline. Because rows can be dropped by a
#' metric (e.g. \code{imputed_post_sd = "exclude"}), \code{es_id} values are
#' not comparable across variants; use \code{studies_in_set} to compare sets.
#'
#' Study-specific alternatives such as a different follow-up time point are
#' not a built-in factor: swap the rows in \code{data} and call the function
#' again, then stack the results.
#'
#' The return value can be passed straight to \code{\link{plot_spec_curve}}
#' and \code{\link{plot_voe}}. The specification curve draws the columns
#' named in \code{factors}, so the effect-size metric and the imputed-post-SD
#' rule appear as how factors next to \code{ma_method} and
#' \code{dependency}; pass \code{factors} to either plot function to choose
#' or relabel the rows (see their documentation). Results stacked from
#' several calls (e.g. different follow-up time points, with an extra column
#' identifying the call) can be plotted the same way by giving the stacked
#' data frame and \code{factors} to \code{plot_spec_curve()}.
#'
#' @examples
#' \donttest{
#' d <- data.frame(
#'   study = paste("Study", 1:8),
#'   n_1 = c(30, 40, 25, 50, 20, 35, 45, 28), n_2 = c(32, 38, 27, 48, 22, 33, 44, 30),
#'   pre_1_m = c(24, 22, 25, 23, 26, 21, 24, 22), pre_1_sd = 6,
#'   pre_2_m = c(23, 23, 24, 23, 25, 22, 24, 23), pre_2_sd = 6,
#'   post_1_m = c(14, 12, 15, 13, 16, 11, 14, 12), post_1_sd = 8,
#'   post_2_m = c(13, 12, 12, 13, 14, 12, 13, 11), post_2_sd = 8,
#'   rater = rep(c("clinician", "self"), 4)
#' )
#' register_metafor_estimators()
#' mv <- run_pre_post_multiverse(
#'   d, which_factors = list(rater = "rater|E"),
#'   ma_methods = c("reml", "dl_hksj"), dependencies = "aggregate",
#'   k_smallest_ma = 3, verbose = FALSE
#' )
#' head(mv$results[, c("es_metric", "rater", "ma_method", "b", "k_studies")])
#' mv$factors
#' plot_spec_curve(mv, interactive = FALSE)
#' plot_voe(mv, cutoff = 3, interactive = FALSE)
#'
#' # which_factors as a function: custom groups may only name levels that are
#' # present in the variant's data, so build them from that data
#' wf_fun <- function(dat) {
#'   present <- unique(dat$study)
#'   list(
#'     rater = "rater|E",
#'     inclusion = list("study", decision = "U", groups = list(
#'       all = present,
#'       no_study_1 = setdiff(present, "Study 1")
#'     ))
#'   )
#' }
#' mv2 <- run_pre_post_multiverse(
#'   d, which_factors = wf_fun,
#'   es_grid = data.frame(es_metric = c("post_smd", "change_smd_r0.5"),
#'                        imputed_post_sd = "borrow"),
#'   ma_methods = "dl", dependencies = "aggregate",
#'   k_smallest_ma = 3, verbose = FALSE
#' )
#' table(mv2$results$es_metric, mv2$results$inclusion)
#' }
#' @export
run_pre_post_multiverse <- function(data, which_factors,
                                    es_grid = NULL,
                                    ma_methods = c("reml", "pm"),
                                    dependencies = c("aggregate", "modeled"),
                                    spec_filter = NULL,
                                    k_smallest_ma = 5,
                                    verbose = TRUE) {
  if (is.null(es_grid)) {
    es_grid <- data.frame(
      es_metric = c("post_smd", "smc_r0.3", "smc_r0.5", "smc_r0.7"),
      imputed_post_sd = "borrow",
      stringsAsFactors = FALSE
    )
  }
  if (!is.data.frame(es_grid) || !all(c("es_metric", "imputed_post_sd") %in% names(es_grid))) {
    stop("run_pre_post_multiverse(): 'es_grid' must be a data frame with columns ",
         "'es_metric' and 'imputed_post_sd'")
  }
  es_grid <- as.data.frame(es_grid, stringsAsFactors = FALSE)
  es_grid$es_metric <- as.character(es_grid$es_metric)
  es_grid$imputed_post_sd <- as.character(es_grid$imputed_post_sd)
  bad_metric <- es_grid$es_metric[vapply(es_grid$es_metric, function(m) is.null(.pp_parse_metric(m)), logical(1))]
  if (length(bad_metric) > 0) .pp_stop_unknown_metric(bad_metric[1])
  bad_rule <- setdiff(es_grid$imputed_post_sd, .pp_imputed_rules)
  if (length(bad_rule) > 0) {
    stop("run_pre_post_multiverse(): unknown imputed_post_sd '", bad_rule[1], "'. Use ",
         paste(sQuote(.pp_imputed_rules, FALSE), collapse = ", "), ".")
  }
  if (!is.list(which_factors) && !is.function(which_factors)) {
    stop("run_pre_post_multiverse(): 'which_factors' must be a named list or a function")
  }
  missing_methods <- setdiff(ma_methods, list_ma_methods())
  if (length(missing_methods) > 0) {
    hint <- if (any(missing_methods %in% .pp_metafor_keys)) {
      " Call register_metafor_estimators() first."
    } else ""
    stop("run_pre_post_multiverse(): unknown ma_methods: ",
         paste(missing_methods, collapse = ", "), ".", hint)
  }

  old_opt <- options(metaMultiverse.k_smallest_ma = k_smallest_ma)
  on.exit(options(old_opt), add = TRUE)

  results <- list(); specs_all <- list(); warnings_all <- character(0); n_attempted <- 0L
  wf_labels <- character(0)

  for (i in seq_len(nrow(es_grid))) {
    g <- es_grid[i, , drop = FALSE]
    dat <- compute_pre_post_es(data, g$es_metric, g$imputed_post_sd)
    dat$es_id <- seq_len(nrow(dat))

    wf <- if (is.function(which_factors)) which_factors(dat) else which_factors
    setup <- suppressMessages(suppressWarnings(
      do.call(define_factors, c(list(check_data_multiverse(dat)), wf))
    ))
    factor_info <- setup$factors
    wf_labels <- union(wf_labels, as.character(factor_info$label))
    spec_out <- create_multiverse_specifications(setup, ma_methods = ma_methods,
                                                 dependencies = dependencies)
    if (!is.null(spec_filter)) {
      spec_out$specifications <- spec_filter(spec_out$specifications, g, factor_info)
      spec_out$specifications$row_id <- seq_len(nrow(spec_out$specifications))
    }
    n_attempted <- n_attempted + nrow(spec_out$specifications)
    if (nrow(spec_out$specifications) == 0) next

    res <- suppressMessages(suppressWarnings(
      run_multiverse_analysis(spec_out, verbose = FALSE, progress = FALSE)
    ))
    n_res <- if (is.null(res$results)) 0L else nrow(res$results)
    if (verbose) {
      cat(sprintf("  [%2d/%2d] %-18s %-12s specs %5d -> %5d\n", i, nrow(es_grid),
                  g$es_metric, g$imputed_post_sd, nrow(spec_out$specifications), n_res))
    }
    warnings_all <- c(warnings_all, res$multiverse_warnings)
    if (n_res == 0) next

    sets <- strsplit(as.character(res$results$set), ",")
    k_studies <- vapply(sets, function(ids) {
      length(unique(dat$study[dat$es_id %in% as.integer(ids)]))
    }, integer(1))
    studies_in_set <- vapply(sets, function(ids) {
      paste(sort(unique(as.character(dat$study[dat$es_id %in% as.integer(ids)]))), collapse = "; ")
    }, character(1))

    wf_map <- stats::setNames(factor_info$label, factor_info$wf_internal)
    relabel <- function(df) {
      df <- as.data.frame(df)
      hit <- names(df) %in% names(wf_map)
      names(df)[hit] <- wf_map[names(df)[hit]]
      df$es_metric <- g$es_metric
      df$imputed_post_sd <- g$imputed_post_sd
      df
    }
    r_i <- relabel(res$results)
    r_i$k_studies <- k_studies
    r_i$studies_in_set <- studies_in_set
    results[[length(results) + 1]] <- r_i
    specs_all[[length(specs_all) + 1]] <- relabel(spec_out$specifications)
  }

  structure(
    list(
      results = if (length(results)) do.call(rbind, results) else NULL,
      specifications = if (length(specs_all)) do.call(rbind, specs_all) else NULL,
      factors = c(wf_labels, "ma_method", "dependency", "es_metric", "imputed_post_sd"),
      n_attempted = n_attempted,
      warnings = warnings_all
    ),
    class = c("pre_post_multiverse", "list")
  )
}

#' @method print pre_post_multiverse
#' @export
print.pre_post_multiverse <- function(x, ...) {
  n_res <- if (is.null(x$results)) 0L else nrow(x$results)
  n_variants <- if (n_res > 0) {
    nrow(unique(x$results[, c("es_metric", "imputed_post_sd")]))
  } else 0L
  cat("Pre/post multiverse meta-analysis (pre_post_multiverse)\n")
  cat("  Effect-size variants with results:", n_variants, "\n")
  cat("  Unique meta-analyses:", n_res, "from", x$n_attempted, "specifications\n")
  cat("  Factors:", paste(x$factors, collapse = ", "), "\n")
  cat("  Warnings:", length(x$warnings), "\n")
  cat("Use plot_spec_curve(x) / plot_voe(x) to plot; x$results holds the table.\n")
  invisible(x)
}
