# ABOUTME: Effect-size plausibility flagging (Harrer et al. 2025) as a
# ABOUTME: first-class package feature: three risk indicators, composite
# ABOUTME: flag profiles, and the plausibility handling-strategy fork family.

# Reference values fitted by Harrer et al. (2025) to LOW risk-of-bias effect
# sizes per database (Gamma distributions; MLE, BIC + Anderson-Darling
# selection). mu_fe = UWLS reference effect delta of low-RoB evidence;
# q1_pwr = first empirical quartile of achieved power among low-RoB trials.
# The gad row was present in the original flagStudy() lookup but rejected by
# its argument validation (GAD silently fell back to "all"); here it is a
# legitimate explicit choice.
.flag_reference_table <- data.frame(
  reference = c("all", "dep", "gad", "psy", "ptsd"),
  distname = "gamma",
  shape = c(1.22780106290059, 1.49556995998092, 2.08900202717736,
            1.2491159032556, 2.69699379305318),
  rate = c(1.82800375781207, 2.343560247288, 2.78725113029231,
           2.78807085068211, 2.02655752359899),
  mu_fe = c(0.455964509009351, 0.484125449169705, 0.678273877252083,
            0.30882193290888, 1.06951362680109),
  q1_pwr = c(0.3240959446545, 0.410179676205664, 0.57613016526012,
             0.218204329492164, 0.748225230452593),
  stringsAsFactors = FALSE
)

# critical value of the reference distribution at upper-tail p
get_critical_value <- function(distname, params, p = 0.95) {
  qfun <- match.fun(paste0("q", distname))
  do.call(qfun, c(list(p = p), as.list(params)))
}

#' Flag implausible effect sizes (Harrer et al. 2025)
#'
#' Computes the three binary risk indicators of the Harrer et al. (2025)
#' flagging tool: (1) extreme effect size relative to a reference
#' distribution fitted to low risk-of-bias evidence, cut at Shannon surprisal
#' S = -log2(p) with p = \code{pval} (default .05, i.e. 4.32 bits);
#' (2) severe lack of statistical power to detect the low-RoB reference
#' effect (below the first empirical quartile of achieved power by default);
#' (3) not judged low risk of bias. The tool's operative flag is the
#' CONJUNCTION of all three (score 3): an effect is never flagged on
#' magnitude alone.
#'
#' Calibration scope: the reference values are estimated for psychological
#' intervention trials against inactive or weak control conditions.
#' Do not apply them to head-to-head comparisons; condition the plausibility
#' fork on comparator type in the domain config.
#'
#' @param smd Numeric vector of standardized mean differences (Hedges' g).
#' @param se Numeric vector of standard errors.
#' @param high_rob Logical vector: TRUE if the study is NOT judged low risk
#'   of bias. Missing values are treated as HIGH risk with a message
#'   (project convention, user decision D2 2026-07-20: missing RoB ratings
#'   count as high risk).
#' @param reference Reference distribution: one of \code{"all"},
#'   \code{"dep"}, \code{"gad"}, \code{"psy"}, \code{"ptsd"}, or a list with
#'   elements \code{distname}, \code{params} (named list for the q-function),
#'   \code{mu_fe}, \code{q1_pwr}. There is NO default: the choice is an
#'   explicit per-domain decision recorded in the config.
#' @param pval Upper-tail probability defining the extreme-effect cutoff
#'   (default 0.05, equivalent to an S-value of 4.32 bits).
#' @param power \code{"q1"} (default: the reference's first empirical power
#'   quartile) or a numeric power in (0, 1).
#' @param verbose Print threshold messages.
#' @return Object of class \code{"mv_flags"}: list with per-effect logicals
#'   \code{flag_effect}, \code{flag_power}, \code{flag_rob}, integer
#'   \code{score} (0-3), \code{flag_profile} (3-character binary string:
#'   effect, power, rob), \code{flagged} (score == 3, the paper's operative
#'   definition), and \code{thresholds}.
#' @references Harrer, M., et al. (2025). Implausible effects of
#'   psychological interventions. medRxiv, doi 10.1101/2025.11.12.25340062.
#' @export
flag_effect_sizes <- function(smd, se, high_rob, reference,
                              pval = 0.05, power = "q1", verbose = FALSE) {
  if (missing(reference) || is.null(reference)) {
    stop("`reference` must be chosen explicitly (all, dep, gad, psy, ptsd, ",
         "or a custom list) -- the reference distribution is a per-domain ",
         "decision recorded in the config; there is no silent default.",
         call. = FALSE)
  }
  if (is.character(reference)) {
    if (!reference %in% .flag_reference_table$reference) {
      stop("Unknown flag reference \"", reference, "\". Available: ",
           paste(.flag_reference_table$reference, collapse = ", "),
           call. = FALSE)
    }
    row <- .flag_reference_table[.flag_reference_table$reference == reference, ]
    ref <- list(distname = row$distname,
                params = list(shape = row$shape, rate = row$rate),
                mu_fe = row$mu_fe, q1_pwr = row$q1_pwr,
                label = reference)
  } else {
    stopifnot(is.list(reference),
              all(c("distname", "params", "mu_fe", "q1_pwr") %in%
                    names(reference)))
    ref <- reference
    ref$label <- ref$label %||% "custom"
  }

  if (!is.numeric(pval) || pval <= 0 || pval >= 1) {
    stop("`pval` must be in (0, 1).", call. = FALSE)
  }
  if (!(identical(power, "q1") ||
        (is.numeric(power) && power > 0 && power < 1))) {
    stop("`power` must be \"q1\" or a numeric in (0, 1).", call. = FALSE)
  }
  n <- length(smd)
  if (length(se) != n || length(high_rob) != n) {
    stop("`smd`, `se`, and `high_rob` must have equal length.", call. = FALSE)
  }
  if (!is.logical(high_rob)) {
    stop("`high_rob` must be a logical vector (TRUE = not low risk of bias).",
         call. = FALSE)
  }

  if (anyNA(high_rob)) {
    message(sum(is.na(high_rob)), " missing RoB rating(s) treated as HIGH ",
            "risk (project convention D2: unrated = not judged low risk).")
    high_rob[is.na(high_rob)] <- TRUE
  }

  # Indicator 1: extreme effect (upper-tail S-value cutoff)
  smd_crit <- get_critical_value(ref$distname, ref$params, p = 1 - pval)
  flag_effect <- abs(smd) > smd_crit

  # Indicator 2: severe underpower against the low-RoB reference effect
  pwr <- if (identical(power, "q1")) ref$q1_pwr else power
  se_crit <- ref$mu_fe / (stats::qnorm(1 - pval / 2) + stats::qnorm(pwr))
  flag_power <- se > se_crit

  # Indicator 3: not judged low risk of bias
  flag_rob <- high_rob

  score <- as.integer(flag_effect) + as.integer(flag_power) +
    as.integer(flag_rob)
  profile <- paste0(as.integer(flag_effect), as.integer(flag_power),
                    as.integer(flag_rob))

  if (verbose) {
    message("Flag reference \"", ref$label, "\": |g| > ",
            round(smd_crit, 2), " extreme; achieved power < ",
            round(pwr * 100, 1), "% (SE > ", round(se_crit, 3),
            ") severely underpowered; S-value = ",
            round(-log2(pval), 2), " bits.")
  }

  structure(
    list(
      flag_effect = flag_effect,
      flag_power = flag_power,
      flag_rob = flag_rob,
      score = score,
      flag_profile = profile,
      flagged = score == 3L,
      thresholds = list(reference = ref$label, smd_crit = smd_crit,
                        power = pwr, se_crit = se_crit, pval = pval,
                        s_value_bits = -log2(pval))
    ),
    class = c("mv_flags", "list")
  )
}

#' @export
print.mv_flags <- function(x, ...) {
  n <- length(x$score)
  cat("<effect-size plausibility flags> reference: ",
      x$thresholds$reference, "\n", sep = "")
  cat(sprintf("  |g| cutoff %.2f | power cutoff %.1f%% (SE %.3f)\n",
              x$thresholds$smd_crit, 100 * x$thresholds$power,
              x$thresholds$se_crit))
  cat(sprintf("  1+ flags: %d/%d | 2+ flags: %d/%d | all 3 (operative): %d/%d\n",
              sum(x$score >= 1), n, sum(x$score >= 2), n,
              sum(x$flagged), n))
  invisible(x)
}

# The hardcoded menu of defensible handling strategies. Each maps a level
# label to the flag profiles it RETAINS ("xyz" = effect, power, rob digits).
# Data-dependence is confined to which profiles occur; the menu itself is
# fixed (Part 1b design rule).
.plausibility_menu <- function() {
  profiles <- c("000", "001", "010", "011", "100", "101", "110", "111")
  score <- vapply(strsplit(profiles, ""), function(d) {
    sum(as.integer(d))
  }, integer(1))
  list(
    keep_all = profiles,
    drop_triple_flagged = profiles[score < 3],          # Harrer default (=3)
    drop_two_plus_flagged = profiles[score < 2],
    drop_any_flagged = profiles[score < 1],
    drop_extreme_effects = profiles[substr(profiles, 1, 1) == "0"],
    drop_underpowered = profiles[substr(profiles, 2, 2) == "0"],
    drop_high_rob = profiles[substr(profiles, 3, 3) == "0"]
  )
}

#' Plausibility handling-strategy fork
#'
#' Builds the U-type analytic fork encoding the defensible handling
#' strategies for flagged effect sizes as levels over the composite
#' \code{flag_profile} column (created by \code{\link{flag_effect_sizes}}).
#' The seven-level menu is hardcoded; which levels are non-empty in a given
#' domain is data-dependent and resolved at validation time, where empty or
#' subset-identical levels are recorded in the availability matrix (never
#' silently dropped).
#'
#' Levels and their warrants (legacy names in parentheses are accepted
#' as aliases):
#' \describe{
#'   \item{keep_all (all_studies)}{Include everything (ignore flags).
#'     Reference level; the conventional naive analysis.}
#'   \item{drop_triple_flagged (exclude_all_flagged)}{Exclude score-3
#'     effects -- the Harrer et al. (2025) prespecifiable default: never
#'     exclude on magnitude alone.}
#'   \item{drop_two_plus_flagged (exclude_2plus_flags)}{Looser sensitivity
#'     tier (removes more evidence; Harrer et al. treat as exploratory).}
#'   \item{drop_any_flagged (exclude_1plus_flags)}{Loosest tier; documented
#'     as aggressive.}
#'   \item{drop_extreme_effects (exclude_effect_flag)}{Exclude by
#'     extreme-effect indicator alone (single-indicator sensitivity).}
#'   \item{drop_underpowered (exclude_power_flag)}{Exclude severely
#'     underpowered trials (single-indicator sensitivity).}
#'   \item{drop_high_rob (exclude_rob_flag)}{Exclude not-low-RoB trials
#'     (single-indicator sensitivity; kept a SEPARATE family from any
#'     standalone RoB fork -- see Details).}
#' }
#'
#' Plausibility flags and risk-of-bias flags are related but distinct
#' quality signals: if a domain config also carries a standalone RoB
#' exclusion fork, both are analytic-family forks but keep separate names so
#' the decomposition can attribute variance to each. None of these levels
#' changes the clinical estimand (they alter the evidence base, not the
#' question), which is why the fork is U, not N; ad hoc post-hoc exclusions
#' to reach a desired result are X-layer material and are not part of this
#' menu.
#'
#' @param reference_level Level used as reference (default
#'   \code{"keep_all"}).
#' @param include_levels Optional character vector restricting the menu
#'   (e.g. for domains where an indicator is unavailable); must be a subset
#'   of the seven menu levels.
#' @return An adaptive U-type \code{\link{fork}} over \code{flag_profile}.
#' @export
plausibility_fork <- function(reference_level = "keep_all",
                              include_levels = NULL) {
  menu <- .plausibility_menu()
  reference_level <- normalize_plausibility_level(reference_level,
                                                  "plausibility_fork")
  if (!is.null(include_levels)) {
    include_levels <- normalize_plausibility_level(include_levels,
                                                   "plausibility_fork")
    if (!reference_level %in% include_levels) {
      include_levels <- c(reference_level, include_levels)
    }
    menu <- menu[include_levels]
  }
  fork(
    name = "plausibility",
    type = "data",
    decision = "U",
    family = "analytic",
    column = "flag_profile",
    adaptive = function(data) {
      observed <- unique(data$flag_profile)
      lapply(menu, function(keep) intersect(keep, observed))
    },
    reference = reference_level,
    justification = list(
      basis = "citation",
      text = paste("Several defensible handling strategies exist for",
                   "implausible effects; strategy varied within the",
                   "multiverse and tracked as metadata (U). Menu hardcoded;",
                   "level availability is data-dependent by design."),
      citation = "Harrer et al. 2025, doi 10.1101/2025.11.12.25340062"
    )
  )
}

#' Attach plausibility flags to a dataset
#'
#' Convenience wrapper: runs \code{\link{flag_effect_sizes}} and binds
#' \code{flag_effect}, \code{flag_power}, \code{flag_rob}, \code{flag_score},
#' \code{flag_profile}, \code{flagged} onto the data by position (no
#' bind_cols alignment risk: inputs are taken from the same data frame).
#'
#' @param data Data frame.
#' @param smd_col,se_col Column names for the standardized mean difference
#'   and its standard error.
#' @param high_rob Logical vector (TRUE = not low RoB) or the name of a
#'   logical column in \code{data}.
#' @param ... Passed to \code{\link{flag_effect_sizes}} (\code{reference} is
#'   required).
#' @return \code{data} with the six flag columns appended and the thresholds
#'   stored in \code{attr(, "flag_thresholds")}.
#' @export
add_plausibility_flags <- function(data, smd_col = "yi", se_col = NULL,
                                   high_rob, ...) {
  stopifnot(is.data.frame(data))
  smd <- data[[smd_col]]
  if (is.null(smd)) stop("Column `", smd_col, "` not found.", call. = FALSE)
  se <- if (!is.null(se_col)) {
    data[[se_col]]
  } else if ("vi" %in% names(data)) {
    sqrt(data$vi)
  } else {
    stop("Supply `se_col` or a `vi` column.", call. = FALSE)
  }
  if (is.character(high_rob) && length(high_rob) == 1) {
    high_rob <- data[[high_rob]]
    if (is.null(high_rob)) stop("high_rob column not found.", call. = FALSE)
  }
  fl <- flag_effect_sizes(smd = smd, se = se, high_rob = high_rob, ...)
  data$flag_effect <- fl$flag_effect
  data$flag_power <- fl$flag_power
  data$flag_rob <- fl$flag_rob
  data$flag_score <- fl$score
  data$flag_profile <- fl$flag_profile
  data$flagged <- fl$flagged
  attr(data, "flag_thresholds") <- fl$thresholds
  data
}
