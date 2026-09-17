# ABOUTME: Standardized fragility metrics M1-M6, computed WITHIN an estimand
# ABOUTME: cell (two-layer rule): fragility always means analytic fragility
# ABOUTME: within a fixed clinical question. M2 is the provisional primary.

cell_slice <- function(result, cell) {
  stopifnot(inherits(result, "mv_grid_result"))
  df <- result$results
  df <- df[df$layer == "inference" & df$cell_id == cell, , drop = FALSE]
  if (nrow(df) == 0) {
    stop("No converged inference specs in cell \"", cell, "\".", call. = FALSE)
  }
  df
}

varying_forks_in <- function(result, df) {
  dm <- attr(result$grid, "decision_map")
  candidates <- names(dm)[dm != "changes_question"]
  candidates <- intersect(candidates, names(df))
  candidates[vapply(candidates, function(f) {
    length(unique(df[[f]][!is.na(df[[f]])])) > 1
  }, logical(1))]
}

#' M1 -- Marginal fork impact in effect-size units
#'
#' SD of the option-level marginal means per fork, in Hedges' g units.
#' Comparable across domains under approximate additivity regardless of what
#' else is in the grid; computed only over forks that actually vary within
#' the cell, and only over specifications where the fork is observed.
#'
#' @param result An \code{mv_grid_result}.
#' @param cell Estimand cell id.
#' @return Data frame: fork, n_options, sigma_g, n_specs.
#' @export
m1_fork_impact <- function(result, cell) {
  df <- cell_slice(result, cell)
  forks <- varying_forks_in(result, df)
  out <- do.call(rbind, lapply(forks, function(f) {
    d <- df[!is.na(df[[f]]), ]
    means <- tapply(d$b, d[[f]], mean)
    data.frame(fork = f, n_options = length(means),
               sigma_g = if (length(means) < 2) NA_real_ else
                 stats::sd(means),
               n_specs = nrow(d), stringsAsFactors = FALSE)
  }))
  rownames(out) <- NULL
  out
}

#' M2 -- Specification-I2: fragility relative to sampling uncertainty
#'
#' Variance of the curve estimates relative to median squared standard error
#' across specifications (Higgins-Thompson logic at the specification level),
#' plus the more readable curve range in SE units. Near 0: analytic wobble
#' within sampling noise (answers the objection that variation may be a
#' power artifact); near 1: analytic choices move the estimate far beyond
#' sampling uncertainty. Provisional primary metric (decision D5).
#'
#' @param result An \code{mv_grid_result}.
#' @param cell Estimand cell id.
#' @return One-row data frame: v_spec, med_se2, spec_i2, range_in_se,
#'   n_specs.
#' @export
m2_spec_i2 <- function(result, cell) {
  df <- cell_slice(result, cell)
  se <- df$se
  se[is.na(se)] <- (df$ci.ub[is.na(se)] - df$ci.lb[is.na(se)]) / 3.92
  v_spec <- stats::var(df$b)
  med_se2 <- stats::median(se^2, na.rm = TRUE)
  data.frame(
    v_spec = v_spec,
    med_se2 = med_se2,
    spec_i2 = v_spec / (v_spec + med_se2),
    range_in_se = (stats::quantile(df$b, .99, names = FALSE) -
                     stats::quantile(df$b, .01, names = FALSE)) /
      sqrt(med_se2),
    n_specs = nrow(df),
    stringsAsFactors = FALSE
  )
}

#' M3 -- Clinically anchored consistency (MID exceedance)
#'
#' Share of specifications whose estimate exceeds a minimally important
#' difference. Grid-size invariant in units and reframes fragility as
#' whether the conclusion that treatment is worthwhile survives.
#' Reported as a labeled descriptive frequency, never as inference.
#'
#' @param result An \code{mv_grid_result}.
#' @param cell Estimand cell id.
#' @param mid Minimally important difference on the effect-size scale. The
#'   default of 0.24 is the threshold for a clinically relevant effect in
#'   depression psychotherapy (Cuijpers et al., 2014,
#'   \doi{10.1002/da.22249}) used in the Metapsy multiverse analyses; it is
#'   domain-specific, so set it for your outcome.
#' @return One-row data frame: mid, prop_above_mid, n_specs.
#' @export
m3_mid_consistency <- function(result, cell, mid = 0.24) {
  df <- cell_slice(result, cell)
  data.frame(mid = mid, prop_above_mid = mean(df$b > mid),
             n_specs = nrow(df), stringsAsFactors = FALSE)
}

#' M4 -- Fork-balanced aggregation
#'
#' Aggregates a statistic hierarchically (within option, across options,
#' across forks) so every decision node gets equal voice regardless of its
#' option count; the difference to the flat statistic flags option-count
#' artifacts.
#'
#' @param result An \code{mv_grid_result}.
#' @param cell Estimand cell id.
#' @param stat Function of the b vector returning a scalar (default:
#'   MID exceedance at 0.24).
#' @return One-row data frame: flat, fork_balanced, delta.
#' @export
m4_fork_balanced <- function(result, cell,
                             stat = function(b) mean(b > 0.24)) {
  df <- cell_slice(result, cell)
  forks <- varying_forks_in(result, df)
  flat <- stat(df$b)
  per_fork <- vapply(forks, function(f) {
    d <- df[!is.na(df[[f]]), ]
    mean(tapply(d$b, d[[f]], stat)) # equal weight per option
  }, numeric(1))
  balanced <- mean(per_fork) # equal weight per fork
  data.frame(flat = flat, fork_balanced = balanced,
             delta = balanced - flat, stringsAsFactors = FALSE)
}

#' M5 -- Leave-one-fork-out influence analysis of the grid itself
#'
#' Recomputes the specification-I2 with each varying fork held at its
#' reference level; large drops localize the fragility to that decision
#' node. Rankings stable across all leave-one-out grids are trustworthy.
#'
#' @param result An \code{mv_grid_result}.
#' @param cell Estimand cell id.
#' @return Data frame: fixed_fork ("none" first), spec_i2, n_specs.
#' @export
m5_lofo <- function(result, cell) {
  df <- cell_slice(result, cell)
  forks <- varying_forks_in(result, df)
  config <- attr(result$grid, "config")
  base <- m2_spec_i2(result, cell)
  rows <- list(data.frame(fixed_fork = "none", spec_i2 = base$spec_i2,
                          n_specs = base$n_specs, stringsAsFactors = FALSE))
  for (f in forks) {
    ref <- config$forks[[f]]$reference
    d <- df[!is.na(df[[f]]) & df[[f]] == ref, ]
    if (nrow(d) < 3) {
      rows[[length(rows) + 1]] <- data.frame(fixed_fork = f,
                                             spec_i2 = NA_real_,
                                             n_specs = nrow(d),
                                             stringsAsFactors = FALSE)
      next
    }
    se <- d$se
    se[is.na(se)] <- (d$ci.ub[is.na(se)] - d$ci.lb[is.na(se)]) / 3.92
    v <- stats::var(d$b)
    m <- stats::median(se^2, na.rm = TRUE)
    rows[[length(rows) + 1]] <- data.frame(fixed_fork = f,
                                           spec_i2 = v / (v + m),
                                           n_specs = nrow(d),
                                           stringsAsFactors = FALSE)
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' M6 -- Common surviving subgrid comparison
#'
#' Specification-I2 per domain on the full designated cell versus on the
#' cross-domain common core (specifications converged in every domain).
#' The gap quantifies how much differential specification survival distorts
#' the cross-domain ranking.
#'
#' @param results Named list of \code{mv_grid_result}.
#' @param cells Named character vector domain -> designated cell id.
#' @return Data frame: domain, spec_i2_full, spec_i2_common, n_full,
#'   n_common. Attribute \code{common_core}: the
#'   \code{\link{common_core_specs}} output.
#' @export
m6_common_subgrid <- function(results, cells) {
  core <- common_core_specs(results, cells)
  out <- do.call(rbind, lapply(names(results), function(dom) {
    full <- m2_spec_i2(results[[dom]], cells[[dom]])
    sub <- core$subsets[[dom]]
    if (nrow(sub) >= 3) {
      se <- sub$se
      se[is.na(se)] <- (sub$ci.ub[is.na(se)] - sub$ci.lb[is.na(se)]) / 3.92
      v <- stats::var(sub$b)
      m <- stats::median(se^2, na.rm = TRUE)
      i2c <- v / (v + m)
    } else {
      i2c <- NA_real_
    }
    data.frame(domain = dom, spec_i2_full = full$spec_i2,
               spec_i2_common = i2c, n_full = full$n_specs,
               n_common = nrow(sub), stringsAsFactors = FALSE)
  }))
  rownames(out) <- NULL
  attr(out, "common_core") <- core
  out
}

#' All fragility metrics for one domain cell
#'
#' Convenience wrapper computing M1-M5 on a designated estimand cell.
#' M2 (specification-I2) is the provisional primary metric (decision D5);
#' M1 is the effect-scale companion; M3-M5 are sensitivity/diagnostic
#' layers. M6 is cross-domain: see \code{\link{m6_common_subgrid}}.
#'
#' @param result An \code{mv_grid_result}.
#' @param cell Estimand cell id.
#' @param mid Minimally important difference for M3/M4 (default 0.24, a
#'   depression-psychotherapy convention; see \code{\link{m3_mid_consistency}}).
#' @return List with elements m1, m2, m3, m4, m5.
#' @export
fragility_metrics <- function(result, cell, mid = 0.24) {
  list(
    m1 = m1_fork_impact(result, cell),
    m2 = m2_spec_i2(result, cell),
    m3 = m3_mid_consistency(result, cell, mid = mid),
    m4 = m4_fork_balanced(result, cell,
                          stat = function(b) mean(b > mid)),
    m5 = m5_lofo(result, cell)
  )
}

#' Equivalence-margin check
#'
#' Flags equivalent-ruled forks whose marginal impact (M1 sigma per fork)
#' exceeds their declared equivalence_margin: an "equivalent" decision that
#' moves estimates more than expected is auto-escalated for
#' reclassification scrutiny (is it really equivalent -- or uncertain,
#' question-changing, or indefensible?). The same escalation fires for any
#' fork dominating within-cell variance.
#'
#' @param result An \code{mv_grid_result}.
#' @param cell Estimand cell id.
#' @param dominance_share Share of summed sigma_g above which any fork is
#'   escalated regardless of decision type. The default of 0.5 is a working
#'   convention of the Metapsy multiverse analyses with no external
#'   source; treat it as a tuning choice.
#' @return Data frame: fork, decision, sigma_g, equivalence_margin,
#'   exceeds_margin, dominates, escalate.
#' @export
check_fork_tolerances <- function(result, cell, dominance_share = 0.5) {
  m1 <- m1_fork_impact(result, cell)
  config <- attr(result$grid, "config")
  dm <- attr(result$grid, "decision_map")
  total <- sum(m1$sigma_g, na.rm = TRUE)
  out <- do.call(rbind, lapply(seq_len(nrow(m1)), function(i) {
    f <- m1$fork[i]
    margin <- config$forks[[f]]$equivalence_margin
    decision <- unname(dm[[f]])
    exceeds <- decision == "equivalent" && !is.null(margin) &&
      !is.na(m1$sigma_g[i]) && m1$sigma_g[i] > margin
    dominates <- total > 0 && !is.na(m1$sigma_g[i]) &&
      m1$sigma_g[i] / total > dominance_share
    data.frame(fork = f, decision = decision, sigma_g = m1$sigma_g[i],
               equivalence_margin = margin %||% NA_real_,
               exceeds_margin = exceeds, dominates = dominates,
               escalate = exceeds || dominates, stringsAsFactors = FALSE)
  }))
  rownames(out) <- NULL
  out
}
