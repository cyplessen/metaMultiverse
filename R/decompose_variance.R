# ABOUTME: Hierarchical variance decomposition of specification-level
# ABOUTME: estimates with fork families as variance components. Supersedes
# ABOUTME: order-dependent sequential ANOVA; shares are analytic sensitivity,
# ABOUTME: not sampling-theory variance.

#' Decompose specification variance by fork
#'
#' Fits a crossed random-effects model over the specification-level
#' estimates, \code{b ~ 1 + (1 | fork_1) + ... + (1 | fork_K)}, treating
#' every varying fork as a variance component. N forks enter as labeled
#' between-estimand factors: their share is the "clinical share" of
#' divergence attributable to asking different clinical questions; the
#' within-question share is analytic. This layer compares estimands; it
#' never pools them into an effect estimate.
#'
#' Unlike sequential ANOVA sums of squares, variance components are not
#' order-dependent under unbalanced grids. Two caveats are attached to the
#' output and must accompany any table or figure built from it:
#' (1) all specifications share one dataset, so components describe analytic
#' sensitivity, not sampling-theory variance; (2) components estimated from
#' forks with few levels are noisy and often shrunk toward zero -- the
#' method-of-moments shares from option-level marginal means are reported
#' alongside as a stability check.
#'
#' @param result An \code{mv_grid_result}.
#' @param cell Optional single estimand cell id; default uses all inference
#'   specs with N forks as between-estimand components.
#' @return Data frame: term, family, layer ("between_estimand",
#'   "within_question", "residual"), variance, share, mom_share
#'   (method-of-moments companion), n_levels. Attributes: \code{caveat},
#'   \code{model} (the lmer fit or NULL if lme4 unavailable/failed).
#' @export
decompose_variance <- function(result, cell = NULL) {
  stopifnot(inherits(result, "mv_grid_result"))
  df <- result$results
  df <- df[df$layer == "inference", , drop = FALSE]
  if (!is.null(cell)) df <- df[df$cell_id == cell, , drop = FALSE]
  if (nrow(df) < 4) stop("Too few specifications to decompose.", call. = FALSE)

  dm <- attr(result$grid, "decision_map")
  fam <- attr(result$grid, "family_map")
  forks <- intersect(names(dm)[dm != "indefensible"], names(df))
  forks <- forks[vapply(forks, function(f) {
    length(unique(df[[f]][!is.na(df[[f]])])) > 1
  }, logical(1))]
  if (length(forks) == 0) {
    stop("No fork varies in the selected specifications.", call. = FALSE)
  }

  # method-of-moments companion: variance of option-level marginal means
  mom <- vapply(forks, function(f) {
    d <- df[!is.na(df[[f]]), ]
    means <- tapply(d$b, d[[f]], mean)
    if (length(means) < 2) 0 else stats::var(as.numeric(means))
  }, numeric(1))

  model <- NULL
  vc <- NULL
  if (requireNamespace("lme4", quietly = TRUE)) {
    fml <- stats::as.formula(paste(
      "b ~ 1 +", paste(sprintf("(1 | %s)", forks), collapse = " + ")
    ))
    model <- tryCatch(
      suppressWarnings(suppressMessages(
        lme4::lmer(fml, data = df,
                   control = lme4::lmerControl(check.conv.singular =
                                                 lme4::.makeCC("ignore",
                                                               tol = 1e-4)))
      )),
      error = function(e) NULL
    )
    if (!is.null(model)) {
      vc_df <- as.data.frame(lme4::VarCorr(model))
      vc <- stats::setNames(vc_df$vcov, vc_df$grp)
    }
  }

  if (is.null(vc)) {
    # lme4 unavailable or failed: fall back to method-of-moments components
    resid <- max(stats::var(df$b) - sum(mom), 0)
    vc <- c(mom, Residual = resid)
  }

  terms <- setdiff(names(vc), "Residual")
  total <- sum(vc)
  mom_total <- sum(mom) + max(stats::var(df$b) - sum(mom), 0)

  out <- data.frame(
    term = c(terms, "residual"),
    family = c(vapply(terms, function(t) {
      unname(fam[[t]]) %||% NA_character_
    }, character(1)), "residual"),
    layer = c(vapply(terms, function(t) {
      if (identical(unname(dm[[t]]), "changes_question")) "between_estimand"
      else "within_question"
    }, character(1)), "residual"),
    variance = c(unname(vc[terms]), unname(vc[["Residual"]])),
    share = c(unname(vc[terms]), unname(vc[["Residual"]])) / total,
    mom_share = c(unname(mom[terms]) / mom_total,
                  max(stats::var(df$b) - sum(mom), 0) / mom_total),
    n_levels = c(vapply(terms, function(t) {
      length(unique(df[[t]][!is.na(df[[t]])]))
    }, integer(1)), NA_integer_),
    stringsAsFactors = FALSE
  )
  rownames(out) <- NULL
  attr(out, "caveat") <- paste(
    "Variance components describe analytic sensitivity of one dataset to",
    "specification choices, not sampling-theory variance; components from",
    "forks with few levels are noisy and often shrunk toward zero",
    "(method-of-moments shares reported alongside). The between-estimand",
    "layer compares clinical questions; it never pools them."
  )
  attr(out, "model") <- model
  out
}

#' Family-level variance shares (clinical vs analytic vs between-estimand)
#'
#' @param decomposition Output of \code{\link{decompose_variance}}.
#' @return Data frame aggregating shares by layer and family.
#' @export
family_shares <- function(decomposition) {
  d <- decomposition[decomposition$term != "residual", , drop = FALSE]
  key <- paste(d$layer, d$family, sep = "/")
  out <- do.call(rbind, lapply(split(d, key), function(g) {
    data.frame(layer = g$layer[1], family = g$family[1],
               share = sum(g$share), mom_share = sum(g$mom_share),
               stringsAsFactors = FALSE)
  }))
  resid <- decomposition[decomposition$term == "residual", , drop = FALSE]
  out <- rbind(out, data.frame(layer = "residual", family = "residual",
                               share = resid$share,
                               mom_share = resid$mom_share,
                               stringsAsFactors = FALSE))
  rownames(out) <- NULL
  out
}

#' Model-based marginal fork means (safeguarded marginal view)
#'
#' Option-level marginal means for one fork, computed two ways: raw (means
#' over surviving specs -- distorted by differential specification survival)
#' and balanced (equal weight per combination of the other varying forks,
#' restricted to combinations observed under every level of the target fork,
#' i.e. the common surviving subgrid within the cell). Marginal comparisons
#' should quote the balanced version; the raw one is shown alongside.
#'
#' @param result An \code{mv_grid_result}.
#' @param fork Fork name.
#' @param cell Optional estimand cell restriction.
#' @return Data frame: level, n_specs, raw_mean, balanced_mean,
#'   n_common_combos.
#' @export
marginal_fork_means <- function(result, fork, cell = NULL) {
  df <- result$results
  df <- df[df$layer == "inference", , drop = FALSE]
  if (!is.null(cell)) df <- df[df$cell_id == cell, , drop = FALSE]
  if (!fork %in% names(df)) stop("Unknown fork: ", fork, call. = FALSE)
  df <- df[!is.na(df[[fork]]), , drop = FALSE]

  dm <- attr(result$grid, "decision_map")
  others <- setdiff(intersect(names(dm), names(df)), fork)
  others <- others[vapply(others, function(f) {
    length(unique(df[[f]][!is.na(df[[f]])])) > 1
  }, logical(1))]

  combo <- if (length(others)) {
    apply(df[, others, drop = FALSE], 1, paste, collapse = "|")
  } else {
    rep("all", nrow(df))
  }
  levels_seen <- unique(df[[fork]])
  combos_per_level <- tapply(combo, df[[fork]], unique)
  common <- Reduce(intersect, combos_per_level)

  out <- do.call(rbind, lapply(levels_seen, function(lv) {
    rows <- df[[fork]] == lv
    bal_rows <- rows & combo %in% common
    data.frame(
      level = lv,
      n_specs = sum(rows),
      raw_mean = mean(df$b[rows]),
      balanced_mean = if (any(bal_rows)) {
        mean(tapply(df$b[bal_rows], combo[bal_rows], mean))
      } else NA_real_,
      n_common_combos = length(common),
      stringsAsFactors = FALSE
    )
  }))
  rownames(out) <- NULL
  out
}
