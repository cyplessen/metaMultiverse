# ABOUTME: run_multiverse_grid() -- executes an mv_spec_grid with full
# ABOUTME: traceability: attrition ledger (structural vs estimation loss),
# ABOUTME: per-universe diagnostics, worst-universe audit, estimand guard.

#' Run every specification of a grid
#'
#' Executes the inference and bias layers of an \code{mv_spec_grid}. Every
#' loss is recorded in the attrition ledger with a machine-readable stage and
#' reason -- nothing is silently dropped. Results carry \code{spec_id} for
#' full traceability via \code{\link{lookup_spec}}.
#'
#' @param grid An \code{mv_spec_grid} from \code{\link{build_specification_grid}}.
#' @param within_study_correlation Assumed correlation between effect sizes
#'   from the same study, used by the aggregate dependency strategy
#'   (default 0.6, the Metapsy convention).
#' @param progress Show a progress bar.
#' @param rho Legacy alias for \code{within_study_correlation}.
#' @return An object of class \code{"mv_grid_result"}: list with
#'   \code{results} (data frame: spec columns + b, ci.lb, ci.ub, pval, se,
#'   tau2, i2, k, converged, method_label), \code{attrition} (ledger),
#'   \code{grid}, and \code{data}.
#' @export
run_multiverse_grid <- function(grid, within_study_correlation = 0.6,
                                progress = TRUE, rho = NULL) {
  if (!is.null(rho)) within_study_correlation <- rho
  stopifnot(inherits(grid, "mv_spec_grid"))
  config <- attr(grid, "config")
  validation <- attr(grid, "validation")
  data <- validation$data
  forks <- config$forks
  levels <- validation$levels

  ledger <- list()
  note <- function(spec_id, stage, reason) {
    ledger[[length(ledger) + 1]] <<- data.frame(
      spec_id = spec_id, stage = stage, reason = reason,
      stringsAsFactors = FALSE
    )
  }

  # structural absences recorded at grid build time enter the ledger up front
  av <- validation$availability
  for (i in seq_len(nrow(av))) {
    if (!isTRUE(av$viable[i]) && !is.na(av$n_es[i])) {
      note(paste0(config$domain, "::level::", av$fork[i], "=", av$level[i]),
           "structural", av$status[i])
    }
  }

  which_forks <- names(forks)[vapply(forks, function(f) {
    f$type == "data" && f$decision != "indefensible"
  }, logical(1))]
  which_x <- names(forks)[vapply(forks, function(f) {
    f$type == "data" && f$decision == "indefensible"
  }, logical(1))]

  n <- nrow(grid)
  if (progress) pb <- utils::txtProgressBar(min = 0, max = n, style = 3)
  rows <- vector("list", n)

  for (i in seq_len(n)) {
    spec <- grid[i, ]
    dat <- data

    # subset by every which-fork level active in this spec (bias-layer rows
    # include X which-forks as well)
    active_which <- c(which_forks, if (spec$layer == "bias") which_x)
    subset_failed <- FALSE
    for (wf in active_which) {
      lv <- spec[[wf]]
      if (is.null(lv) || is.na(lv)) next
      vals <- levels[[wf]][[lv]]
      if (is.null(vals)) {
        note(spec$spec_id, "structural",
             paste0("level ", lv, " of fork ", wf, " not materialized"))
        subset_failed <- TRUE
        break
      }
      dat <- dat[dat[[forks[[wf]]$column]] %in% vals, , drop = FALSE]
    }
    if (subset_failed) {
      if (progress) utils::setTxtProgressBar(pb, i)
      next
    }

    k_studies <- length(unique(dat$study))
    if (k_studies < config$min_studies) {
      note(spec$spec_id, "too_few_studies",
           paste0(k_studies, " studies < min_studies = ",
                  config$min_studies))
      if (progress) utils::setTxtProgressBar(pb, i)
      next
    }

    # engine parameters: ma_method/dependency columns, then overrides from
    # how forks that map levels to engine settings
    ma_method <- spec$ma_method
    dependency <- spec$dependency
    for (f in forks) {
      if (f$type == "analysis" && !is.null(f$overrides)) {
        lv <- spec[[f$name]]
        if (!is.null(lv) && !is.na(lv) && lv %in% names(f$overrides)) {
          ov <- f$overrides[[lv]]
          if (!is.null(ov$ma_method)) ma_method <- ov$ma_method
          if (!is.null(ov$dependency)) dependency <- ov$dependency
        }
      }
    }
    if (is.null(ma_method) || is.na(ma_method) ||
        is.null(dependency) || is.na(dependency)) {
      note(spec$spec_id, "structural", "no ma_method/dependency resolved")
      if (progress) utils::setTxtProgressBar(pb, i)
      next
    }

    fit <- tryCatch({
      if (dependency %in% c("select_max", "select_min")) {
        run_select_dependency(dat, ma_method, dependency)
      } else if (dependency == "aggregate") {
        run_aggregate_dependency(dat, ma_method,
                                 rho = within_study_correlation)
      } else if (dependency == "modeled") {
        run_modeled_dependency(dat, ma_method)
      } else {
        stop("unknown dependency: ", dependency)
      }
    }, error = function(e) {
      note(spec$spec_id, "estimation_error", conditionMessage(e))
      NULL
    })

    if (is.null(fit)) {
      if (progress) utils::setTxtProgressBar(pb, i)
      next
    }
    if (!is_valid_universe(fit)) {
      note(spec$spec_id, "invalid_result",
           paste0(attr(fit, "method", exact = TRUE) %||% ma_method,
                  ": no finite estimate with finite confidence limits ",
                  "(optimizer failure)"))
      if (progress) utils::setTxtProgressBar(pb, i)
      next
    }

    rows[[i]] <- cbind(
      spec,
      data.frame(
        b = fit$b, ci.lb = fit$ci.lb, ci.ub = fit$ci.ub, pval = fit$pval,
        se = fit$se, tau2 = fit$tau2, i2 = fit$i2,
        k = if (is.na(fit$k)) k_studies else fit$k,
        n_es = nrow(dat),
        converged = fit$convergence,
        method_label = attr(fit, "method", exact = TRUE) %||% ma_method,
        stringsAsFactors = FALSE
      )
    )
    if (progress) utils::setTxtProgressBar(pb, i)
  }
  if (progress) close(pb)

  results <- do.call(rbind, rows[!vapply(rows, is.null, logical(1))])
  ledger <- if (length(ledger)) do.call(rbind, ledger) else
    data.frame(spec_id = character(), stage = character(),
               reason = character(), stringsAsFactors = FALSE)

  structure(
    list(
      results = results,
      attrition = ledger,
      grid = grid,
      data = data
    ),
    class = "mv_grid_result"
  )
}

#' Is a universe result a valid estimate?
#'
#' A fit counts as estimated only if it produced a finite point estimate AND
#' finite confidence limits. Optimizers that fail without erroring (e.g.
#' p-uniform* non-convergence returning an absurd estimate with NA limits)
#' are thereby routed to the attrition ledger as estimation failures.
#' The criterion is value-agnostic: it tests estimability, not magnitude.
#'
#' @param fit A \code{universe_result}.
#' @return Logical.
#' @keywords internal
is_valid_universe <- function(fit) {
  is.finite(fit$b) && is.finite(fit$ci.lb) && is.finite(fit$ci.ub)
}

#' @export
print.mv_grid_result <- function(x, ...) {
  at <- attrition_summary(x)
  cat("<multiverse grid result> ", attr(x$grid, "config")$domain, "\n", sep = "")
  print(at, row.names = FALSE)
  invisible(x)
}

#' Planned vs executed vs converged specification counts
#'
#' The specification attrition report: how many specifications were planned,
#' defined after viability/compatibility, executed, and converged -- plus loss
#' reasons from the ledger.
#'
#' @param result An \code{mv_grid_result}.
#' @return Data frame with stage counts.
#' @export
attrition_summary <- function(result) {
  stopifnot(inherits(result, "mv_grid_result"))
  at <- attr(result$grid, "attrition")
  inference_ids <- result$grid$spec_id[result$grid$layer == "inference"]
  led <- result$attrition
  lost_k <- sum(led$stage == "too_few_studies" &
                  led$spec_id %in% inference_ids)
  lost_est <- sum(led$stage %in% c("estimation_error", "invalid_result") &
                    led$spec_id %in% inference_ids)
  done <- if (is.null(result$results)) 0 else
    sum(result$results$layer == "inference")
  data.frame(
    stage = c("planned", "defined", "lost: too few studies",
              "lost: estimation", "converged"),
    n = c(at$planned, at$defined, lost_k, lost_est, done),
    stringsAsFactors = FALSE
  )
}

#' Trace a specification id back to its configuration and result
#'
#' Implements the traceability requirement: every estimate carries a
#' \code{spec_id}; this returns spec definition, fork warrants, estimand
#' declaration, result row, diagnostics, and any attrition entries.
#'
#' @param result An \code{mv_grid_result}.
#' @param spec_id A specification id.
#' @return A list with elements \code{spec}, \code{estimand}, \code{forks}
#'   (decision/family/justification per fork), \code{result}, \code{audit},
#'   \code{attrition}.
#' @export
lookup_spec <- function(result, spec_id) {
  stopifnot(inherits(result, "mv_grid_result"))
  grid <- result$grid
  hit <- grid[grid$spec_id == spec_id, , drop = FALSE]
  if (nrow(hit) == 0) {
    stop("spec_id not found in grid: ", spec_id, call. = FALSE)
  }
  config <- attr(grid, "config")
  res_row <- if (!is.null(result$results)) {
    result$results[result$results$spec_id == spec_id, , drop = FALSE]
  } else NULL
  audit_row <- audit_universes(result)
  audit_row <- audit_row[audit_row$spec_id == spec_id, , drop = FALSE]
  list(
    spec = hit,
    estimand = attr(grid, "estimand_cells")[[hit$cell_id]],
    forks = lapply(config$forks, function(f) {
      list(decision = f$decision, family = f$family,
           justification = f$justification)
    }),
    result = res_row,
    audit = audit_row,
    attrition = result$attrition[result$attrition$spec_id == spec_id, ,
                                 drop = FALSE]
  )
}

#' Worst-universe audit
#'
#' Automatic per-universe diagnostics: convergence, fixed-effect under high
#' heterogeneity, small k, extreme leverage of the estimate relative to the
#' cell median. Failing universes are flagged for scrutiny (and shown
#' separately in reports), implementing the worst-universe evaluation
#' convention: the multiverse is judged by its worst universes, which must
#' therefore be visible.
#'
#' @param result An \code{mv_grid_result}.
#' @param i2_high Threshold above which fixed-effect models are flagged
#'   (default 50 percent).
#' @return Data frame: spec_id, flags (comma-joined), n_flags.
#' @export
audit_universes <- function(result, i2_high = 50) {
  stopifnot(inherits(result, "mv_grid_result"))
  df <- result$results
  if (is.null(df) || nrow(df) == 0) {
    return(data.frame(spec_id = character(), flags = character(),
                      n_flags = integer(), stringsAsFactors = FALSE))
  }
  config <- attr(result$grid, "config")
  flags <- lapply(seq_len(nrow(df)), function(i) {
    f <- character()
    if (!isTRUE(df$converged[i])) f <- c(f, "not_converged")
    ran_fe <- (!is.na(df$ma_method[i]) && df$ma_method[i] == "fe") ||
      (!is.na(df$method_label[i]) && df$method_label[i] == "FE")
    if (ran_fe && !is.na(df$i2[i]) && df$i2[i] > i2_high) {
      f <- c(f, "fixed_effect_under_high_i2")
    }
    if (!is.na(df$k[i]) && df$k[i] < config$min_studies_strict) {
      f <- c(f, "below_strict_study_threshold")
    }
    cell <- df$cell_id[i]
    cell_b <- df$b[df$cell_id == cell & df$layer == df$layer[i]]
    if (length(cell_b) >= 5) {
      cell_mad <- stats::mad(cell_b, constant = 1.4826)
      if (cell_mad > 0 &&
          abs(df$b[i] - stats::median(cell_b)) / cell_mad > 3.5) {
        f <- c(f, "extreme_within_cell")
      }
    }
    f
  })
  data.frame(
    spec_id = df$spec_id,
    flags = vapply(flags, paste, character(1), collapse = ","),
    n_flags = vapply(flags, length, integer(1)),
    stringsAsFactors = FALSE
  )
}

#' Per-cell effect summaries with the estimand pooling guard
#'
#' Effect-size summaries are computed per estimand cell only. Requesting a
#' pooled effect summary across cells is refused with an explanation -- the
#' two-layer rule: effects within cells, divergence between cells.
#'
#' @param result An \code{mv_grid_result}.
#' @param cells Optional character vector of cell ids to summarize (default
#'   all, each separately).
#' @return Data frame of per-cell summaries (inference layer only).
#' @export
summarize_effects <- function(result, cells = NULL) {
  stopifnot(inherits(result, "mv_grid_result"))
  df <- result$results
  df <- df[df$layer == "inference", , drop = FALSE]
  if (!is.null(cells)) df <- df[df$cell_id %in% cells, , drop = FALSE]
  if (nrow(df) == 0) stop("No inference-layer results to summarize.",
                          call. = FALSE)
  out <- do.call(rbind, lapply(split(df, df$cell_id), function(d) {
    data.frame(
      cell_id = d$cell_id[1],
      n_specs = nrow(d),
      median_b = stats::median(d$b),
      q10 = stats::quantile(d$b, 0.1, names = FALSE),
      q90 = stats::quantile(d$b, 0.9, names = FALSE),
      min_b = min(d$b), max_b = max(d$b),
      sign_consistency = max(mean(d$b > 0), mean(d$b < 0)),
      stringsAsFactors = FALSE
    )
  }))
  rownames(out) <- NULL
  out
}

#' Refuse pooled effect summaries across estimand cells
#'
#' Runtime guard used by summary and plotting code: throws if the supplied
#' specification results span more than one estimand cell.
#'
#' @param df Data frame of results with a \code{cell_id} column.
#' @param what Label for the refused quantity (used in the error message).
#' @return Invisibly TRUE if a single cell.
#' @export
assert_single_estimand <- function(df, what = "an effect summary") {
  cells <- unique(df$cell_id)
  if (length(cells) > 1) {
    stop("Refusing to compute ", what, " across ", length(cells),
         " estimand cells (", paste(cells, collapse = "; "),
         "): pooled effect summaries across different estimands are not ",
         "defensible (two-layer rule). Summarize per cell, or use ",
         "summarize_divergence() for labeled between-estimand comparisons.",
         call. = FALSE)
  }
  invisible(TRUE)
}

#' Between-cell divergence summary (labeled between-estimand layer)
#'
#' Compares estimand cells without pooling them into an effect estimate:
#' per-cell medians, pairwise median differences, and the share of total
#' specification variance lying between cells. This layer compares
#' estimands; it never averages them.
#'
#' @param result An \code{mv_grid_result}.
#' @return List with \code{cell_summaries}, \code{between_cell_differences},
#'   \code{variance_between_share}.
#' @export
summarize_divergence <- function(result) {
  stopifnot(inherits(result, "mv_grid_result"))
  df <- result$results
  df <- df[df$layer == "inference", , drop = FALSE]
  cells <- summarize_effects(result)
  if (nrow(cells) < 2) {
    return(list(cell_summaries = cells,
                between_cell_differences = NULL,
                variance_between_share = NA_real_))
  }
  combs <- utils::combn(cells$cell_id, 2, simplify = FALSE)
  diffs <- do.call(rbind, lapply(combs, function(p) {
    data.frame(
      cell_a = p[1], cell_b = p[2],
      delta_median = cells$median_b[cells$cell_id == p[1]] -
        cells$median_b[cells$cell_id == p[2]],
      stringsAsFactors = FALSE
    )
  }))
  grand <- mean(df$b)
  between <- sum(tapply(df$b, df$cell_id, function(x) {
    length(x) * (mean(x) - grand)^2
  }))
  total <- sum((df$b - grand)^2)
  list(
    cell_summaries = cells,
    between_cell_differences = diffs,
    variance_between_share = if (total > 0) between / total else NA_real_
  )
}
