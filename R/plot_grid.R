# ABOUTME: Visualization for grid results: two-layer (p-box style) plot
# ABOUTME: separating possibilistic spec-variation from probabilistic
# ABOUTME: estimation uncertainty, and a per-cell specification curve.

#' Compact display form of a cell id
#'
#' Strips the fork-name prefixes from a cell id so facet strips stay
#' readable: \code{"comparator=wl|intervention_type=cbt"} becomes
#' \code{"wl / cbt"}.
#'
#' @param x Character vector of cell ids.
#' @return Character vector of shortened labels.
#' @export
shorten_cell_label <- function(x) {
  x <- gsub("[a-z_]+=", "", x)
  gsub("\\|", " / ", x)
}

#' Two-layer uncertainty plot (p-box style)
#'
#' Milliways-style display keeping the two uncertainty layers visually
#' distinct: the step ECDF of point estimates shows possibilistic
#' specification variation; the shaded envelope between the ECDFs of the
#' lower and upper confidence limits shows probabilistic estimation
#' uncertainty. Frequencies on the y axis are descriptive shares of
#' specifications, not probabilities of correctness.
#'
#' @param result An \code{mv_grid_result}.
#' @param cell Estimand cell id (one cell: effect displays never pool
#'   estimand cells).
#' @return A ggplot object.
#' @export
plot_two_layer <- function(result, cell) {
  df <- cell_slice(result, cell)
  ord_b <- sort(df$b)
  ord_lb <- sort(df$ci.lb)
  ord_ub <- sort(df$ci.ub)
  p <- seq_along(ord_b) / length(ord_b)
  band <- data.frame(
    x_lb = ord_lb, x_ub = ord_ub, p = p
  )
  est <- data.frame(x = ord_b, p = p)

  ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      data = band,
      ggplot2::aes(xmin = .data$x_lb, xmax = .data$x_ub, y = .data$p),
      fill = "steelblue", alpha = 0.25, orientation = "y"
    ) +
    ggplot2::geom_step(
      data = est, ggplot2::aes(x = .data$x, y = .data$p),
      linewidth = 0.9, color = "black"
    ) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                        color = "grey50") +
    ggplot2::labs(
      title = sprintf("Two-layer uncertainty: %s", cell),
      subtitle = paste("Black step: specification variation (possibilistic).",
                       "Blue band: estimation uncertainty (probabilistic)."),
      x = "Hedges' g",
      y = "cumulative share of specifications (descriptive)"
    ) +
    ggplot2::theme_minimal()
}

#' Specification curve per estimand cell
#'
#' Classic magnitude-ordered specification curve, faceted by estimand cell
#' so effect displays never mix estimands. Inference layer only.
#'
#' @param result An \code{mv_grid_result}.
#' @param color_by Fork name to color points by (default dependency if
#'   present).
#' @return A ggplot object.
#' @export
plot_grid_spec_curve <- function(result, color_by = NULL) {
  stopifnot(inherits(result, "mv_grid_result"))
  df <- result$results
  df <- df[df$layer == "inference", , drop = FALSE]
  if (is.null(color_by)) {
    color_by <- if ("dependency" %in% names(df)) "dependency" else NULL
  }
  df <- df[order(df$cell_id, df$b), ]
  df$spec_rank <- stats::ave(seq_len(nrow(df)), df$cell_id,
                             FUN = seq_along)

  aes_args <- if (!is.null(color_by)) {
    ggplot2::aes(x = .data$spec_rank, y = .data$b,
                 color = .data[[color_by]])
  } else {
    ggplot2::aes(x = .data$spec_rank, y = .data$b)
  }
  ggplot2::ggplot(df, aes_args) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                        color = "grey50") +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = .data$ci.lb, ymax = .data$ci.ub),
      width = 0, alpha = 0.25
    ) +
    ggplot2::geom_point(size = 0.9) +
    ggplot2::facet_wrap(~cell_id, scales = "free_x",
                        labeller = ggplot2::as_labeller(shorten_cell_label)) +
    ggplot2::labs(
      title = "Specification curves per estimand cell",
      x = "specifications, ordered by effect size",
      y = "Hedges' g"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")
}
