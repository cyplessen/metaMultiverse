#' Vibration of Effects Plot
#'
#' Visualizes the distribution of effect sizes and p-values across multiverse specifications.
#' Points are colored by density to highlight concentration areas. Automatically handles
#' multiple multiverses when non-equivalent factors are present.
#'
#' @param x A multiverse_result object from \code{\link{run_multiverse_analysis}}
#' @param x_var Column name for x-axis values. Default: "b" (effect size)
#' @param y_var Column name for y-axis values. Default: "pval" (p-value)
#' @param colorblind_friendly Logical. Use colorblind-friendly palette. Default: TRUE
#' @param cutoff Minimum number of studies required for inclusion. Default: 5
#' @param x_breaks Numeric vector of x-axis break points. Default: NULL (auto-generated)
#' @param y_breaks Numeric vector of y-axis break points for log scale.
#'   Default: c(1e-11, 1e-10, 1e-9, 1e-8, 1e-7, 1e-6, 1e-5, 1e-4, 0.001, 0.01, 0.05, 0.1, 0.5, 1)
#' @param x_limits Numeric vector of length 2 for x-axis limits. Default: NULL (auto-calculated)
#' @param y_limits Numeric vector of length 2 for y-axis limits. Default: c(1e-11, 1)
#' @param vertical_lines Numeric vector of quantiles for vertical reference lines. Default: c(0.1, 0.9)
#' @param hline_value Y-value for horizontal reference line. Default: 0.05
#' @param title_template Character string with glue syntax for plot title.
#'   Available variables: k (number of analyses), cutoff.
#' @param interactive Logical. Return interactive plotly (TRUE) or static ggplot2 (FALSE). Default: TRUE
#'
#' @return A plotly object if interactive = TRUE, otherwise a ggplot2 object
#'
#' @details
#' The plot displays:
#' \itemize{
#'   \item Points colored by density (high-density areas highlighted)
#'   \item Log-scaled y-axis for p-values
#'   \item Vertical lines at specified quantiles of the effect size distribution
#'   \item Horizontal line at significance threshold (default: p = 0.05)
#'   \item Automatic faceting when multiple multiverses exist (from N-type factors)
#' }
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' multiverse %>% plot_voe()
#'
#' # Customize significance threshold and quantiles
#' multiverse %>%
#'   plot_voe(hline_value = 0.01,
#'           vertical_lines = c(0.25, 0.75))
#'
#' # Static plot with custom limits
#' multiverse %>%
#'   plot_voe(interactive = FALSE,
#'           x_limits = c(-0.5, 1.5),
#'           cutoff = 10)
#' }
#'
#' @seealso
#' \code{\link{plot_spec_curve}} for specification curve visualization
#' \code{\link{run_multiverse_analysis}} for generating input data
#'
#' @export
plot_voe <- function(
    x,
    x_var = "b",
    y_var = "pval",
    colorblind_friendly = TRUE,
    cutoff = 5,
    x_breaks = NULL,
    y_breaks = c(1e-11, 1e-10, 1e-9, 1e-8, 1e-7, 1e-6, 1e-5, 1e-4, 0.001, 0.01, 0.05, 0.1, 0.5, 1),
    x_limits = NULL,
    y_limits = c(1e-11, 1),
    vertical_lines = c(0.1, 0.9),
    hline_value = 0.05,
    title_template = "{k} meta-analyses with at least {cutoff} studies",
    interactive = TRUE
) {

  if (!inherits(x, "multiverse_result")) {
    stop("Input must be a multiverse_result object from run_multiverse_analysis()")
  }

  data <- x$results
  if (is.null(data) || nrow(data) == 0) {
    stop("No results to plot")
  }

  # Filter and prepare data
  data <- data %>%
    dplyr::mutate(!!dplyr::sym(y_var) := ifelse(!!dplyr::sym(y_var) < 1e-11, 1e-11, !!dplyr::sym(y_var))) %>%
    dplyr::filter(k >= cutoff)

  if (nrow(data) == 0) {
    stop(paste("No analyses with at least", cutoff, "studies"))
  }

  # Check if we have multiple multiverse_ids (Type N factors present)
  has_multiple_multiverses <- length(unique(data$multiverse_id)) > 1

  # Compute density-based colors using the helper function
  data <- compute_density_colors(data, x_var, y_var, colorblind_friendly)

  # Add tooltips only if interactive
  if (interactive) {
    data <- generate_tooltip_voe(data, x_var, y_var)
  }

  # Number of analyses
  k <- nrow(data)

  # Auto-generate x_breaks if not provided
  if (is.null(x_breaks)) {
    x_range <- range(data[[x_var]], na.rm = TRUE)
    x_breaks <- pretty(x_range, n = 8)
  }

  # Dynamically compute x_limits if not provided
  if (is.null(x_limits)) {
    x_limits <- c(min(data[[x_var]], na.rm = TRUE) - 0.1,
                  max(data[[x_var]], na.rm = TRUE) + 0.1)
  }

  # Create base ggplot - only include text aesthetic if interactive
  if (interactive) {
    p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]], text = tooltip))
  } else {
    p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]]))
  }

  p <- p +
    ggplot2::geom_point(ggplot2::aes(colour = density),
                        size = 1.5,
                        alpha = 0.7,
                        show.legend = FALSE) +
    ggplot2::scale_color_identity() +
    ggplot2::geom_hline(yintercept = hline_value, linetype = 2, color = "black", alpha = 0.5) +
    ggplot2::scale_y_continuous(
      trans = "log10",
      breaks = y_breaks,
      labels = scales::label_scientific(),
      limits = y_limits
    ) +
    ggplot2::scale_x_continuous(
      breaks = x_breaks,
      limits = x_limits
    ) +
    ggplot2::labs(
      x = "Effect Size",
      y = "P-value",
      title = if (has_multiple_multiverses) {
        glue::glue("{k} meta-analyses across {length(unique(data$multiverse_id))} multiverses (>={cutoff} studies)")
      } else {
        glue::glue(title_template, k = k, cutoff = cutoff)
      }
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.border = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 12, face = "bold"),
      axis.title = ggplot2::element_text(size = 11),
      axis.text = ggplot2::element_text(size = 10),
      legend.position = "none"  # Explicitly hide any legend
    ) +
    ggplot2::guides(colour = "none", fill = "none", text = "none")  # Remove all guides

  # Add faceting if multiple multiverses exist
  if (has_multiple_multiverses) {
    # Compute quantiles per multiverse for vertical lines
    quantile_data <- data %>%
      dplyr::group_by(multiverse_id) %>%
      dplyr::summarise(
        q10 = stats::quantile(.data[[x_var]], vertical_lines[1], na.rm = TRUE),
        q90 = stats::quantile(.data[[x_var]], vertical_lines[2], na.rm = TRUE),
        .groups = "drop"
      )

    # Add vertical lines per facet
    p <- p +
      ggplot2::geom_vline(data = quantile_data,
                          ggplot2::aes(xintercept = q10),
                          color = "red", linetype = 2, alpha = 0.5) +
      ggplot2::geom_vline(data = quantile_data,
                          ggplot2::aes(xintercept = q90),
                          color = "red", linetype = 2, alpha = 0.5) +
      ggplot2::facet_wrap(~ multiverse_id,
                          labeller = ggplot2::labeller(
                            multiverse_id = function(x) paste("Multiverse:", x)
                          ))
  } else {
    # Single multiverse - add global quantiles
    x_quantiles <- stats::quantile(data[[x_var]], vertical_lines, na.rm = TRUE)
    p <- p +
      ggplot2::geom_vline(xintercept = x_quantiles, color = "red", linetype = 2, alpha = 0.5)
  }

  if (interactive) {
    # Convert to Plotly for interactivity
    p_plotly <- plotly::ggplotly(p, tooltip = "text") %>%
      plotly::layout(
        hoverlabel = list(bgcolor = "white", font = list(color = "black")),
        hovermode = "closest",
        showlegend = FALSE  # Also hide legend in plotly
      )
    return(p_plotly)
  } else {
    return(p)
  }
}
