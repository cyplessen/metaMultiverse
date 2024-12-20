# Suppress warnings for dynamically evaluated variables
globalVariables(c("density", ".data"))

#' Interactive Vibration of Effects Plot
#'
#' Creates an interactive plot visualizing the vibration of effects with effect sizes and p-values.
#'
#' @param data Data frame containing the input data for plotting.
#' @param x Character. Name of the x-variable column (default: "b").
#' @param y Character. Name of the y-variable column (default: "pvalue").
#' @param colorblind_friendly Logical. Use a colorblind-friendly palette (default: TRUE).
#' @param cutoff Numeric. Minimum number of studies for inclusion (default: 10).
#' @param x_breaks Numeric vector. Break points for the x-axis.
#' @param y_breaks Numeric vector. Break points for the y-axis.
#' @param x_limits Numeric vector. Limits for the x-axis.
#' @param y_limits Numeric vector. Limits for the y-axis.
#' @param vertical_lines Numeric vector. Quantiles for vertical reference lines (default: c(0.1, 0.9)).
#' @param hline_value Numeric. Horizontal reference line value (default: 0.05).
#' @param title_template Character. Template for the plot title (supports glue syntax).
#' @param interactive Logical. Whether to create an interactive Plotly plot (`TRUE`) or static ggplot plot (`FALSE`).
#'
#' @return A `plotly` object containing the interactive vibration of effects plot if `interactive = TRUE`, or a combined `ggplot` object if `interactive = FALSE`.
#' @examples
#'
#'example_data <- data.frame(
#'     b = as.numeric(c(0.5, 0.3, 0.3)),
#'     pval = as.numeric(c(0.001, 0.05, 0.3)),
#'     k = c(10, 12, 11),
#'     set = c("1,2,3", "4,5,6", "1")
#'   )
#'plotly_VoE(
#'    data = example_data,
#'    x = "b",
#'    y = "pval",
#'    colorblind_friendly = TRUE,
#'    interactive = FALSE
#'  )
#'
#' @export
#' @importFrom dplyr mutate filter sym %>%
#' @importFrom ggplot2 ggplot aes geom_jitter geom_hline geom_vline scale_color_identity scale_y_continuous scale_x_continuous labs theme_bw theme
#' @importFrom plotly ggplotly layout
#' @importFrom scales label_scientific
#' @importFrom glue glue
#' @importFrom stats quantile
#' @importFrom cowplot plot_grid
#' @importFrom data.table :=
plotly_VoE <- function(
    data,
    x = "b",
    y = "pval",
    colorblind_friendly = TRUE,
    cutoff = 10,
    x_breaks = seq(-0.5, 2, by = 0.25),
    y_breaks = c(1e-11, 1e-10, 1e-9, 1e-8, 1e-7, 1e-6, 1e-5, 1e-4, 0.001, 0.01, 0.05, 0.1, 0.5, 1),
     x_limits = NULL,
     y_limits = c(1e-11,1),
    vertical_lines = c(0.1, 0.9),
    hline_value = 0.05,
    title_template = "{k} meta-analyses with at least {cutoff} studies.",
    interactive = TRUE
) {
  # Filter and prepare data
  data <- data %>%
    dplyr::mutate(!!dplyr::sym(y) := ifelse(!!dplyr::sym(y) < 1e-11, 1e-11, !!dplyr::sym(y))) %>%
    dplyr::filter(k >= cutoff)

  # Compute density-based colors
  data <- compute_density_colors(data, x, y, colorblind_friendly)

  # Add tooltips
  data <- generate_tooltip_voe(data, x, y)

  # Number of analyses
  k <- nrow(data)
  x_quantiles <- stats::quantile(data[[x]], vertical_lines, na.rm = TRUE)  # Compute quantiles

  # Dynamically compute x_limits and y_limits if not provided
  if (is.null(x_limits)) {
    x_limits <- c(min(data[[x]], na.rm = TRUE) -.3, max(data[[x]], na.rm = TRUE) + .3)
  }

  if (is.null(y_limits)) {
    y_limits <- c(min(data[[y]], na.rm = TRUE), max(data[[y]], na.rm = TRUE))
  }

  # Create ggplot base
  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[x]], y = .data[[y]], text = tooltip)) +
    ggplot2::geom_point(ggplot2::aes(colour = density),
                         size = 1,
                         #width = 0.01,
                        show.legend = FALSE) +
    ggplot2::scale_color_identity() +
    ggplot2::geom_hline(yintercept = hline_value, linetype = 2, color = "black") +
    ggplot2::geom_vline(xintercept = x_quantiles, color = "red", linetype = 2) +
    ggplot2::scale_y_continuous(
      trans = "log",
      breaks = y_breaks,
      labels = scales::label_scientific(),
      limits = y_limits
    ) +
    ggplot2::scale_x_continuous(
      breaks = x_breaks,
      limits = x_limits
    ) +
    ggplot2::labs(
      x = "Effect Size (b)",
      y = "P-value",
      title = glue::glue(title_template, k = k, cutoff = cutoff)
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.border = ggplot2::element_blank(), plot.title = ggplot2::element_text(size = 10)) +
    ggplot2::guides(colour = "none")

  if (interactive) {
    # Convert to Plotly for interactivity
    p_plotly <- plotly::ggplotly(p, tooltip = "text") %>%
      plotly::layout(hoverlabel = list(bgcolor = "white", font = list(color = "black")))

    return(p_plotly)
  } else {
    # Combine ggplot plots using cowplot
    p_static <- cowplot::plot_grid(p, ncol = 1, align = "v", rel_heights = c(1))

    return(p_static)
  }
}
