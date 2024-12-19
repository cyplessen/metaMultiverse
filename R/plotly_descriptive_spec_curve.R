#' Interactive Specification Curve Plot
#'
#' Creates an interactive specification curve plot with a forest plot for effect sizes and a tile plot for specification factors.
#'
#' @param data Data frame containing the specification data and results.
#' @param ylim_lower Numeric. Lower limit for the y-axis of the forest plot.
#' @param ylim_upper Numeric. Upper limit for the y-axis of the forest plot.
#' @param colorblind_friendly Logical. Use a colorblind-friendly palette (default: TRUE).
#' @param interactive Logical. Whether to create an interactive Plotly plot (`TRUE`) or static ggplot plot (`FALSE`). Default is `TRUE`.
#'
#' @return A `plotly` object containing the interactive specification curve plot if `interactive = TRUE`, or a combined `ggplot` object if `interactive = FALSE`.
#' @details Combines a forest plot for effect sizes with confidence intervals and a tile plot visualizing specification factors.
#'
#' @examples
#' plotly_descriptive_spec_curve(data, ylim_lower = -0.5, ylim_upper = 1.5, interactive = TRUE)
#'
#' @importFrom dplyr select mutate left_join filter distinct
#' @importFrom ggplot2 ggplot aes geom_raster geom_errorbar geom_line geom_point geom_hline labs scale_x_continuous scale_y_discrete scale_fill_manual scale_color_manual theme_classic theme_bw
#' @importFrom plotly ggplotly subplot style
#' @importFrom viridis viridis
#' @importFrom RColorBrewer brewer.pal
#' @importFrom stringr str_to_sentence
#' @importFrom scales label_scientific
#' @importFrom glue glue
#' @importFrom cowplot plot_grid
#'
#' @export
plotly_descriptive_spec_curve <- function(data,
                                          ylim_lower = NULL,
                                          ylim_upper = NULL,
                                          colorblind_friendly = TRUE,
                                          interactive = TRUE) {

  # Detect "Which" and "How" factors dynamically
  wf_cols <- colnames(data)[grep("^wf_", colnames(data))]
  how_cols <- colnames(data)[colnames(data) %in% c("ma_method")]
  all_factors <- c(wf_cols, how_cols)  # Combine for plotting

  # Generate human-readable labels for factors
  factor_labels <- generate_dynamic_labels(data, factor_label_lookup)

  # Create levels for the y-axis by combining labels with data values
  factor_levels <- lapply(all_factors, function(col) {
    unique_values <- unique(data[[col]])
    replaced_values <- ifelse(grepl("total_", unique_values),
                              "Combined",
                              stringr::str_to_sentence(unique_values))  # Handle "Combined"
    paste0(factor_labels[[col]], ": ", replaced_values)  # Combine with labels
  })

  # Create y-axis factor levels for plotting
  yvar <- factor(
    rev(unlist(factor_levels)),  # Reverse for better ordering in the plot
    levels = rev(unlist(factor_levels))
  )

  # Rank effect sizes for x-axis
  x_rank <- rank(data$b, ties.method = "random")

  # Expand x_rank across all levels of yvar
  xvar <- rep(x_rank, each = length(levels(yvar)))

  # Select columns required for plotting
  data <- data %>%
    dplyr::select(all_of(all_factors), b, ci.lb, ci.ub, pval, k, set)

  # Generate spec matrix to define the tile plot structure
  spec <- NULL
  for (i in 1:nrow(data)) {
    id <- as.numeric(yvar %in%
                       unlist(lapply(all_factors, function(col) {
                         value <- stringr::str_to_sentence(data[[col]][i])
                         if (grepl("total_", data[[col]][i])) {
                           paste0(factor_labels[[col]], ": Combined")
                         } else {
                           paste0(factor_labels[[col]], ": ", value)
                         }
                       })))
    spec <- c(spec, id)
  }

  # Prepare plot data for tile plot
  plotdata <- data.frame(xvar, yvar, spec)
  plotdata$k <- rep(data$k, each = length(levels(yvar)))  # Repeat study count
  plotdata$fill <- as.factor(plotdata$k * plotdata$spec)  # Map to color intensity

  # Assign quantile-based levels for colors
  data <- assign_quantile_levels(data, k_column = "k", num_levels = 9)

  # Choose color palette based on `colorblind_friendly`
  palette_colors <- if (colorblind_friendly) {
    viridis::viridis(length(levels(data$fill_levels)), option = "C", direction = -1)
  } else {
    RColorBrewer::brewer.pal(length(levels(data$fill_levels)), "Spectral")
  }

  # Match colors to plot data
  data <- data %>% dplyr::mutate(fill_manual = as.factor(fill_levels))
  plotdata <- plotdata %>%
    dplyr::left_join(data %>% dplyr::select(k, fill_manual) %>% dplyr::distinct(), by = "k") %>%
    dplyr::mutate(fill_manual = ifelse(spec == 1, fill_manual, 0),
                  fill_manual = as.factor(fill_manual))  # White for empty cells

  # Create the tile plot
  tile_plot <- ggplot2::ggplot(data = plotdata,
                               ggplot2::aes(x = xvar,
                                   y = as.factor(yvar),
                                   fill = fill_manual,
                                   text = yvar)) +
    ggplot2::geom_raster() +
    ggplot2::scale_x_continuous(position = "bottom") +
    ggplot2::scale_y_discrete(labels = levels(yvar)) +
    ggplot2::scale_fill_manual(values = c("white", palette_colors)) +
    ggplot2::labs(x = "Specification Number", y = "Which/How Factors") +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "none",
                   axis.text.y = ggplot2::element_text(colour = "black", size = 8),
                   axis.text.x = ggplot2::element_text(colour = "black"),
                   axis.ticks = ggplot2::element_line(colour = "black")) +
    ggplot2::coord_cartesian(expand = FALSE, xlim = c(0, nrow(data)))

  # Define y-axis limits for the forest plot
  yrng <- range(c(0, data$ci.lb, data$ci.ub))
  if (!is.null(ylim_lower)) yrng[1] <- ylim_lower
  if (!is.null(ylim_upper)) yrng[2] <- ylim_upper

  # Add tooltips to data
  data <- generate_tooltip(data, factor_label_lookup)
  data <- data %>% dplyr::mutate(group_id = 1)

  # Create the forest plot
  spec_curve_plot <- data %>%
    ggplot2::ggplot(ggplot2::aes(x = x_rank, y = b, text = tooltip, group = group_id)) +
    ggplot2::geom_line(col = "black", linewidth = .2) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = ci.lb, ymax = ci.ub, col = as.factor(fill_manual)),
                           width = 0.1, linewidth = 0.6) +
    ggplot2::geom_point(size = .2) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    ggplot2::labs(y = "Summary Effect", x = "") +
    ggplot2::scale_color_manual(values = palette_colors) +
    ggplot2::coord_cartesian(ylim = yrng, xlim = c(0, nrow(data)), expand = FALSE) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none",
                   axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank())

  if (interactive) {
    # Convert to Plotly for interactivity
    spec_curve_plotly <- plotly::ggplotly(spec_curve_plot, tooltip = "text")  %>%
      plotly::style(hoverlabel = list(bgcolor = "white", font = list(color = "black")))

    tile_plot_plotly <- plotly::ggplotly(tile_plot, tooltip = "text")

    # Combine both plots into a single interactive layout
    p <- plotly::subplot(spec_curve_plotly, tile_plot_plotly, nrows = 2, shareX = TRUE, titleY = TRUE)
    return(p)
  } else {
    # Combine ggplot plots using cowplot
    p <- cowplot::plot_grid(spec_curve_plot, tile_plot, ncol = 1, align = "v", rel_heights = c(1, 1.5))
    return(p)
  }
}
