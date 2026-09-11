#' Specification Curve Plot
#'
#' Creates a two-panel visualization showing effect sizes with confidence intervals (top)
#' and the corresponding specification choices (bottom). Effects are sorted by magnitude
#' and colored by the number of studies included.
#'
#' @param x A \code{multiverse_result} object from
#'   \code{\link{run_multiverse_analysis}}, a \code{pre_post_multiverse} object
#'   from \code{\link{run_pre_post_multiverse}}, or a data frame of results
#'   with columns \code{b}, \code{ci.lb}, \code{ci.ub}, \code{pval}, \code{k}
#'   and one column per factor (see \code{factors})
#' @param ylim_lower Lower limit for effect size axis. Default: NULL (auto-calculated as min CI * 1.1)
#' @param ylim_upper Upper limit for effect size axis. Default: NULL (auto-calculated as max CI * 1.1)
#' @param colorblind_friendly Logical. Use colorblind-friendly palette. Default: TRUE
#' @param interactive Logical. Return interactive plotly (TRUE) or static ggplot2 (FALSE). Default: TRUE
#' @param factors Character vector naming the columns of the results that hold
#'   the which/how factor levels, drawn top to bottom in the order given.
#'   Names, where present, are used as the display labels
#'   (e.g. \code{c("Rater" = "rater", "Metric" = "es_metric")}). Default:
#'   \code{NULL}, which uses the \code{wf_*}, \code{ma_method} and
#'   \code{dependency} columns for a \code{multiverse_result} or a data
#'   frame, and the \code{factors} element of a \code{pre_post_multiverse}.
#' @param ... Additional arguments (currently unused)
#'
#' @return A plotly object if interactive = TRUE, otherwise a ggplot2 object
#'
#' @details
#' The specification curve consists of two aligned panels:
#'
#' \strong{Top panel:} Forest plot showing:
#' \itemize{
#'   \item Effect sizes ordered by magnitude
#'   \item 95\% confidence intervals
#'   \item Color coding by number of included studies
#'   \item Horizontal reference line at zero
#' }
#'
#' \strong{Bottom panel:} Specification grid showing:
#' \itemize{
#'   \item Which factors (study inclusion criteria)
#'   \item How factors (analytical choices)
#'   \item Tiles colored by number of studies
#'   \item "Combined" indicates all levels included
#' }
#'
#' Factor labels are automatically extracted from the multiverse setup, or
#' taken from the names of \code{factors}; unlabelled columns are shown with a
#' title-cased version of the column name (\code{ma_method},
#' \code{dependency}, \code{es_metric} and \code{imputed_post_sd} have
#' built-in labels). A level starting with \code{"total_"} is shown as
#' "Combined". Interactive plots include hover tooltips with detailed
#' information for each specification.
#'
#' For the stacked results of \code{\link{run_pre_post_multiverse}} the
#' effect-size metric and the imputed-post-SD rule are drawn as additional
#' how factors. Any data frame with the required columns can be plotted by
#' naming its factor columns in \code{factors}, e.g. results stacked from
#' several calls.
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' multiverse %>% plot_spec_curve()
#'
#' # Static plot with custom y-axis limits
#' multiverse %>%
#'   plot_spec_curve(interactive = FALSE,
#'                  ylim_lower = -0.5,
#'                  ylim_upper = 1.5)
#'
#' # Non-colorblind-friendly palette (why would you?)
#' multiverse %>%
#'   plot_spec_curve(colorblind_friendly = FALSE)
#'
#' # Stacked pre/post results: the metric is drawn as a how factor
#' # (d holds arm-level pre/post data, see ?run_pre_post_multiverse)
#' mv <- run_pre_post_multiverse(d, which_factors = list(rater = "rater|E"))
#' plot_spec_curve(mv, interactive = FALSE)
#'
#' # Any results data frame, naming (and labelling) the factor columns
#' plot_spec_curve(mv$results, interactive = FALSE,
#'                 factors = c("Rater" = "rater", "Metric" = "es_metric",
#'                             "Method" = "ma_method"))
#' }
#'
#' @seealso
#' \code{\link{plot_voe}} for vibration of effects visualization
#' \code{\link{run_multiverse_analysis}} and
#' \code{\link{run_pre_post_multiverse}} for generating input data
#'
#' @export
plot_spec_curve <- function(x,
                            ylim_lower = NULL,
                            ylim_upper = NULL,
                            colorblind_friendly = TRUE,
                            interactive = TRUE,
                            factors = NULL,
                            ...) {

  input <- resolve_plot_input(x, factors,
                              required = c("b", "ci.lb", "ci.ub", "pval", "k"))
  data <- input$data
  all_factors <- input$factors
  factor_labels <- input$labels
  factor_groups <- input$factor_groups

  # Auto-detect y-axis limits
  if (is.null(ylim_lower)) {
    ylim_lower <- min(data$ci.lb, na.rm = TRUE) * 1.1
  }
  if (is.null(ylim_upper)) {
    ylim_upper <- max(data$ci.ub, na.rm = TRUE) * 1.1
  }

  # Create levels for the y-axis by combining labels with data values
  # Enhanced: Show custom group details if factor_groups exists
  factor_levels <- lapply(all_factors, function(col) {
    unique_values <- unique(data[[col]])
    unique_values <- unique_values[!is.na(unique_values)]

    replaced_values <- sapply(unique_values, function(val) {
      if (grepl("^total_", val)) {
        "Combined"
      } else {
        val_display <- stringr::str_to_sentence(as.character(val))

        # Check if this is a custom group and add details
        if (!is.null(factor_groups) && col %in% names(factor_groups)) {
          groups <- factor_groups[[col]]
          if (val %in% names(groups)) {
            # This is a custom group - add the levels it includes
            levels_included <- groups[[val]]
            if (length(levels_included) <= 3) {
              # Show all levels if 3 or fewer
              levels_str <- paste(levels_included, collapse = ", ")
              val_display <- paste0(val_display, " (", levels_str, ")")
            } else {
              # Show count if more than 3 levels
              val_display <- paste0(val_display, " (", length(levels_included), " levels)")
            }
          }
        }

        val_display
      }
    })

    paste0(factor_labels[[col]], ": ", replaced_values)
  })

  # Create y-axis factor levels for plotting
  yvar <- factor(
    rev(unlist(factor_levels)),
    levels = rev(unlist(factor_levels))
  )

  # Rank effect sizes for x-axis
  x_rank <- rank(data$b, ties.method = "random")

  # Expand x_rank across all levels of yvar
  xvar <- rep(x_rank, each = length(levels(yvar)))

  # Select columns required for plotting ("set" is carried along when present)
  plot_cols <- intersect(c(all_factors, "b", "ci.lb", "ci.ub", "pval", "k", "set"), colnames(data))
  plot_data <- data %>%
    dplyr::select(dplyr::all_of(plot_cols))

  # Generate spec matrix to define the tile plot structure
  spec <- NULL
  for (i in 1:nrow(plot_data)) {
    id_values <- unlist(lapply(all_factors, function(col) {
      value <- as.character(plot_data[[col]][i])
      if (grepl("^total_", value)) {
        paste0(factor_labels[[col]], ": Combined")
      } else {
        val_display <- stringr::str_to_sentence(value)

        # Check if this is a custom group and add details (same logic as above)
        if (!is.null(factor_groups) && col %in% names(factor_groups)) {
          groups <- factor_groups[[col]]
          if (value %in% names(groups)) {
            levels_included <- groups[[value]]
            if (length(levels_included) <= 3) {
              levels_str <- paste(levels_included, collapse = ", ")
              val_display <- paste0(val_display, " (", levels_str, ")")
            } else {
              val_display <- paste0(val_display, " (", length(levels_included), " levels)")
            }
          }
        }

        paste0(factor_labels[[col]], ": ", val_display)
      }
    }))

    id <- as.numeric(levels(yvar) %in% id_values)
    spec <- c(spec, id)
  }

  # Prepare plot data for tile plot
  plotdata <- data.frame(xvar, yvar, spec, stringsAsFactors = FALSE)
  plotdata$k <- rep(plot_data$k, each = length(levels(yvar)))
  plotdata$fill <- as.factor(plotdata$k * plotdata$spec)

  # Create fill levels based on k values
  k_values <- sort(unique(plot_data$k))
  if (length(k_values) > 9) {
    # Use quantiles if too many unique values
    fill_levels <- cut(plot_data$k,
                       breaks = quantile(plot_data$k, probs = seq(0, 1, length.out = 10)),
                       labels = 1:9,
                       include.lowest = TRUE)
  } else {
    fill_levels <- as.factor(plot_data$k)
  }

  plot_data$fill_levels <- fill_levels
  plot_data$fill_manual <- as.factor(fill_levels)

  # Choose color palette
  n_colors <- length(unique(plot_data$fill_manual))
  palette_colors <- if (colorblind_friendly) {
    viridis::viridis(n_colors, option = "C", direction = -1)
  } else {
    if (n_colors <= 9) {
      RColorBrewer::brewer.pal(max(3, n_colors), "Spectral")
    } else {
      colorRampPalette(RColorBrewer::brewer.pal(9, "Spectral"))(n_colors)
    }
  }

  # Match colors to plot data
  plotdata <- plotdata %>%
    dplyr::left_join(plot_data %>% dplyr::select(k, fill_manual) %>% dplyr::distinct(), by = "k") %>%
    dplyr::mutate(fill_manual = ifelse(spec == 1, as.character(fill_manual), "0"),
                  fill_manual = factor(fill_manual, levels = c("0", levels(plot_data$fill_manual))))

  # Create the tile plot
  tile_plot <- ggplot2::ggplot(data = plotdata,
                               ggplot2::aes(x = xvar,
                                            y = yvar,
                                            fill = fill_manual)) +
    ggplot2::geom_raster() +
    ggplot2::scale_x_continuous(position = "bottom", expand = c(0, 0)) +
    ggplot2::scale_y_discrete(labels = levels(yvar)) +
    ggplot2::scale_fill_manual(values = c("0" = "white", setNames(palette_colors, levels(plot_data$fill_manual)))) +
    ggplot2::labs(x = "Specification Number", y = "Which/How Factors") +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "none",
                   axis.text.y = ggplot2::element_text(colour = "black", size = 8),
                   axis.text.x = ggplot2::element_text(colour = "black"),
                   axis.ticks = ggplot2::element_line(colour = "black"))

  # Create tooltip text
  plot_data$tooltip <- apply(plot_data, 1, function(row) {
    factor_info <- paste(names(factor_labels), ": ", row[names(factor_labels)], collapse = "\n")
    paste0("Effect: ", round(as.numeric(row["b"]), 3), "\n",
           "95% CI: [", round(as.numeric(row["ci.lb"]), 3), ", ",
           round(as.numeric(row["ci.ub"]), 3), "]\n",
           "p-value: ", format.pval(as.numeric(row["pval"]), digits = 3), "\n",
           "k studies: ", row["k"], "\n\n",
           factor_info)
  })

  plot_data$group_id <- 1

  # Create the forest plot
  spec_curve_plot <- plot_data %>%
    ggplot2::ggplot(ggplot2::aes(x = x_rank, y = b, text = tooltip, group = group_id)) +
    ggplot2::geom_line(col = "black", linewidth = .2) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = ci.lb, ymax = ci.ub, col = fill_manual),
                           width = 0.1, linewidth = 0.6) +
    ggplot2::geom_point(size = .2) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    ggplot2::labs(y = "Summary Effect", x = "") +
    ggplot2::scale_color_manual(values = palette_colors) +
    ggplot2::coord_cartesian(ylim = c(ylim_lower, ylim_upper),
                             xlim = c(0.5, nrow(plot_data) + 0.5),
                             expand = FALSE) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none",
                   axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank())

  if (interactive) {
    # Convert to Plotly for interactivity
    spec_curve_plotly <- plotly::ggplotly(spec_curve_plot, tooltip = "text") %>%
      plotly::style(hoverlabel = list(bgcolor = "white", font = list(color = "black")))

    tile_plot_plotly <- plotly::ggplotly(tile_plot, tooltip = FALSE)

    # Combine both plots into a single interactive layout
    p <- plotly::subplot(spec_curve_plotly, tile_plot_plotly,
                         nrows = 2, shareX = TRUE, titleY = TRUE,
                         heights = c(0.6, 0.4))
    return(p)
  } else {
    # Combine ggplot plots using cowplot
    p <- cowplot::plot_grid(spec_curve_plot, tile_plot,
                            ncol = 1, align = "v", rel_heights = c(1, 1.5))
    return(p)
  }
}
