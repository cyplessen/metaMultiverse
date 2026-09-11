#' Compute Density-Based Coloring
#'
#' Adds a `density` column to the dataset based on the relationship between two variables.
#'
#' @param data Data frame containing the input data.
#' @param x Character. Name of the x-variable column.
#' @param y Character. Name of the y-variable column.
#' @param colorblind_friendly Logical. Use a colorblind-friendly palette (default: TRUE).
#'
#' @return A data frame with an additional `density` column for coloring points.
#'
#' @importFrom grDevices densCols rainbow colorRampPalette
#' @importFrom viridis viridis
compute_density_colors <- function(data, x, y, colorblind_friendly = TRUE) {
  dens_values <- grDevices::densCols(
    x = data[[x]],
    y = data[[y]],
    colramp = if (colorblind_friendly)
      grDevices::colorRampPalette(viridis::viridis(256, direction = -1))
    else
      grDevices::colorRampPalette(rev(grDevices::rainbow(10, end = 4/6)))
  )
  data$density <- dens_values
  return(data)
}

#' Generate Tooltips for Vibration of Effects Plot
#'
#' Creates a `tooltip` column for interactive plots by dynamically summarizing key information.
#'
#' @param data Data frame containing the input data.
#' @param x Character. Name of the x-variable column for the effect size
#' @param y Character. Name of the y-variable column for the pvalue.
#' @param k_col Character. Name of the column containing the number of studies (default: "k").
#' @param set_col Character. Name of the column containing the study set IDs (default: "set").
#'   Omitted from the tooltip when the column is absent.
#' @param factors Character vector of factor columns whose values are appended
#'   to the tooltip, one line each (default: none).
#' @param factor_labels Named list of display labels for \code{factors}; a
#'   factor without a label is shown under its column name.
#'
#' @return A data frame with an additional `tooltip` column.
#'
#' @importFrom dplyr mutate %>%
#' @importFrom glue glue
#' @importFrom scales scientific
#' @importFrom stringr str_wrap
generate_tooltip_voe <- function(data, x, y, k_col = "k", set_col = "set",
                                 factors = character(0), factor_labels = list()) {
  tooltip <- paste0(
    "<b>Effect Size (d):</b> ", round(data[[x]], 3), "<br>",  # This will always show "Effect Size (d)"
    "<b>P-value:</b> ", scales::scientific(data[[y]], digits = 8), "<br>",
    "<b>Number of Studies:</b> ", data[[k_col]]
  )
  if (set_col %in% names(data)) {
    tooltip <- paste0(tooltip, "<br><b>Study Set:</b> ",
                      stringr::str_wrap(as.character(data[[set_col]]), width = 30))
  }
  for (col in factors) {
    label <- if (!is.null(factor_labels[[col]])) factor_labels[[col]] else col
    tooltip <- paste0(tooltip, "<br><b>", label, ":</b> ", as.character(data[[col]]))
  }
  data$tooltip <- tooltip
  data
}
