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
#' @examples
#' compute_density_colors(data, x = "b", y = "pvalue")
#'
#' @export
#' @importFrom grDevices densCols
#' @importFrom viridis viridis
compute_density_colors <- function(data, x, y, colorblind_friendly = TRUE) {
  dens_values <- grDevices::densCols(
    x = data[[x]],
    y = data[[y]],
    colramp = if (colorblind_friendly)
      colorRampPalette(viridis::viridis(256, direction = -1))
    else
      colorRampPalette(rev(rainbow(10, end = 4/6)))
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
#'
#' @return A data frame with an additional `tooltip` column.
#' @examples
#' generate_tooltip(data, x = "b", y = "pvalue")
#'
#' @export
#' @importFrom dplyr mutate
#' @importFrom glue glue
#' @importFrom scales scientific
#' @importFrom stringr str_wrap
generate_tooltip_voe <- function(data, x, y, k_col = "k", set_col = "set") {
  data %>%
    dplyr::mutate(
      tooltip = glue::glue(
        "<b>Effect Size (d):</b> {round(.data[[x]], 3)}<br>",  # This will always show "Effect Size (d)"
        "<b>P-value:</b> {scales::scientific(.data[[y]], digits = 8)}<br>",
        "<b>Number of Studies:</b> {get(k_col)}<br>",
        "<b>Study Set:</b> {stringr::str_wrap(get(set_col), width = 30)}"
      )
    )
}
