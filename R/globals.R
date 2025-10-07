# ABOUTME: This file declares imports from base R packages to avoid R CMD check NOTEs
# ABOUTME: about undefined global functions and NSE variables

#' @importFrom stats as.formula complete.cases density median sd setNames
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom ggplot2 aes aes_string geom_violin geom_boxplot geom_hline geom_histogram geom_vline labs theme_minimal theme element_text ggsave ggplot
#' @importFrom plotly plot_ly add_markers add_lines layout
NULL

# Declare global variables used in tidyverse/data.table style programming
# to avoid "no visible binding" NOTEs
utils::globalVariables(c(
  # dplyr/tidyverse NSE variables used across multiple functions
  "study", "k", "mod", "dependency", "ci_width", "fill_manual", "tooltip",
  "group_id", "multiverse_id", "q10", "q90", "density",
  # data.table operator
  ":="
))
