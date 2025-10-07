# ABOUTME: This file declares imports from base R packages to avoid R CMD check NOTEs
# ABOUTME: about undefined global functions

#' @importFrom stats as.formula complete.cases density median sd setNames
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom graphics layout
NULL

# Declare global variables used in tidyverse/data.table style programming
# to avoid "no visible binding" NOTEs
utils::globalVariables(c(
  # dplyr/tidyverse NSE variables
  "study", "k", "mod", "dependency", "ci_width", "fill_manual", "tooltip",
  "group_id", "multiverse_id", "q10", "q90", "density",
  # data.table operator
  ":="
))
