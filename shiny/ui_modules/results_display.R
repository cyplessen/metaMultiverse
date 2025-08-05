# ==============================================================================
# UI_MODULES/RESULTS_DISPLAY.R
# ==============================================================================

#' Results Display UI Module
results_display_ui <- function() {
  div(
    h2("Step 3: Multiverse Analysis Results"),

    conditionalPanel(
      condition = "!output.analysis_complete",
      get_no_results_ui()
    ),

    conditionalPanel(
      condition = "output.analysis_complete",
      get_results_content_ui()
    )
  )
}
