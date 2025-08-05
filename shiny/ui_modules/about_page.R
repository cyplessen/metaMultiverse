# ==============================================================================
# UI_MODULES/ABOUT_PAGE.R
# ==============================================================================

#' About Page UI Module
about_page_ui <- function() {
  div(
    h2("About Multiverse Meta-Analysis"),

    fluidRow(
      column(8,
             get_theoretical_background_ui()
      ),
      column(4,
             get_package_info_ui()
      )
    )
  )
}
