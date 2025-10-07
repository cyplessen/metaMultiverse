# ==============================================================================
# UI_MODULES/LANDING_PAGE.R - Cosmic Minimalism
# ==============================================================================

#' Landing Page UI Module
landing_page_ui <- function() {
  tagList(
    # Hero Section with starfield animation
    tags$div(class = "landing-wrapper",
             tags$div(class = "landing-content",
                      div(class = "hero-content",
                          h1(class = "hero-title",
                             "Meta", tags$span(style = "display: block;", "Multiverse")
                          ),
                          p("Systematic exploration of analytical decisions in meta-analysis. Transparent, comprehensive, and credible.",
                            class = "hero-subtitle"),
                          actionButton("start_analysis", "Begin Analysis",
                                       class = "hero-cta")
                      )
             )
    )
  )
}
