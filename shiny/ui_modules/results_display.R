# ==============================================================================
# UI_MODULES/RESULTS_DISPLAY.R - Cosmic Minimalism
# ==============================================================================

#' Results Display UI Module
results_display_ui <- function() {
  tagList(
    conditionalPanel(
      condition = "!output.analysis_complete",
      div(class = "card",
          div(class = "card-body text-center",
              style = "padding: 96px 32px;",
              h3("No Results Yet", class = "mb-md"),
              p("Configure your analysis and run the multiverse to see results here.", class = "text-muted mb-lg"),
              actionButton("goto_step3", "← Back to Configuration", class = "btn-secondary")
          )
      )
    ),

    conditionalPanel(
      condition = "output.analysis_complete",
      tagList(
        h1("Multiverse Results", class = "mb-md"),
        p("Explore how different analytical decisions affect your meta-analytic conclusions.",
          class = "text-muted mb-xl"),

        # Summary Stats
        uiOutput("results_summary"),

        # Tabs for different visualizations - using Shiny tabsetPanel
        tabsetPanel(
          id = "results_tabs",
          type = "pills",

          tabPanel(
            "Specification Curve",
            value = "spec_curve",
            div(class = "card mt-lg",
                div(class = "card-body",
                    p("Effect sizes across all specifications, ranked from smallest to largest.",
                      class = "text-muted mb-lg"),
                    plotlyOutput("spec_curve", height = "600px")
                )
            )
          ),

          tabPanel(
            "Vibration of Effects",
            value = "voe",
            div(class = "card mt-lg",
                div(class = "card-body",
                    p("Relationship between effect sizes and p-values across the multiverse.",
                      class = "text-muted mb-lg"),
                    plotlyOutput("voe_plot", height = "600px")
                )
            )
          ),

          tabPanel(
            "Results Table",
            value = "table",
            div(class = "card mt-lg",
                div(class = "card-header",
                    h4(class = "card-title mb-0", "Results Table")
                ),
                div(class = "card-body",
                    DT::dataTableOutput("results_table")
                )
            )
          ),

          tabPanel(
            "Warnings",
            value = "warnings",
            div(class = "card mt-lg",
                div(class = "card-header",
                    h4(class = "card-title mb-0", "Analysis Warnings")
                ),
                div(class = "card-body",
                    verbatimTextOutput("warnings_output")
                )
            )
          )
        )
      )
    )
  )
}
