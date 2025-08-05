# ==============================================================================
# UTILS/RESULTS_HELPERS.R
# ==============================================================================

#' Get No Results UI
get_no_results_ui <- function() {
  wellPanel(
    h4("No Results Yet"),
    p("Please complete the configuration step and run the analysis to see results."),
    actionButton("goto_step3_from_results", "← Back to Configuration", class = "btn-default")
  )
}

#' Get Results Content UI
get_results_content_ui <- function() {
  tagList(
    # Results Summary
    h4("Analysis Summary"),
    uiOutput("results_summary"),

    br(),

    # Results Tabs
    tabsetPanel(
      tabPanel("Specification Curve",
               br(),
               p("The specification curve shows effect sizes across all analysis specifications, ranked from smallest to largest."),
               plotlyOutput("spec_curve", height = "600px")
      ),

      tabPanel("Vibration of Effects",
               br(),
               p("Explore the relationship between effect sizes and p-values across specifications."),

               fluidRow(
                 column(3,
                        wellPanel(
                          h5("Plot Settings"),
                          numericInput("voe_cutoff", "Minimum studies:", value = 10, min = 1, max = 50),
                          numericInput("voe_hline", "P-value threshold:", value = 0.05, min = 0.001, max = 1, step = 0.01),
                          checkboxInput("voe_colorblind", "Colorblind friendly", value = TRUE),
                          actionButton("update_voe", "Update Plot", class = "btn-primary")
                        )
                 ),
                 column(9,
                        plotlyOutput("voe_plot", height = "500px")
                 )
               )
      ),

      tabPanel("Results Table",
               br(),
               p("Detailed results for all analysis specifications."),
               DT::dataTableOutput("results_table")
      ),

      tabPanel("Analysis Warnings",
               br(),
               p("Any warnings or issues encountered during the analysis."),
               verbatimTextOutput("warnings_output")
      )
    )
  )
}
