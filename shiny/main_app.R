# ==============================================================================
# MAIN APP.R
# ==============================================================================

# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(purrr)
library(metaMultiverse)

# Source all modules
source("ui_modules/landing_page.R")
source("ui_modules/data_upload.R")
source("ui_modules/factor_config.R")
source("ui_modules/results_display.R")
source("ui_modules/about_page.R")

source("server_modules/data_handler.R")
source("server_modules/factor_config_server.R")
source("server_modules/analysis_runner.R")
source("server_modules/results_server.R")

source("utils/css_styles.R")
source("utils/navigation_helpers.R")
source("utils/validation_helpers.R")

# Main UI
ui <- fluidPage(
  # Load CSS styles
  get_app_css(),

  # Navigation components
  get_navigation_ui(),

  # Main content area
  uiOutput("main_content"),

  # Bottom navigation buttons
  br(),
  fluidRow(
    column(6, uiOutput("prev_button")),
    column(6, div(style = "text-align: right;", uiOutput("next_button")))
  )
)

# Main Server
server <- function(input, output, session) {

  # Initialize reactive values
  values <- reactiveValues(
    current_step = 1,
    data = NULL,
    data_validation_status = NULL,
    data_validation_messages = NULL,
    factor_setup = NULL,           # NEW: Store define_factors() setup
    factor_decisions = NULL,       # NEW: Store factor decisions
    custom_groups = NULL,          # NEW: Store custom groupings
    specifications = NULL,
    results = NULL,
    show_nav = FALSE,
    analysis_running = FALSE       # NEW: Track analysis status
  )

  # Initialize modular servers (SAME ORDER IS IMPORTANT)
  data_upload_server(input, output, session, values)
  factor_config_server(input, output, session, values)
  analysis_runner_server(input, output, session, values)
  results_server(input, output, session, values)

  # Handle navigation
  setup_navigation_server(input, output, session, values)

  # Main content routing
  output$main_content <- renderUI({
    switch(as.character(values$current_step),
           "1" = landing_page_ui(),
           "2" = data_upload_ui(),
           "3" = factor_config_ui(),
           "4" = results_display_ui(),
           "5" = about_page_ui()
    )
  })
  #  debugging:
  observe({
    cat("DEBUG: Data changed. Rows:", if (!is.null(values$data)) nrow(values$data) else "NULL", "\n")
    cat("DEBUG: Data columns:", if (!is.null(values$data)) paste(names(values$data), collapse = ", ") else "NULL", "\n")
  })

  observe({
    if (!is.null(values$data)) {
      cat("MAIN SERVER: Data exists -", nrow(values$data), "rows\n")
    } else {
      cat("MAIN SERVER: Data is NULL\n")
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)
