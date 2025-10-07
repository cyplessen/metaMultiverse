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

# Source utilities FIRST (UI/server modules depend on them)
source("utils/css_styles.R")
source("utils/navigation_helpers.R")
source("utils/validation_helpers.R")
source("utils/results_helpers.R")
source("utils/theoretical_background.R")

# Then source UI modules
source("ui_modules/landing_page.R")
source("ui_modules/data_upload.R")
source("ui_modules/factor_config.R")
source("ui_modules/results_display.R")
source("ui_modules/about_page.R")

# Finally source server modules
source("server_modules/data_handler.R")
source("server_modules/factor_config_server.R")
source("server_modules/analysis_runner.R")
source("server_modules/results_server.R")

# Main UI
ui <- fluidPage(
  # Load CSS styles
  get_app_css(),

  # Conditional layout: full-screen landing or sidebar layout
  uiOutput("app_layout")
)

# Main Server
server <- function(input, output, session) {

  # Initialize reactive values
  values <- reactiveValues(
    current_step = 1,
    data = NULL,
    data_validation_status = NULL,
    data_validation_messages = NULL,
    factor_setup = NULL,           # Store define_factors() output
    spec_output = NULL,            # Store create_multiverse_specifications() output (v0.2.0)
    results = NULL,
    show_nav = FALSE,
    analysis_running = FALSE       # Track analysis status
  )

  # Initialize modular servers (SAME ORDER IS IMPORTANT)
  data_upload_server(input, output, session, values)
  factor_config_server(input, output, session, values)
  analysis_runner_server(input, output, session, values)
  results_server(input, output, session, values)

  # Handle navigation
  setup_navigation_server(input, output, session, values)

  # App Layout (landing vs sidebar layout)
  output$app_layout <- renderUI({
    if (values$current_step == 1) {
      # Full-screen landing page
      landing_page_ui()
    } else {
      # Sidebar layout for steps 2-5
      tagList(
        div(class = "app-sidebar",
            div(class = "app-logo",
                tags$span("metaMultiverse")
            ),
            div(class = "progress-timeline",
                # Step 1: Upload Data
                div(class = if(values$current_step == 2) "timeline-step active" else if(values$current_step > 2) "timeline-step" else "timeline-step",
                    div(class = if(values$current_step == 2) "step-indicator active" else if(values$current_step > 2) "step-indicator completed" else "step-indicator",
                        if(values$current_step > 2) "✓" else "1"
                    ),
                    div(class = "step-content",
                        div(class = "step-title", "Upload Data"),
                        div(class = "step-description", "Import your meta-analysis dataset")
                    )
                ),
                # Step 2: Configure
                div(class = if(values$current_step == 3) "timeline-step active" else if(values$current_step > 3) "timeline-step" else "timeline-step",
                    div(class = if(values$current_step == 3) "step-indicator active" else if(values$current_step > 3) "step-indicator completed" else "step-indicator",
                        if(values$current_step > 3) "✓" else "2"
                    ),
                    div(class = "step-content",
                        div(class = "step-title", "Configure"),
                        div(class = "step-description", "Define factors and methods")
                    )
                ),
                # Step 3: Results
                div(class = if(values$current_step == 4) "timeline-step active" else "timeline-step",
                    div(class = if(values$current_step == 4) "step-indicator active" else "step-indicator",
                        "3"
                    ),
                    div(class = "step-content",
                        div(class = "step-title", "Results"),
                        div(class = "step-description", "Explore your multiverse")
                    )
                )
            ),
            tags$hr(style = "border-color: rgba(255,255,255,0.1); margin: 32px 0;"),
            actionButton("goto_step1", "← Home", class = "btn-ghost", style = "width: 100%; color: rgba(255,255,255,0.6); border-color: rgba(255,255,255,0.1);"),
            actionButton("goto_about", "About", class = "btn-ghost", style = "width: 100%; margin-top: 8px; color: rgba(255,255,255,0.6); border-color: rgba(255,255,255,0.1);")
        ),
        div(class = "app-main",
            uiOutput("main_content")
        )
      )
    }
  })

  # Main content routing
  output$main_content <- renderUI({
    switch(as.character(values$current_step),
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
