# ==============================================================================
# UI_MODULES/DATA_UPLOAD.R
# ==============================================================================

#' Data Upload UI Module
data_upload_ui <- function() {
  div(style = "min-height: 100vh; background: linear-gradient(to bottom, #f8f9fa, #ffffff); padding: 80px 0;",
      div(style = "max-width: 1200px; margin: 0 auto; padding: 0 40px;",

          # Header section
          div(style = "text-align: center; margin-bottom: 4rem;",
              h1("Step 1: Upload Your Data",
                 style = "font-size: 3rem; font-weight: 300; color: #2c3e50; margin-bottom: 1.5rem;"),
              p("Upload your meta-analysis dataset and we'll validate it for you. Our system will check your data structure, identify potential analytical factors, and guide you through any necessary adjustments.",
                style = "font-size: 1.3rem; color: #6c757d; line-height: 1.6; max-width: 700px; margin: 0 auto;")
          ),

          # Upload section
          div(style = "background: white; border-radius: 20px; padding: 3rem; box-shadow: 0 10px 40px rgba(0,0,0,0.08); margin-bottom: 3rem;",

              fluidRow(
                # Left side - Requirements
                column(6,
                       h3("Data Requirements",
                          style = "color: #2c3e50; margin-bottom: 2rem; font-weight: 600;"),
                       get_data_requirements_ui()
                ),

                # Right side - Upload
                column(6,
                       h3("Choose Your Dataset",
                          style = "color: #2c3e50; margin-bottom: 2rem; font-weight: 600;"),
                       get_file_upload_ui()
                )
              )
          ),

          # Status panel - only show when data is uploaded
          conditionalPanel(
            condition = "output.data_uploaded",
            get_data_status_ui()
          ),

          # Data preview - minimalist
          conditionalPanel(
            condition = "output.data_uploaded",
            get_data_preview_ui()
          )
      )
  )
}

# ==============================================================================
# UI HELPER FUNCTIONS
# ==============================================================================

#' Get Data Requirements UI
get_data_requirements_ui <- function() {
  div(style = "background: #f8f9fa; padding: 2rem; border-radius: 12px; border-left: 4px solid #667eea;",
      div(style = "margin-bottom: 1.5rem;",
          h5(tags$strong("study"), style = "color: #667eea; margin-bottom: 0.5rem;"),
          p("Unique identifier for each study (e.g., 'Smith2020', 'Jones2019')",
            style = "color: #6c757d; margin: 0; font-size: 0.9rem;")
      ),
      div(style = "margin-bottom: 1.5rem;",
          h5(tags$strong("es_id"), style = "color: #667eea; margin-bottom: 0.5rem;"),
          p("Unique identifier for each effect size within studies",
            style = "color: #6c757d; margin: 0; font-size: 0.9rem;")
      ),
      div(style = "margin-bottom: 1.5rem;",
          h5(tags$strong("yi"), style = "color: #667eea; margin-bottom: 0.5rem;"),
          p("The effect size values (Cohen's d, correlation coefficients, etc.)",
            style = "color: #6c757d; margin: 0; font-size: 0.9rem;")
      ),
      div(
        h5(tags$strong("vi"), style = "color: #667eea; margin-bottom: 0.5rem;"),
        p("Variance of each effect size (squared standard errors)",
          style = "color: #6c757d; margin: 0; font-size: 0.9rem;")
      ),

      div(style = "margin-top: 1.5rem; padding-top: 1.5rem; border-top: 1px solid #dee2e6;",
          p(tags$strong("Optional:"), " Any additional columns that represent different analytical choices (populations, measures, etc.) will be automatically detected.",
            style = "color: #6c757d; font-size: 0.85rem; font-style: italic;")
      )
  )
}

#' Get File Upload UI
get_file_upload_ui <- function() {
  div(style = "border: 2px dashed #dee2e6; border-radius: 12px; padding: 3rem; text-align: center; background: #fafafa;",
      div(style = "margin-bottom: 1.5rem;",
          tags$i(class = "fa fa-upload", style = "font-size: 3rem; color: #667eea; margin-bottom: 1rem;"),
          p("Upload your CSV file", style = "color: #6c757d; margin: 0;")
      ),
      fileInput("file", "",
                accept = c(".csv"),
                buttonLabel = "Browse Files",
                placeholder = "Select a CSV file...")
  )
}

#' Get Data Status UI
get_data_status_ui <- function() {
  div(style = "background: white; border-radius: 20px; padding: 2.5rem; box-shadow: 0 10px 40px rgba(0,0,0,0.08); margin-bottom: 3rem;",

      div(style = "text-align: center; margin-bottom: 2rem;",
          h3("✓ Data Successfully Loaded",
             style = "color: #28a745; margin-bottom: 1rem; font-weight: 600;")
      ),

      # Data summary in a clean grid
      fluidRow(
        column(3, div(style = "text-align: center; padding: 1rem;",
                      h4(textOutput("n_rows", inline = TRUE),
                         style = "color: #667eea; margin-bottom: 0.5rem;"),
                      p("Rows", style = "color: #6c757d; margin: 0; font-size: 0.9rem;")
        )),
        column(3, div(style = "text-align: center; padding: 1rem;",
                      h4(textOutput("n_studies", inline = TRUE),
                         style = "color: #667eea; margin-bottom: 0.5rem;"),
                      p("Studies", style = "color: #6c757d; margin: 0; font-size: 0.9rem;")
        )),
        column(3, div(style = "text-align: center; padding: 1rem;",
                      h4(textOutput("n_effects", inline = TRUE),
                         style = "color: #667eea; margin-bottom: 0.5rem;"),
                      p("Effect Sizes", style = "color: #6c757d; margin: 0; font-size: 0.9rem;")
        )),
        column(3, div(style = "text-align: center; padding: 1rem;",
                      h4(textOutput("n_potential_factors", inline = TRUE),
                         style = "color: #667eea; margin-bottom: 0.5rem;"),
                      p("Potential Factors", style = "color: #6c757d; margin: 0; font-size: 0.9rem;")
        ))
      ),

      # Validation status
      div(style = "margin-top: 1.5rem;",
          uiOutput("data_validation_status")
      ),

      # Configure Analysis Button
      div(style = "text-align: center; margin-top: 3rem; padding-top: 2rem; border-top: 1px solid #dee2e6;",
          h4("Ready to Configure Your Analysis?",
             style = "color: #2c3e50; margin-bottom: 1.5rem; font-weight: 300;"),
          div(style = "background: linear-gradient(135deg, #667eea, #764ba2); padding: 3px; border-radius: 50px; display: inline-block; box-shadow: 0 10px 30px rgba(102, 126, 234, 0.3);",
              tags$button("Configure Analysis →",
                          id = "next_step",
                          class = "btn",
                          onclick = "Shiny.setInputValue('next_step', Math.random());",
                          style = "background: white; color: #667eea; border: none; padding: 18px 40px; border-radius: 50px; font-size: 1.3rem; font-weight: 600; cursor: pointer; transition: all 0.3s ease; text-transform: uppercase; letter-spacing: 1px; margin: 0;")
          )
      )
  )
}

#' Get Data Preview UI
get_data_preview_ui <- function() {
  div(style = "margin-top: 2rem;",
      tags$details(
        tags$summary("View Data Preview",
                     style = "font-size: 1.1rem; color: #667eea; cursor: pointer; margin-bottom: 1rem;"),
        div(style = "background: white; border-radius: 12px; padding: 2rem; box-shadow: 0 5px 20px rgba(0,0,0,0.05); margin-top: 1rem;",
            DT::dataTableOutput("data_preview_step2", height = "400px")
        )
      )
  )
}
