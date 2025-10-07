# ==============================================================================
# UI_MODULES/DATA_UPLOAD.R - Cosmic Minimalism
# ==============================================================================

#' Data Upload UI Module
data_upload_ui <- function() {
  tagList(
    h1("Upload Data", class = "mb-lg"),
    p("Import your meta-analysis dataset. We'll validate the structure and prepare it for multiverse analysis.",
      class = "text-muted mb-xl"),

    # Upload Zone
    div(class = "card",
        div(class = "upload-zone",
            div(class = "upload-icon", "📊"),
            div(class = "upload-text", "Drop your CSV file here"),
            div(class = "upload-subtext mb-md", "or click to browse"),
            fileInput("file", "",
                      accept = c(".csv"),
                      width = "300px")
        )
    ),

    # Requirements Card
    div(class = "card mt-lg",
        div(class = "card-header",
            h4(class = "card-title mb-0", "Required Columns")
        ),
        div(class = "card-body",
            div(class = "d-flex gap-md mb-md",
                div(style = "flex: 1;",
                    div(class = "form-label", "study"),
                    p(class = "text-muted mb-0", "Unique study identifier")
                ),
                div(style = "flex: 1;",
                    div(class = "form-label", "es_id"),
                    p(class = "text-muted mb-0", "Effect size ID")
                )
            ),
            div(class = "d-flex gap-md",
                div(style = "flex: 1;",
                    div(class = "form-label", "yi"),
                    p(class = "text-muted mb-0", "Effect size value")
                ),
                div(style = "flex: 1;",
                    div(class = "form-label", "vi"),
                    p(class = "text-muted mb-0", "Effect size variance")
                )
            ),
            tags$hr(style = "margin: 24px 0; border-color: rgba(0,0,0,0.06);"),
            div(class = "alert-info",
                tags$strong("Optional: "),
                "Additional columns will be detected as potential analytical factors."
            )
        )
    ),

    # Data Status
    conditionalPanel(
      condition = "output.data_uploaded",
      div(class = "stat-grid mt-xl",
          div(class = "stat-card",
              div(class = "stat-value", textOutput("n_rows")),
              div(class = "stat-label", "Effect Sizes")
          ),
          div(class = "stat-card",
              div(class = "stat-value", textOutput("n_studies")),
              div(class = "stat-label", "Studies")
          ),
          div(class = "stat-card",
              div(class = "stat-value", textOutput("n_effects")),
              div(class = "stat-label", "Unique Effects")
          ),
          div(class = "stat-card",
              div(class = "stat-value", textOutput("n_potential_factors")),
              div(class = "stat-label", "Factors Detected")
          )
      )
    ),

    # Validation Status
    conditionalPanel(
      condition = "output.data_uploaded",
      uiOutput("data_validation_status")
    ),

    # Data Preview
    conditionalPanel(
      condition = "output.data_uploaded",
      div(class = "card mt-lg",
          div(class = "card-header",
              h4(class = "card-title mb-0", "Data Preview")
          ),
          div(class = "card-body",
              div(class = "table-wrapper",
                  DT::dataTableOutput("data_preview_step2")
              )
          )
      )
    ),

    # Next Button
    conditionalPanel(
      condition = "output.data_uploaded",
      div(class = "text-right mt-xl",
          actionButton("next_step", "Configure Analysis →", class = "btn-primary")
      )
    )
  )
}
