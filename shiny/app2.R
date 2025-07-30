library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(purrr)
# library(metaMultiverse)  # Uncomment if package is installed

# Load package functions if available, otherwise use placeholder functions
# devtools::load_all("..")  # Remove this line - causes the error

# Add placeholder functions if metaMultiverse package functions aren't available
if (!exists("create_principled_multiverse_specifications")) {
  create_principled_multiverse_specifications <- function(data, wf_vars, ma_methods, dependencies, decision_map) {
    # Placeholder function - replace with actual implementation
    warning("Using placeholder function. Please ensure metaMultiverse package is loaded.")

    # Create simple specifications for demonstration
    specs <- expand.grid(
      ma_method = ma_methods,
      dependency = dependencies,
      stringsAsFactors = FALSE
    )
    specs$multiverse_id <- 1:nrow(specs)

    return(list(
      specifications = specs,
      number_specs = nrow(specs)
    ))
  }
}

if (!exists("run_multiverse_analysis")) {
  run_multiverse_analysis <- function(data, specifications, verbose = TRUE) {
    # Placeholder function - replace with actual implementation
    warning("Using placeholder function. Please ensure metaMultiverse package is loaded.")

    # Create dummy results for demonstration
    results <- data.frame(
      multiverse_id = specifications$multiverse_id,
      ma_method = specifications$ma_method,
      dependency = specifications$dependency,
      b = rnorm(nrow(specifications), 0.3, 0.1),
      ci.lb = rnorm(nrow(specifications), 0.1, 0.05),
      ci.ub = rnorm(nrow(specifications), 0.5, 0.05),
      pval = runif(nrow(specifications), 0.001, 0.8),
      k = sample(5:20, nrow(specifications), replace = TRUE),
      stringsAsFactors = FALSE
    )

    return(list(
      results = results,
      multiverse_warnings = c("This is a placeholder analysis. Please load the metaMultiverse package for real results.")
    ))
  }
}

if (!exists("plotly_descriptive_spec_curve")) {
  plotly_descriptive_spec_curve <- function(data, factor_label_lookup, colorblind_friendly = TRUE, interactive = TRUE) {
    # Placeholder plotting function
    data$x_rank <- rank(data$b)

    plot_ly(data, x = ~x_rank, y = ~b, type = 'scatter', mode = 'lines+markers',
            text = ~paste("Method:", ma_method, "<br>Effect:", sprintf("%.2f", b),
                          "<br>p-value:", sprintf("%.3f", pval)),
            hovertemplate = "%{text}<extra></extra>") %>%
      layout(title = "Specification Curve (Placeholder)",
             xaxis = list(title = "Specification Rank"),
             yaxis = list(title = "Effect Size"))
  }
}

if (!exists("plotly_VoE")) {
  plotly_VoE <- function(data, x, y, colorblind_friendly = TRUE, cutoff = 10, hline_value = 0.05, interactive = TRUE) {
    # Placeholder VoE plotting function
    data <- data[data$k >= cutoff, ]

    plot_ly(data, x = ~get(x), y = ~get(y), type = 'scatter', mode = 'markers',
            text = ~paste("Method:", ma_method, "<br>Effect:", sprintf("%.2f", b),
                          "<br>p-value:", sprintf("%.3f", pval), "<br>Studies:", k),
            hovertemplate = "%{text}<extra></extra>") %>%
      add_lines(x = range(data[[x]]), y = hline_value,
                line = list(dash = "dash", color = "red"),
                showlegend = FALSE, hoverinfo = "none") %>%
      layout(title = paste("Vibration of Effects (", nrow(data), " analyses)"),
             xaxis = list(title = "Effect Size"),
             yaxis = list(title = "P-value", type = "log"))
  }
}

# Load example data if available
if (!exists("data_tiny")) {
  data_tiny <- NULL  # Will trigger fallback data creation
}
#' Check Data for Multiverse Analysis
check_data_multiverse <- function(data) {
  # Required columns
  required_columns <- c(
    "study", "es_id", "yi", "vi"
  )
  # Dynamically detect wf columns (e.g., wf_1, wf_2, ...)
  wf_columns <- grep("^wf_", colnames(data), value = TRUE)
  # Combine required and dynamically detected wf columns
  all_required_columns <- c(required_columns, wf_columns)

  # Check if all required columns are present
  missing_columns <- setdiff(all_required_columns, colnames(data))
  if (length(missing_columns) > 0) {
    stop(paste("Missing required columns:", paste(missing_columns, collapse = ", ")))
  }

  # Check data types with detailed error messages
  expected_types <- list(
    study = "character",
    es_id = "numeric",
    yi = "numeric",
    vi = "numeric"
  )

  type_errors <- c()
  for (col in names(expected_types)) {
    if (col %in% names(data)) {
      actual_type <- class(data[[col]])[1]
      expected_type <- expected_types[[col]]

      if (expected_type == "character" && !is.character(data[[col]])) {
        type_errors <- c(type_errors, sprintf("Column '%s' is %s but should be character", col, actual_type))
      } else if (expected_type == "numeric" && !is.numeric(data[[col]])) {
        type_errors <- c(type_errors, sprintf("Column '%s' is %s but should be numeric", col, actual_type))
      }
    }
  }

  if (length(type_errors) > 0) {
    stop(paste("Data type errors found:\n", paste(type_errors, collapse = "\n")))
  }

  # Check unique es_id
  if (anyDuplicated(data$es_id)) {
    stop("Duplicate values found in 'es_id'. Each effect size must have a unique identifier.")
  }

  # Dynamic validation for wf variables
  wf_errors <- c()
  for (wf_col in wf_columns) {
    if (!is.character(data[[wf_col]])) {
      actual_type <- class(data[[wf_col]])[1]
      wf_errors <- c(wf_errors, sprintf("Column '%s' is %s but should be character", wf_col, actual_type))
    }
  }

  if (length(wf_errors) > 0) {
    stop(paste("Which factor column errors:\n", paste(wf_errors, collapse = "\n")))
  }

  # Check for missing values (warning, not error)
  missing_values <- colSums(is.na(data[all_required_columns]))
  if (any(missing_values > 0)) {
    warning_msg <- paste("Missing values found in required columns:\n",
                         paste(names(missing_values[missing_values > 0]),
                               missing_values[missing_values > 0],
                               sep = ": ", collapse = "\n"))
    warning(warning_msg)
  }

  # Success message if all checks pass
  message("Data validation passed. Dataset is ready for multiverse analysis.")
  return(TRUE)
}

# Define UI as a wizard with steps
ui <- fluidPage(
  titlePanel("Multiverse Meta-Analysis Wizard"),

  # Hidden navigation menu (collapsible)
  conditionalPanel(
    condition = "output.show_nav == true",
    wellPanel(
      h4("Navigation"),
      actionButton("goto_step1", "1. Landing", class = "btn-link"),
      actionButton("goto_step2", "2. Upload Data", class = "btn-link"),
      actionButton("goto_step3", "3. Configure Analysis", class = "btn-link"),
      actionButton("goto_step4", "4. Results", class = "btn-link"),
      actionButton("goto_about", "About", class = "btn-link"),
      hr()
    )
  ),

  # Navigation toggle
  div(style = "position: fixed; top: 10px; right: 10px; z-index: 1000;",
      actionButton("toggle_nav", "☰ Menu", class = "btn-sm")),

  # Main content area - shows different steps
  uiOutput("main_content"),

  # Navigation buttons at bottom
  br(),
  fluidRow(
    column(6, uiOutput("prev_button")),
    column(6, div(style = "text-align: right;", uiOutput("next_button")))
  )
)

server <- function(input, output, session) {

  # Reactive values
  values <- reactiveValues(
    current_step = 1,
    data = NULL,
    specifications = NULL,
    results = NULL,
    show_nav = FALSE,
    wf_config = list(),  # Store which factor configuration
    data_validation_status = NULL,
    data_validation_messages = NULL
  )

  # Navigation toggle
  observeEvent(input$toggle_nav, {
    values$show_nav <- !values$show_nav
  })

  # Output for navigation visibility
  output$show_nav <- reactive({ values$show_nav })
  outputOptions(output, "show_nav", suspendWhenHidden = FALSE)

  # Navigation buttons
  observeEvent(input$goto_step1, { values$current_step <- 1 })
  observeEvent(input$goto_step2, { values$current_step <- 2 })
  observeEvent(input$goto_step3, { values$current_step <- 3 })
  observeEvent(input$goto_step4, { values$current_step <- 4 })
  observeEvent(input$goto_about, { values$current_step <- 5 })

  # Step navigation
  observeEvent(input$next_step, {
    if (values$current_step < 4) {
      values$current_step <- values$current_step + 1
    }
  })

  observeEvent(input$prev_step, {
    if (values$current_step > 1) {
      values$current_step <- values$current_step - 1
    }
  })

  # Main content renderer
  output$main_content <- renderUI({
    switch(values$current_step,
           "1" = step1_landing_ui(),
           "2" = step2_upload_ui(),
           "3" = step3_config_ui(),
           "4" = step4_results_ui(),
           "5" = about_ui()
    )
  })

  # Navigation buttons
  output$prev_button <- renderUI({
    if (values$current_step > 1 && values$current_step <= 4) {
      actionButton("prev_step", "← Previous", class = "btn-default")
    }
  })

  output$next_button <- renderUI({
    switch(values$current_step,
           "1" = actionButton("next_step", "Get Started →", class = "btn-primary"),
           "2" = if (!is.null(values$data)) {
             actionButton("next_step", "Configure Analysis →", class = "btn-primary")
           } else {
             tags$span("Upload data to continue", style = "color: #999;")
           },
           "3" = if (!is.null(values$specifications)) {
             actionButton("next_step", "View Results →", class = "btn-primary")
           } else {
             tags$span("Create specifications to continue", style = "color: #999;")
           },
           "4" = NULL
    )
  })

  # STEP 1: Landing Page
  step1_landing_ui <- function() {
    div(
      h1("Welcome to Multiverse Meta-Analysis"),
      br(),

      fluidRow(
        column(8,
               h3("What is Multiverse Meta-Analysis?"),
               p("Multiverse meta-analysis systematically explores how different analytical
                 decisions affect your meta-analytic results, providing a more comprehensive
                 view of the evidence."),

               h4("Why Use This Approach?"),
               tags$ul(
                 tags$li("Transparency: Shows how robust your findings are to different choices"),
                 tags$li("Comprehensiveness: Explores multiple valid analytical paths"),
                 tags$li("Credibility: Addresses concerns about researcher degrees of freedom")
               ),

               h4("What You'll Need:"),
               tags$ul(
                 tags$li("A CSV file with effect sizes (yi), variances (vi), study IDs, and effect IDs"),
                 tags$li("Optional: columns representing different analytical choices (e.g., populations, measures)")
               ),

               br(),
               h4("This wizard will guide you through:"),
               tags$ol(
                 tags$li(tags$strong("Data Upload"), " - Load your meta-analysis dataset"),
                 tags$li(tags$strong("Configuration"), " - Set up your multiverse specifications"),
                 tags$li(tags$strong("Analysis"), " - Run the multiverse analysis"),
                 tags$li(tags$strong("Results"), " - Explore specification curves and vibration plots")
               )
        ),
        column(4,
               wellPanel(
                 h4("Quick Start"),
                 p("Ready to begin? Click 'Get Started' to load your data."),
                 br(),
                 p(tags$strong("Need example data?"), " The app will load sample data
                   automatically to help you explore the features."),
                 br(),
                 p(tags$em("Estimated time: 5-10 minutes"))
               )
        )
      )
    )
  }

  # STEP 2: Upload Data
  step2_upload_ui <- function() {
    div(
      h2("Step 1: Upload Your Data"),
      br(),

      fluidRow(
        column(6,
               wellPanel(
                 h4("Upload CSV File"),
                 fileInput("file", "Choose CSV File", accept = c(".csv")),

                 h5("Required Columns:"),
                 tags$ul(
                   tags$li(tags$strong("study"), " - Study identifier"),
                   tags$li(tags$strong("es_id"), " - Effect size identifier"),
                   tags$li(tags$strong("yi"), " - Effect size"),
                   tags$li(tags$strong("vi"), " - Variance")
                 ),

                 h5("Optional Columns:"),
                 p("Any columns starting with 'wf_' or that represent different
                   analytical choices (populations, measures, etc.)")
               )
        ),
        column(6,
               conditionalPanel(
                 condition = "output.data_uploaded",
                 wellPanel(
                   h4("✓ Data Loaded Successfully"),
                   p(tags$strong("Rows:"), textOutput("n_rows", inline = TRUE)),
                   p(tags$strong("Studies:"), textOutput("n_studies", inline = TRUE)),
                   p(tags$strong("Effect Sizes:"), textOutput("n_effects", inline = TRUE)),
                   p(tags$strong("Potential Which Factors:"), textOutput("n_wf_factors", inline = TRUE)),

                   br(),
                   h5("Data Validation:"),
                   uiOutput("data_validation_status")
                 )
               )
        )
      ),

      conditionalPanel(
        condition = "output.data_uploaded",
        h4("Data Preview:"),
        DT::dataTableOutput("data_preview_step2", height = "400px")
      )
    )
  }

  # STEP 3: Configuration
  step3_config_ui <- function() {
    div(
      h2("Step 2: Configure Your Multiverse Analysis"),

      tabsetPanel(
        id = "config_tabs",

        # Which Factors Tab
        tabPanel("Which Factors",
                 br(),
                 h4("Configure 'Which' Factors"),
                 p("Select which columns represent different analytical choices and
                   classify them according to their theoretical relationship."),

                 conditionalPanel(
                   condition = "output.data_uploaded",

                   # Decision Framework Guide
                   wellPanel(
                     h5("Decision Framework Guide"),
                     tags$ul(
                       tags$li(tags$strong("Equivalent (E):"), " Different ways of measuring the same construct"),
                       tags$li(tags$strong("Non-equivalent (N):"), " Fundamentally different approaches (creates separate analyses)"),
                       tags$li(tags$strong("Uncertain (U):"), " Unclear equivalence (treated as Equivalent)"),
                       tags$li(tags$strong("Ignore:"), " Exclude from multiverse entirely")
                     )
                   ),

                   # Which factors configuration
                   uiOutput("wf_config_step3")
                 )
        ),

        # Methods Tab
        tabPanel("Meta-Analysis Methods",
                 br(),
                 h4("Select Meta-Analysis Estimators"),
                 p("Choose which meta-analytic methods to include in your multiverse."),

                 wellPanel(
                   h5("Basic Methods"),
                   fluidRow(
                     column(6, checkboxInput("method_reml", "Random Effects (REML)", value = TRUE)),
                     column(6, checkboxInput("method_fe", "Fixed Effects"))
                   ),

                   h5("Dependency Handling"),
                   fluidRow(
                     column(6, checkboxInput("method_rve", "Robust Variance Estimation", value = TRUE)),
                     column(6, checkboxInput("method_3_level", "3-Level Meta-Analysis"))
                   ),

                   h5("Publication Bias Correction"),
                   fluidRow(
                     column(4, checkboxInput("method_pet_peese_corr", "PET-PEESE (corrected)", value = TRUE)),
                     column(4, checkboxInput("method_puni_star", "p-uniform*", value = TRUE)),
                     column(4, checkboxInput("method_pet_peese", "PET-PEESE"))
                   ),
                   fluidRow(
                     column(4, checkboxInput("method_uwls", "UWLS", value = TRUE)),
                     column(4, checkboxInput("method_waap", "WAAP")),
                     column(4, checkboxInput("method_bayesmeta", "Bayesian Meta-Analysis"))
                   )
                 )
        ),

        # Dependencies Tab
        tabPanel("Dependency Handling",
                 br(),
                 h4("Handle Study Dependencies"),
                 p("Choose how to handle cases where studies contribute multiple effect sizes."),

                 wellPanel(
                   checkboxGroupInput("dependencies", "Dependency Methods:",
                                      choices = list(
                                        "Aggregate within studies" = "aggregate",
                                        "Select maximum effect" = "select_max",
                                        "Select minimum effect" = "select_min",
                                        "Model dependencies" = "modeled"
                                      ),
                                      selected = "aggregate"),

                   br(),
                   numericInput("k_smallest", "Minimum number of studies:",
                                value = 5, min = 1, max = 20, step = 1),
                   helpText("Analyses with fewer studies will be excluded")
                 )
        ),

        # Create Specifications Tab
        tabPanel("Create Specifications",
                 br(),
                 h4("Generate Analysis Specifications"),

                 wellPanel(
                   p("Review your configuration and create the multiverse specifications."),

                   br(),
                   actionButton("create_specs", "Create Specifications",
                                class = "btn-primary btn-lg"),

                   br(), br(),

                   conditionalPanel(
                     condition = "output.specs_created",
                     div(
                       h5("✓ Specifications Created"),
                       verbatimTextOutput("specs_summary_step3"),

                       br(),
                       actionButton("run_analysis", "Run Multiverse Analysis",
                                    class = "btn-success btn-lg"),

                       br(),
                       conditionalPanel(
                         condition = "output.analysis_running",
                         div(
                           br(),
                           tags$p("⏳ Analysis running... This may take several minutes."),
                           tags$div(class = "progress progress-striped active",
                                    tags$div(class = "progress-bar", style = "width: 100%"))
                         )
                       )
                     )
                   )
                 )
        )
      )
    )
  }

  # STEP 4: Results
  step4_results_ui <- function() {
    div(
      h2("Step 3: Multiverse Analysis Results"),

      conditionalPanel(
        condition = "!output.analysis_complete",
        wellPanel(
          h4("No Results Yet"),
          p("Please complete the configuration step and run the analysis to see results."),
          actionButton("goto_step3_from_results", "← Back to Configuration", class = "btn-default")
        )
      ),

      conditionalPanel(
        condition = "output.analysis_complete",

        # Results Summary
        h4("Analysis Summary"),
        uiOutput("results_summary_step4"),

        br(),

        # Results Tabs
        tabsetPanel(
          tabPanel("Specification Curve",
                   br(),
                   p("The specification curve shows effect sizes across all analysis specifications,
                     ranked from smallest to largest."),
                   plotlyOutput("spec_curve_step4", height = "600px")
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
                            plotlyOutput("voe_plot_step4", height = "500px")
                     )
                   )
          ),

          tabPanel("Results Table",
                   br(),
                   p("Detailed results for all analysis specifications."),
                   DT::dataTableOutput("results_table_step4")
          ),

          tabPanel("Analysis Warnings",
                   br(),
                   p("Any warnings or issues encountered during the analysis."),
                   verbatimTextOutput("warnings_output_step4")
          )
        )
      )
    )
  }

  # About Page
  about_ui <- function() {
    div(
      h2("About Multiverse Meta-Analysis"),

      fluidRow(
        column(8,
               h3("Theoretical Background"),

               h4("The Multiverse Approach"),
               p("Multiverse analysis explores how different analytical decisions affect research conclusions.
                 Rather than making a single set of choices, researchers systematically examine all
                 reasonable combinations of decisions."),

               h4("Decision Framework"),
               p("This app implements the framework by Del Giudice & Gangestad (2021), which categorizes
                 analytical decisions into:"),

               tags$ul(
                 tags$li(tags$strong("Non-equivalent (N):"), " Decisions representing fundamentally different
                         theoretical approaches. These create separate 'multiverses' because combining them
                         would be inappropriate."),
                 tags$li(tags$strong("Equivalent (E):"), " Decisions that are theoretically interchangeable
                         and represent different ways of operationalizing the same construct."),
                 tags$li(tags$strong("Uncertain (U):"), " Decisions where equivalence status is unclear.
                         Treated conservatively as equivalent.")
               ),

               h4("Meta-Analysis Methods"),
               p("The app includes various meta-analytic estimators:"),
               tags$ul(
                 tags$li("Basic methods: Fixed effects, Random effects (REML)"),
                 tags$li("Dependency handling: 3-level models, Robust variance estimation"),
                 tags$li("Publication bias correction: PET-PEESE, p-uniform*, UWLS, WAAP")
               ),

               h4("References"),
               tags$ul(
                 tags$li("Del Giudice, M., & Gangestad, S. W. (2021). A traveler's guide to the multiverse:
                         Promises, pitfalls, and a framework for the evaluation of analytic decisions.
                         Advances in Methods and Practices in Psychological Science, 4(1)."),
                 tags$li("Voracek, M., et al. (2019). Which data to meta-analyze, and how? A specification-curve
                         and multiverse-analysis approach to meta-analysis. Zeitschrift für Psychologie, 227(1), 64-82.")
               )
        ),

        column(4,
               wellPanel(
                 h4("Package Information"),
                 p("This app is built using the", code("metaMultiverse"), "R package."),

                 h5("Key Features:"),
                 tags$ul(
                   tags$li("Principled specification creation"),
                   tags$li("Multiple meta-analytic estimators"),
                   tags$li("Interactive visualization"),
                   tags$li("Comprehensive result reporting")
                 ),

                 br(),
                 h5("Getting Help:"),
                 p("For questions about the methodology or technical issues, please refer to the package documentation.")
               )
        )
      )
    )
  }

  # Auto-load example data
  observe({
    if (is.null(values$data)) {
      tryCatch({
        if (exists("data_tiny")) {
          values$data <- data_tiny

          # Validate example data
          validation_messages <- capture.output({
            validation_result <- check_data_multiverse(values$data)
          }, type = "message")

          values$data_validation_status <- "success"
          values$data_validation_messages <- validation_messages

          showNotification("Example dataset loaded and validated!", type = "message", duration = 3)
        }
      }, error = function(e) {
        # Create fallback data
        values$data <- data.frame(
          study = rep(paste0("Study_", 1:10), each = 2),
          es_id = 1:20,
          yi = rnorm(20, mean = 0.3, sd = 0.2),
          vi = runif(20, min = 0.01, max = 0.1),
          wf_population = rep(c("adults", "children"), times = 10),
          wf_measure = rep(c("self_report", "behavioral"), each = 10),
          stringsAsFactors = FALSE
        )

        # Validate fallback data
        tryCatch({
          validation_messages <- capture.output({
            validation_result <- check_data_multiverse(values$data)
          }, type = "message")

          values$data_validation_status <- "success"
          values$data_validation_messages <- validation_messages

        }, error = function(e2) {
          values$data_validation_status <- "error"
          values$data_validation_messages <- e2$message
        })

        showNotification("Example dataset created!", type = "message", duration = 3)
      })
    }
  })

  # Data upload handling
  observeEvent(input$file, {
    req(input$file)

    # Reset validation status
    values$data_validation_status <- NULL
    values$data_validation_messages <- NULL

    tryCatch({
      # Load the data
      values$data <- read.csv(input$file$datapath, stringsAsFactors = FALSE)

      # Run data validation
      validation_messages <- capture.output({
        validation_result <- check_data_multiverse(values$data)
      }, type = "message")

      # Store validation results
      values$data_validation_status <- "success"
      values$data_validation_messages <- validation_messages

      showNotification("Data uploaded and validated successfully!", type = "message", duration = 3)

    }, error = function(e) {
      # If validation fails, keep the data but show error
      values$data_validation_status <- "error"
      values$data_validation_messages <- e$message

      showNotification(paste("Data validation failed:", e$message),
                       type = "error", duration = 8)

      # Check if it's just missing required columns vs other errors
      required_cols <- c("study", "es_id", "yi", "vi")
      if (!all(required_cols %in% names(values$data))) {
        values$data <- NULL  # Don't keep invalid data
      }
    }, warning = function(w) {
      # If there are warnings (like missing values), show them but keep data
      values$data_validation_status <- "warning"
      values$data_validation_messages <- w$message

      showNotification(paste("Data loaded with warnings:", w$message),
                       type = "warning", duration = 5)
    })
  })

  # Data status outputs
  output$data_uploaded <- reactive({ !is.null(values$data) })
  outputOptions(output, "data_uploaded", suspendWhenHidden = FALSE)

  output$n_rows <- renderText({ if (!is.null(values$data)) nrow(values$data) else "0" })
  output$n_studies <- renderText({
    if (!is.null(values$data)) length(unique(values$data$study)) else "0"
  })
  output$n_effects <- renderText({
    if (!is.null(values$data)) length(unique(values$data$es_id)) else "0"
  })
  output$n_wf_factors <- renderText({
    if (!is.null(values$data)) {
      required_cols <- c("study", "es_id", "yi", "vi")
      potential_wf_cols <- names(values$data)[!names(values$data) %in% required_cols]
      length(potential_wf_cols)
    } else "0"
  })

  # Data validation status display
  output$data_validation_status <- renderUI({
    req(values$data_validation_status)

    if (values$data_validation_status == "success") {
      div(
        style = "color: #28a745; border: 1px solid #28a745; padding: 10px; border-radius: 5px; background-color: #f8fff9;",
        tags$strong("✓ Validation Passed"),
        if (!is.null(values$data_validation_messages) && length(values$data_validation_messages) > 0) {
          div(style = "margin-top: 5px; font-size: 0.9em;",
              paste(values$data_validation_messages, collapse = " "))
        }
      )
    } else if (values$data_validation_status == "warning") {
      div(
        style = "color: #856404; border: 1px solid #ffc107; padding: 10px; border-radius: 5px; background-color: #fff3cd;",
        tags$strong("⚠ Validation Warnings"),
        if (!is.null(values$data_validation_messages)) {
          div(style = "margin-top: 5px; font-size: 0.9em;",
              values$data_validation_messages)
        }
      )
    } else if (values$data_validation_status == "error") {
      div(
        style = "color: #721c24; border: 1px solid #dc3545; padding: 10px; border-radius: 5px; background-color: #f8d7da;",
        tags$strong("✗ Validation Failed"),
        if (!is.null(values$data_validation_messages)) {
          div(style = "margin-top: 5px; font-size: 0.9em;",
              values$data_validation_messages)
        }
      )
    }
  })

  # Data preview
  output$data_preview_step2 <- DT::renderDataTable({
    req(values$data)

    # Create the datatable
    dt <- DT::datatable(head(values$data, 100),
                        options = list(scrollX = TRUE, pageLength = 10))

    # Apply formatting to numeric columns that are likely to need it
    numeric_cols <- names(values$data)[sapply(values$data, is.numeric)]

    # Format common meta-analysis columns with various possible names
    effect_size_cols <- intersect(numeric_cols, c("yi", "d", "r", "es", "effect_size", "effect"))
    variance_cols <- intersect(numeric_cols, c("vi", "var", "variance", "var_yi"))
    se_cols <- intersect(numeric_cols, c("sei", "se", "standard_error", "se_yi"))

    # Apply appropriate formatting
    if (length(effect_size_cols) > 0) {
      dt <- dt %>% DT::formatRound(effect_size_cols, 2)
    }
    if (length(variance_cols) > 0) {
      dt <- dt %>% DT::formatRound(variance_cols, 4)  # Variances need more precision
    }
    if (length(se_cols) > 0) {
      dt <- dt %>% DT::formatRound(se_cols, 2)
    }

    # Format any other numeric columns (excluding IDs and already formatted columns)
    formatted_cols <- c(effect_size_cols, variance_cols, se_cols)
    id_cols <- c("study", "es_id", "id", "study_id", "effect_id")
    other_numeric <- numeric_cols[!numeric_cols %in% c(formatted_cols, id_cols)]

    if (length(other_numeric) > 0) {
      dt <- dt %>% DT::formatRound(other_numeric, 2)
    }

    return(dt)
  })

  # Which factors configuration for step 3
  output$wf_config_step3 <- renderUI({
    req(values$data)

    required_cols <- c("study", "es_id", "yi", "vi")
    all_other_cols <- names(values$data)[!names(values$data) %in% required_cols]
    potential_wf_cols <- all_other_cols[!sapply(all_other_cols, function(col) is.numeric(values$data[[col]]))]

    if (length(potential_wf_cols) == 0) {
      return(div(
        class = "alert alert-info",
        h4("No Which Factors Available"),
        p("Your data only contains required columns. Multiverse will vary only across meta-analysis methods.")
      ))
    }

    div(
      h5("Available Columns for Which Factors:"),

      lapply(seq_along(potential_wf_cols), function(i) {
        col_name <- potential_wf_cols[i]
        unique_vals <- unique(values$data[[col_name]])

        wellPanel(
          fluidRow(
            column(4,
                   h6(tags$strong(col_name)),
                   p(paste("Values:", paste(head(unique_vals, 3), collapse = ", "),
                           if(length(unique_vals) > 3) "..." else ""))
            ),
            column(8,
                   radioButtons(paste0("decision_", col_name),
                                "Decision Type:",
                                choices = list(
                                  "Equivalent (E)" = "E",
                                  "Non-equivalent (N)" = "N",
                                  "Uncertain (U)" = "U",
                                  "Ignore" = "IGNORE"
                                ),
                                selected = "IGNORE",
                                inline = TRUE)
            )
          )
        )
      })
    )
  })

  # Get selected methods
  selected_methods <- reactive({
    methods <- character(0)
    if (input$method_fe) methods <- c(methods, "fe")
    if (input$method_reml) methods <- c(methods, "reml")
    if (input$method_3_level) methods <- c(methods, "3-level")
    if (input$method_rve) methods <- c(methods, "rve")
    if (input$method_pet_peese) methods <- c(methods, "pet.peese")
    if (input$method_pet_peese_corr) methods <- c(methods, "pet.peese.corrected")
    if (input$method_puni_star) methods <- c(methods, "puni_star")
    if (input$method_uwls) methods <- c(methods, "uwls")
    if (input$method_waap) methods <- c(methods, "waap")
    if (input$method_bayesmeta) methods <- c(methods, "bayesmeta")
    return(methods)
  })

  # Create specifications
  observeEvent(input$create_specs, {
    req(values$data, input$dependencies)

    # Get which factors configuration
    required_cols <- c("study", "es_id", "yi", "vi")
    potential_wf_cols <- names(values$data)[!names(values$data) %in% required_cols]

    # Build decision map and wf_vars
    decision_map <- character(0)
    wf_vars <- character(0)

    for (col_name in potential_wf_cols) {
      decision_input_name <- paste0("decision_", col_name)
      if (!is.null(input[[decision_input_name]])) {
        decision <- input[[decision_input_name]]
        if (decision != "IGNORE") {
          # Create wf_ column name
          wf_name <- paste0("wf_", col_name)
          values$data[[wf_name]] <- values$data[[col_name]]
          decision_map[wf_name] <- decision
          wf_vars <- c(wf_vars, wf_name)
        }
      }
    }

    tryCatch({
      specs_result <- create_principled_multiverse_specifications(
        data = values$data,
        wf_vars = wf_vars,
        ma_methods = selected_methods(),
        dependencies = input$dependencies,
        decision_map = decision_map
      )

      values$specifications <- specs_result$specifications
      showNotification(paste("Created", specs_result$number_specs, "specifications"),
                       type = "message", duration = 3)

    }, error = function(e) {
      showNotification(paste("Error creating specifications:", e$message),
                       type = "error", duration = 5)
    })
  })

  # Specs created status
  output$specs_created <- reactive({ !is.null(values$specifications) })
  outputOptions(output, "specs_created", suspendWhenHidden = FALSE)

  # Specs summary
  output$specs_summary_step3 <- renderText({
    req(values$specifications)
    paste("Total specifications:", nrow(values$specifications),
          "\nUnique multiverses:", length(unique(values$specifications$multiverse_id)),
          "\nMA methods:", paste(unique(values$specifications$ma_method), collapse = ", "))
  })

  # Analysis running status
  values$analysis_running <- FALSE
  output$analysis_running <- reactive({ values$analysis_running })
  outputOptions(output, "analysis_running", suspendWhenHidden = FALSE)

  # Run analysis


  # Run analysis
  observeEvent(input$run_analysis, {
    req(values$data, values$specifications, input$k_smallest)

    values$analysis_running <- TRUE
    options(metaMultiverse.k_smallest_ma = input$k_smallest)

    withProgress(message = 'Running Multiverse Analysis', value = 0, {

      incProgress(0.1, detail = "Starting analysis...")
      Sys.sleep(0.5)

      tryCatch({
        values$results <- run_multiverse_analysis(
          data = values$data,
          specifications = values$specifications,
          verbose = TRUE
        )

        incProgress(0.7, detail = "Analysis complete!")
        values$analysis_running <- FALSE

      }, error = function(e) {
        values$analysis_running <- FALSE
        showNotification(paste("Error running analysis:", e$message), type = "error", duration = 5)
      })

      incProgress(0.2, detail = "Done!")

    })

    showNotification("Analysis completed!", type = "message", duration = 3)
  })

  # Analysis complete status
  output$analysis_complete <- reactive({
    !is.null(values$results) && !is.null(values$results$results)
  })
  outputOptions(output, "analysis_complete", suspendWhenHidden = FALSE)

  # Navigate from results back to config
  observeEvent(input$goto_step3_from_results, { values$current_step <- 3 })

  # Results summary for step 4
  output$results_summary_step4 <- renderUI({
    req(values$results)

    if (is.null(values$results$results) || nrow(values$results$results) == 0) {
      return(div(class = "alert alert-warning", "No valid results generated."))
    }

    data <- values$results$results

    fluidRow(
      column(3,
             wellPanel(
               h5("Total Specifications"),
               h3(nrow(data), style = "margin: 0; color: #337ab7;"),
               p("analysis conducted")
             )
      ),
      column(3,
             wellPanel(
               h5("Mean Effect Size"),
               h3(sprintf("%.2f", mean(data$b, na.rm = TRUE)), style = "margin: 0; color: #337ab7;"),
               p(paste("Range:", sprintf("%.2f", min(data$b, na.rm = TRUE)), "to", sprintf("%.2f", max(data$b, na.rm = TRUE))))
             )
      ),
      column(3,
             wellPanel(
               h5("Significant Results"),
               h3(paste0(sprintf("%.1f", 100 * mean(data$pval < 0.05, na.rm = TRUE)), "%"),
                  style = "margin: 0; color: #337ab7;"),
               p(paste(sum(data$pval < 0.05, na.rm = TRUE), "of", nrow(data), "specs"))
             )
      ),
      column(3,
             wellPanel(
               h5("Average Studies"),
               h3(sprintf("%.1f", mean(data$k, na.rm = TRUE)), style = "margin: 0; color: #337ab7;"),
               p("per analysis")
             )
      )
    )
  })

  # Plots and tables for step 4
  output$spec_curve_step4 <- renderPlotly({
    req(values$results)

    if (is.null(values$results$results) || nrow(values$results$results) == 0) {
      return(plotly_empty() %>% add_annotations(text = "No results to display", showarrow = FALSE))
    }

    tryCatch({
      wf_cols <- names(values$results$results)[grepl("^wf_", names(values$results$results))]
      factor_label_lookup <- setNames(paste("Factor", seq_along(wf_cols)), wf_cols)
      factor_label_lookup[["ma_method"]] <- "Meta-Analysis Method"
      factor_label_lookup[["dependency"]] <- "Dependency Handling"

      plotly_descriptive_spec_curve(
        data = values$results$results,
        factor_label_lookup = factor_label_lookup,
        colorblind_friendly = TRUE,
        interactive = TRUE
      )

    }, error = function(e) {
      # Fallback plot
      data <- values$results$results
      data$x_rank <- rank(data$b)

      plot_ly(data, x = ~x_rank, y = ~b, type = 'scatter', mode = 'lines+markers',
              error_y = list(array = ~(ci.ub - b), arrayminus = ~(b - ci.lb)),
              text = ~paste("Method:", ma_method, "<br>Effect:", sprintf("%.2f", b),
                            "<br>p-value:", sprintf("%.3f", pval)),
              hovertemplate = "%{text}<extra></extra>") %>%
        layout(title = "Specification Curve",
               xaxis = list(title = "Specification Rank"),
               yaxis = list(title = "Effect Size"))
    })
  })

  output$voe_plot_step4 <- renderPlotly({
    req(values$results)

    if (is.null(values$results$results) || nrow(values$results$results) == 0) {
      return(plotly_empty() %>% add_annotations(text = "No results to display", showarrow = FALSE))
    }

    tryCatch({
      plotly_VoE(
        data = values$results$results,
        x = "b",
        y = "pval",
        colorblind_friendly = input$voe_colorblind,
        cutoff = input$voe_cutoff,
        hline_value = input$voe_hline,
        interactive = TRUE
      )

    }, error = function(e) {
      # Fallback plot
      data <- values$results$results
      data <- data[data$k >= input$voe_cutoff, ]

      if (nrow(data) == 0) {
        return(plotly_empty() %>% add_annotations(
          text = paste("No results with ≥", input$voe_cutoff, "studies"),
          showarrow = FALSE))
      }

      plot_ly(data, x = ~b, y = ~pval, type = 'scatter', mode = 'markers',
              text = ~paste("Method:", ma_method, "<br>Effect:", sprintf("%.2f", b),
                            "<br>p-value:", sprintf("%.3f", pval), "<br>Studies:", k),
              hovertemplate = "%{text}<extra></extra>") %>%
        add_lines(x = range(data$b), y = input$voe_hline,
                  line = list(dash = "dash", color = "red"),
                  showlegend = FALSE, hoverinfo = "none") %>%
        layout(title = paste("Vibration of Effects (", nrow(data), " analyses)"),
               xaxis = list(title = "Effect Size (b)"),
               yaxis = list(title = "P-value", type = "log"))
    })
  })

  # Update VoE plot when settings change
  observeEvent(input$update_voe, {
    output$voe_plot_step4 <- renderPlotly({
      req(values$results)

      if (is.null(values$results$results) || nrow(values$results$results) == 0) {
        return(plotly_empty() %>% add_annotations(text = "No results to display", showarrow = FALSE))
      }

      tryCatch({
        plotly_VoE(
          data = values$results$results,
          x = "b",
          y = "pval",
          colorblind_friendly = input$voe_colorblind,
          cutoff = input$voe_cutoff,
          hline_value = input$voe_hline,
          interactive = TRUE
        )

      }, error = function(e) {
        data <- values$results$results
        data <- data[data$k >= input$voe_cutoff, ]

        if (nrow(data) == 0) {
          return(plotly_empty() %>% add_annotations(
            text = paste("No results with ≥", input$voe_cutoff, "studies"),
            showarrow = FALSE))
        }

        plot_ly(data, x = ~b, y = ~pval, type = 'scatter', mode = 'markers',
                text = ~paste("Method:", ma_method, "<br>Effect:", round(b, 3),
                              "<br>p-value:", round(pval, 3), "<br>Studies:", k),
                hovertemplate = "%{text}<extra></extra>") %>%
          add_lines(x = range(data$b), y = input$voe_hline,
                    line = list(dash = "dash", color = "red"),
                    showlegend = FALSE, hoverinfo = "none") %>%
          layout(title = paste("Vibration of Effects (", nrow(data), " analyses)"),
                 xaxis = list(title = "Effect Size (b)"),
                 yaxis = list(title = "P-value", type = "log"))
      })
    })
  })

  output$results_table_step4 <- DT::renderDataTable({
    req(values$results)

    if (is.null(values$results$results) || nrow(values$results$results) == 0) {
      return(DT::datatable(data.frame(Message = "No results to display")))
    }

    DT::datatable(values$results$results,
                  options = list(scrollX = TRUE, pageLength = 25)) %>%
      DT::formatRound(c("b", "ci.lb", "ci.ub"), 2) %>%
      DT::formatRound("pval", 3)
  })

  output$warnings_output_step4 <- renderText({
    req(values$results)
    if (length(values$results$multiverse_warnings) > 0) {
      paste(values$results$multiverse_warnings, collapse = "\n")
    } else {
      "No warnings generated."
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)
