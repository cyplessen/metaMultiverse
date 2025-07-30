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
  conditionalPanel(
    condition = "false",  # Never show title on landing page
    titlePanel("Multiverse Meta-Analysis Wizard")
  ),

  # Add CSS
  tags$head(
    tags$style(HTML("
    /* Hide Shiny's default title and eliminate all white space */
    .container-fluid > h1:first-child {
      display: none !important;
    }
    .container-fluid {
      padding: 0 !important;
      margin: 0 !important;
    }

    body {
      margin: 0 !important;
      padding: 0 !important;
    }

    .col-sm-12 {
      padding: 0 !important;
    }

    .tab-content {
      margin: 0 !important;
      padding: 0 !important;
    }

    /* Landing wrapper and layered approach */
    .landing-wrapper {
      position: relative;
      width: 100vw;
      height: 100vh;
      margin-left: calc(-50vw + 50%);
      overflow: hidden;
    }

    .landing-block {
      position: absolute;
      top: 0;
      left: 0;
      width: 100%;
      height: 100%;
    }

    .background-content {
      z-index: 1;
    }

    .background-content img {
      width: 100%;
      height: 100%;
      object-fit: cover;
      object-position: center;
    }

    .foreground-content {
      z-index: 2;
      display: flex;
      align-items: center;
      justify-content: center;
      background: rgba(0, 0, 0, 0.3);
      color: white;
      flex-direction: column;
    }

    /* Hero content styling */
    .hero-content {
      text-align: center;
      max-width: 900px;
      padding: 0 40px;
    }

    .hero-title {
      font-size: clamp(3rem, 8vw, 5rem);
      font-weight: 700;
      margin-bottom: 2rem;
      text-shadow: 2px 2px 20px rgba(0,0,0,0.5);
      color: white;
      letter-spacing: -0.02em;
      line-height: 1.1;
    }

    .hero-subtitle {
      font-size: clamp(1.2rem, 3vw, 1.8rem);
      margin-bottom: 3rem;
      text-shadow: 1px 1px 10px rgba(0,0,0,0.5);
      line-height: 1.4;
      opacity: 0.95;
      font-weight: 300;
      color: white;
    }

    .hero-cta {
      padding: 18px 40px;
      font-size: 1.2rem;
      font-weight: 600;
      background: rgba(255, 255, 255, 0.15);
      color: white;
      border: 2px solid rgba(255, 255, 255, 0.3);
      border-radius: 50px;
      backdrop-filter: blur(10px);
      transition: all 0.3s ease;
      cursor: pointer;
      text-transform: uppercase;
      letter-spacing: 1px;
    }

    .hero-cta:hover {
      background: rgba(255, 255, 255, 0.25);
      border-color: rgba(255, 255, 255, 0.6);
      transform: translateY(-3px);
      box-shadow: 0 15px 35px rgba(0,0,0,0.2);
    }

    /* Scroll indicator */
    .scroll-indicator {
      position: absolute;
      bottom: 40px;
      left: 50%;
      transform: translateX(-50%);
      color: rgba(255,255,255,0.8);
      text-align: center;
      z-index: 3;
    }

    .scroll-text {
      font-size: 0.9rem;
      margin-bottom: 15px;
      letter-spacing: 3px;
      font-weight: 300;
    }

    .scroll-arrow {
      font-size: 2rem;
      animation: bounce 2s infinite;
      display: block;
    }

    @keyframes bounce {
      0%, 20%, 50%, 80%, 100% { transform: translateY(0); }
      40% { transform: translateY(-15px); }
      60% { transform: translateY(-8px); }
    }

    /* Content section styling */
    .content-section {
      background: linear-gradient(to bottom, #f8f9fa, #ffffff);
      padding: 100px 0;
      position: relative;
    }

    .content-container {
      max-width: 1200px;
      margin: 0 auto;
      padding: 0 20px;
    }

    .section-title {
      text-align: center;
      font-size: clamp(2.5rem, 5vw, 3.5rem);
      font-weight: 700;
      margin-bottom: 4rem;
      color: #2c3e50;
      position: relative;
    }

    .section-title::after {
      content: '';
      position: absolute;
      bottom: -15px;
      left: 50%;
      transform: translateX(-50%);
      width: 80px;
      height: 4px;
      background: linear-gradient(135deg, #667eea, #764ba2);
      border-radius: 2px;
    }

    /* Feature cards */
    .feature-card {
      background: white;
      padding: 3rem 2rem;
      margin: 20px;
      border-radius: 20px;
      box-shadow: 0 10px 40px rgba(0,0,0,0.1);
      text-align: center;
      transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
      border: 1px solid rgba(255,255,255,0.2);
      position: relative;
      overflow: hidden;
    }

    .feature-card::before {
      content: '';
      position: absolute;
      top: 0;
      left: 0;
      right: 0;
      height: 4px;
      background: linear-gradient(135deg, #667eea, #764ba2);
    }

    .feature-card:hover {
      transform: translateY(-10px);
      box-shadow: 0 20px 60px rgba(0,0,0,0.15);
    }

    .feature-icon {
      font-size: 4rem;
      margin-bottom: 1.5rem;
      background: linear-gradient(135deg, #667eea, #764ba2);
      -webkit-background-clip: text;
      -webkit-text-fill-color: transparent;
      background-clip: text;
    }

    .feature-title {
      font-size: 1.5rem;
      font-weight: 600;
      color: #2c3e50;
      margin-bottom: 1rem;
    }

    .feature-description {
      color: #6c757d;
      line-height: 1.6;
      font-size: 1.1rem;
    }

    /* Responsive adjustments */
    @media (max-width: 768px) {
      .hero-content {
        padding: 0 20px;
      }

      .feature-card {
        margin: 10px 5px;
        padding: 2rem 1.5rem;
      }

      .content-section {
        padding: 60px 0;
      }
    }

    /* Smooth scrolling */
    html {
      scroll-behavior: smooth;
    }
    /* Modern which factors styling */
    .wf-factor-grid {
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
      gap: 1.5rem;
      margin-top: 1.5rem;
    }

    .wf-factor-card {
      background: white;
      border: 1px solid #e9ecef;
      border-radius: 12px;
      padding: 1.5rem;
      transition: all 0.3s ease;
      box-shadow: 0 2px 8px rgba(0,0,0,0.04);
    }

    .wf-factor-card:hover {
      border-color: #667eea;
      box-shadow: 0 4px 20px rgba(102, 126, 234, 0.1);
    }

    .wf-factor-header {
      display: flex;
      justify-content: space-between;
      align-items: flex-start;
      margin-bottom: 1rem;
    }

    .wf-factor-name {
      font-weight: 600;
      color: #2c3e50;
      margin: 0;
      font-size: 1.1rem;
    }

    .wf-factor-values {
      color: #6c757d;
      font-size: 0.85rem;
      margin: 0;
      line-height: 1.4;
    }

    .wf-decision-btn {
      padding: 0.4rem 0.8rem;
      border: 1px solid #dee2e6;
      background: white;
      border-radius: 6px;
      font-size: 0.8rem;
      cursor: pointer;
      transition: all 0.2s ease;
      color: #6c757d;
      font-weight: 500;
    }

    .wf-decision-btn:hover {
      transform: translateY(-1px);
      box-shadow: 0 2px 8px rgba(0,0,0,0.1);
    }

    /* Equivalent - Blue */
    .wf-decision-btn.equivalent {
      border-color: #2196F3;
      color: #2196F3;
    }

    .wf-decision-btn.equivalent.active {
      background: #2196F3;
      border-color: #2196F3;
      color: white;
      box-shadow: 0 2px 12px rgba(33, 150, 243, 0.3);
    }

    /* Non-equivalent - Purple */
    .wf-decision-btn.non-equivalent {
      border-color: #9C27B0;
      color: #9C27B0;
    }

    .wf-decision-btn.non-equivalent.active {
      background: #9C27B0;
      border-color: #9C27B0;
      color: white;
      box-shadow: 0 2px 12px rgba(156, 39, 176, 0.3);
    }

    /* Uncertain - Orange */
    .wf-decision-btn.uncertain {
      border-color: #FF9800;
      color: #FF9800;
    }

    .wf-decision-btn.uncertain.active {
      background: #FF9800;
      border-color: #FF9800;
      color: white;
      box-shadow: 0 2px 12px rgba(255, 152, 0, 0.3);
    }

    /* Ignore - Grey */
    .wf-decision-btn.ignore {
      border-color: #6c757d;
      color: #6c757d;
    }

    .wf-decision-btn.ignore.active {
      background: #6c757d;
      border-color: #6c757d;
      color: white;
    }

    .menu-overlay {
      position: fixed;
      top: 0;
      right: -400px;
      width: 400px;
      height: 100%;
      background: rgba(0, 0, 0, 0.95);
      z-index: 9999;
      backdrop-filter: blur(10px);
      transition: right 0.3s ease;
      box-shadow: -5px 0 15px rgba(0, 0, 0, 0.3);
    }

    .menu-overlay.open {
      right: 0;
    }

    .menu-content {
      padding: 80px 40px 40px 40px;
      color: white;
    }

    .menu-close {
      position: absolute;
      top: 20px;
      right: 20px;
      font-size: 30px;
      color: white;
      cursor: pointer;
      background: none;
      border: none;
    }

    .menu-item {
      display: block;
      color: white;
      font-size: 18px;
      margin: 15px 0;
      padding: 12px 20px;
      border: none;
      background: none;
      cursor: pointer;
      border-radius: 8px;
      width: 100%;
      text-align: left;
    }

    .menu-item:hover {
      color: #667eea;
      background: rgba(255, 255, 255, 0.1);
    }

    .menu-toggle {
      position: fixed;
      top: 20px;
      right: 20px;
      z-index: 1000;
      background: rgba(0, 0, 0, 0.7);
      color: white;
      border: none;
      padding: 12px 16px;
      border-radius: 8px;
      cursor: pointer;
    }

    .menu-backdrop {
      position: fixed;
      top: 0;
      left: 0;
      width: 100%;
      height: 100%;
      background: rgba(0, 0, 0, 0.5);
      z-index: 9998;
      display: none;
    }

    @keyframes progress-animation {
      0% { width: 30%; }
      50% { width: 70%; }
      100% { width: 30%; }
    }
  "))
  ),

  # Menu backdrop (for clicking outside to close)
  div(id = "menu-backdrop", class = "menu-backdrop"),

  # Right-side menu overlay
  div(id = "menu-overlay", class = "menu-overlay",
      div(class = "menu-content",
          actionButton("menu-close", "×", class = "menu-close"),
          h3("Navigation", style = "color: #888; font-size: 14px; letter-spacing: 2px; margin-bottom: 30px;"),
          actionButton("goto_step1", "Home", class = "menu-item"),
          actionButton("goto_step2", "Upload Data", class = "menu-item"),
          actionButton("goto_step3", "Configure Analysis", class = "menu-item"),
          actionButton("goto_step4", "Results", class = "menu-item"),
          actionButton("goto_about", "About", class = "menu-item")
      )
  ),

  # Menu button
  actionButton("toggle_nav", "☰ MENU", class = "menu-toggle"),

  # JavaScript
  tags$script(HTML("
    $(document).ready(function() {
      $('#toggle_nav').click(function() {
        $('#menu-overlay').addClass('open');
        $('#menu-backdrop').fadeIn(200);
      });

      $('#menu-close, #menu-backdrop').click(function() {
        $('#menu-overlay').removeClass('open');
        $('#menu-backdrop').fadeOut(200);
      });

      $('.menu-item').click(function() {
        $('#menu-overlay').removeClass('open');
        $('#menu-backdrop').fadeOut(200);
      });
    });
  ")),

  # Main content
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
           "1" = NULL,  # No button on landing page
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
  # STEP 1: Landing Page
  step1_landing_ui <- function() {
    tagList(
      # Landing Section with layered approach
      tags$div(class = "landing-wrapper",

               # Background layer - image
               tags$div(class = "landing-block background-content",
                        tags$img(src = "pic1.jpg", alt = "Universe background")
               ),

               # Foreground layer - content
               tags$div(class = "landing-block foreground-content",
                        div(class = "hero-content",
                            h1("Multiverse Meta-Analysis", class = "hero-title"),
                            p("Explore how different analytical decisions affect your meta-analytic results. This systematic approach reveals the robustness of your findings across multiple valid analytical paths, addressing concerns about researcher degrees of freedom while promoting transparency and credibility in meta-analytic research.",
                              class = "hero-subtitle")
                        ),
                        div(class = "scroll-indicator",
                            div("DISCOVER MORE", class = "scroll-text"),
                            div("↓", class = "scroll-arrow")
                        )
               )
      ),

      # Content Section
      div(class = "content-section",
          div(class = "content-container",
              h2("Why Use This Approach?", class = "section-title"),

              fluidRow(
                column(4,
                       div(class = "feature-card",
                           div("🔍", class = "feature-icon"),
                           h4("Transparency", class = "feature-title"),
                           p("Traditional meta-analyses rely on single analytical choices that may influence results. Our multiverse approach systematically explores all reasonable analytical decisions, showing exactly how robust your findings are to different methodological choices. This transparency builds confidence in your conclusions and addresses potential criticisms about selective analytical decisions.",
                             class = "feature-description")
                       )
                ),
                column(4,
                       div(class = "feature-card",
                           div("📊", class = "feature-icon"),
                           h4("Comprehensive Analysis", class = "feature-title"),
                           p("Rather than choosing a single meta-analytic method, explore multiple estimators simultaneously - from basic fixed and random effects to advanced techniques like robust variance estimation, publication bias corrections, and dependency modeling. See how your conclusions hold up across different analytical frameworks and identify which methods are most appropriate for your data.",
                             class = "feature-description")
                       )
                ),
                column(4,
                       div(class = "feature-card",
                           div("🎯", class = "feature-icon"),
                           h4("Credible Results", class = "feature-title"),
                           p("Address the growing concern about researcher degrees of freedom in meta-analysis. By pre-specifying and systematically testing multiple analytical approaches, you demonstrate that your findings aren't dependent on arbitrary choices. This principled approach to analytical variability strengthens the credibility and reproducibility of your research.",
                             class = "feature-description")
                       )
                )
              ),

              # Call to action at the bottom
              div(style = "text-align: center; padding: 4rem 0 2rem 0;",
                  h3("Ready to explore your data's multiverse?",
                     style = "color: #2c3e50; font-weight: 300; font-size: 1.8rem; margin-bottom: 2rem;"),
                  actionButton("next_step", "Start Your Analysis",
                               class = "hero-cta",
                               style = "background: linear-gradient(135deg, #667eea, #764ba2); border: none; font-size: 1.3rem; padding: 20px 50px;")
              )
          )
      )
    )
  }


  # STEP 2: Upload Data
  # STEP 2: Upload Data
  step2_upload_ui <- function() {
    div(style = "min-height: 100vh; background: linear-gradient(to bottom, #f8f9fa, #ffffff); padding: 80px 0;",
        div(style = "max-width: 1200px; margin: 0 auto; padding: 0 40px;",

            # Header section
            div(style = "text-align: center; margin-bottom: 4rem;",
                h1("Step 1: Upload Your Data",
                   style = "font-size: 3rem; font-weight: 300; color: #2c3e50; margin-bottom: 1.5rem;"),
                p("Upload your meta-analysis dataset and we'll validate it for you. Our system will check your data structure, identify potential analytical factors, and guide you through any necessary adjustments.",
                  style = "font-size: 1.3rem; color: #6c757d; line-height: 1.6; max-width: 700px; margin: 0 auto;")
            ),

            # Upload section with side-by-side layout
            div(style = "background: white; border-radius: 20px; padding: 3rem; box-shadow: 0 10px 40px rgba(0,0,0,0.08); margin-bottom: 3rem;",

                fluidRow(
                  # Left side - Requirements
                  column(6,
                         h3("Data Requirements",
                            style = "color: #2c3e50; margin-bottom: 2rem; font-weight: 600;"),

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
                  ),

                  # Right side - Upload
                  column(6,
                         h3("Choose Your Dataset",
                            style = "color: #2c3e50; margin-bottom: 2rem; font-weight: 600;"),

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
                  )
                )
            ),

            # Status panel - only show when data is uploaded
            conditionalPanel(
              condition = "output.data_uploaded",
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
                                  h4(textOutput("n_wf_factors", inline = TRUE),
                                     style = "color: #667eea; margin-bottom: 0.5rem;"),
                                  p("Potential Factors", style = "color: #6c757d; margin: 0; font-size: 0.9rem;")
                    ))
                  ),

                  # Validation status
                  div(style = "margin-top: 1.5rem;",
                      uiOutput("data_validation_status")
                  ),

                  # PROMINENT Configure Analysis Button
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
            ),

            # Data preview - minimalist and less prominent
            conditionalPanel(
              condition = "output.data_uploaded",
              div(style = "margin-top: 2rem;",
                  tags$details(
                    tags$summary("View Data Preview",
                                 style = "font-size: 1.1rem; color: #667eea; cursor: pointer; margin-bottom: 1rem;"),
                    div(style = "background: white; border-radius: 12px; padding: 2rem; box-shadow: 0 5px 20px rgba(0,0,0,0.05); margin-top: 1rem;",
                        DT::dataTableOutput("data_preview_step2", height = "400px")
                    )
                  )
              )
            )
        )
    )
  }

  # STEP 3: Configuration
  # STEP 3: Configuration
  step3_config_ui <- function() {
    div(style = "min-height: 100vh; background: linear-gradient(to bottom, #f8f9fa, #ffffff); padding: 80px 0;",
        div(style = "max-width: 1200px; margin: 0 auto; padding: 0 40px;",

            # Header section
            div(style = "text-align: center; margin-bottom: 4rem;",
                h1("Step 2: Configure Your Analysis",
                   style = "font-size: 3rem; font-weight: 300; color: #2c3e50; margin-bottom: 1.5rem;"),
                p("Set up your multiverse analysis by defining which factors to explore and how to handle study dependencies. We'll guide you through each decision to create a comprehensive analytical framework.",
                  style = "font-size: 1.3rem; color: #6c757d; line-height: 1.6; max-width: 700px; margin: 0 auto;")
            ),

            conditionalPanel(
              condition = "output.data_uploaded",

              # Which Factors Section
              div(style = "background: white; border-radius: 20px; padding: 3rem; box-shadow: 0 10px 40px rgba(0,0,0,0.08); margin-bottom: 3rem;",
                  h3("Which Factors to Explore",
                     style = "color: #2c3e50; margin-bottom: 2rem; font-weight: 600;"),
                  p("Select which columns represent different analytical choices and classify them according to their theoretical relationship.",
                    style = "color: #6c757d; margin-bottom: 2rem;"),

                  # Decision Framework Guide
                  div(style = "background: #f8f9fa; padding: 2rem; border-radius: 12px; border-left: 4px solid #667eea; margin-bottom: 2rem;",
                      h5("Decision Framework Guide", style = "margin-bottom: 1.5rem; color: #2c3e50;"),
                      fluidRow(
                        column(6,
                               div(style = "margin-bottom: 1rem;",
                                   h6(tags$span("●", style = "color: #2196F3; margin-right: 0.5rem;"),
                                      tags$strong("Equivalent (E)"),
                                      style = "color: #2196F3; margin-bottom: 0.5rem;"),
                                   p("Different ways of measuring the same construct",
                                     style = "color: #6c757d; margin: 0; font-size: 0.9rem;")
                               ),
                               div(
                                 h6(tags$span("●", style = "color: #9C27B0; margin-right: 0.5rem;"),
                                    tags$strong("Non-equivalent (N)"),
                                    style = "color: #9C27B0; margin-bottom: 0.5rem;"),
                                 p("Fundamentally different approaches (creates separate analyses)",
                                   style = "color: #6c757d; margin: 0; font-size: 0.9rem;")
                               )
                        ),
                        column(6,
                               div(style = "margin-bottom: 1rem;",
                                   h6(tags$span("●", style = "color: #FF9800; margin-right: 0.5rem;"),
                                      tags$strong("Uncertain (U)"),
                                      style = "color: #FF9800; margin-bottom: 0.5rem;"),
                                   p("Unclear equivalence (treated as Equivalent)",
                                     style = "color: #6c757d; margin: 0; font-size: 0.9rem;")
                               ),
                               div(
                                 h6(tags$span("●", style = "color: #6c757d; margin-right: 0.5rem;"),
                                    tags$strong("Ignore"),
                                    style = "color: #6c757d; margin-bottom: 0.5rem;"),
                                 p("Exclude from multiverse entirely",
                                   style = "color: #6c757d; margin: 0; font-size: 0.9rem;")
                               )
                        )
                      )
                  ),

                  # Which factors configuration
                  uiOutput("wf_config_step3")
              ),

              # How Factors Section (combines MA methods and dependencies)
              div(style = "background: white; border-radius: 20px; padding: 3rem; box-shadow: 0 10px 40px rgba(0,0,0,0.08); margin-bottom: 3rem;",
                  h3("How to Handle Dependencies",
                     style = "color: #2c3e50; margin-bottom: 2rem; font-weight: 600;"),
                  p("Choose how to handle cases where studies contribute multiple effect sizes, then select meta-analytic methods.",
                    style = "color: #6c757d; margin-bottom: 2rem;"),

                  fluidRow(
                    # Left side - Dependency Strategy
                    column(6,
                           div(style = "background: #f8f9fa; padding: 2rem; border-radius: 12px; border-left: 4px solid #667eea;",
                               h5("Dependency Strategy", style = "margin-bottom: 1.5rem; color: #2c3e50;"),
                               p("Choose how to handle cases where studies contribute multiple effect sizes (select multiple options):",
                                 style = "color: #6c757d; font-size: 0.9rem; margin-bottom: 1rem;"),

                               checkboxGroupInput("dependency_strategy", "",
                                                  choices = list(
                                                    "Aggregate within studies" = "aggregate",
                                                    "Select maximum effect" = "select_max",
                                                    "Select minimum effect" = "select_min",
                                                    "Model dependencies" = "modeled"
                                                  ),
                                                  selected = "aggregate"),

                               # Advanced dependency methods (only show when "modeled" is selected)
                               conditionalPanel(
                                 condition = "input.dependency_strategy && input.dependency_strategy.indexOf('modeled') > -1",
                                 div(style = "margin-top: 1.5rem; padding-top: 1.5rem; border-top: 1px solid #dee2e6;",
                                     h6("Advanced Dependency Methods", style = "margin-bottom: 1rem; color: #667eea;"),
                                     p("These methods will be used when 'Model dependencies' is selected:",
                                       style = "color: #6c757d; font-size: 0.85rem; margin-bottom: 1rem;"),
                                     checkboxInput("method_rve", "Robust Variance Estimation", value = TRUE),
                                     checkboxInput("method_3_level", "3-Level Meta-Analysis", value = FALSE)
                                 )
                               ),

                               div(style = "margin-top: 1.5rem;",
                                   numericInput("k_smallest", "Minimum number of studies:",
                                                value = 5, min = 1, max = 20, step = 1),
                                   p("Analyses with fewer studies will be excluded",
                                     style = "color: #6c757d; font-size: 0.85rem; margin: 0;")
                               )
                           )
                    ),

                    # Right side - Meta-Analysis Methods
                    column(6,
                           div(style = "background: #f8f9fa; padding: 2rem; border-radius: 12px; border-left: 4px solid #764ba2;",
                               h5("Meta-Analysis Methods", style = "margin-bottom: 1.5rem; color: #2c3e50;"),

                               h6("Basic Methods", style = "margin-bottom: 1rem; color: #764ba2; font-size: 0.9rem;"),
                               checkboxInput("method_reml", "Random Effects (REML)", value = TRUE),
                               checkboxInput("method_fe", "Fixed Effects", value = FALSE),

                               h6("Publication Bias Correction", style = "margin: 1.5rem 0 1rem 0; color: #764ba2; font-size: 0.9rem;"),
                               checkboxInput("method_pet_peese_corr", "PET-PEESE (corrected)", value = TRUE),
                               checkboxInput("method_puni_star", "p-uniform*", value = TRUE),
                               checkboxInput("method_uwls", "UWLS", value = TRUE),
                               checkboxInput("method_pet_peese", "PET-PEESE", value = FALSE),
                               checkboxInput("method_waap", "WAAP", value = FALSE),
                               checkboxInput("method_bayesmeta", "Bayesian Meta-Analysis", value = FALSE)
                           )
                    )
                  )
              ),

              # Generate Specifications Section
              div(style = "background: white; border-radius: 20px; padding: 3rem; box-shadow: 0 10px 40px rgba(0,0,0,0.08); margin-bottom: 3rem;",
                  h3("Generate Analysis Specifications",
                     style = "color: #2c3e50; margin-bottom: 2rem; font-weight: 600;"),
                  p("Review your configuration and create the multiverse specifications.",
                    style = "color: #6c757d; margin-bottom: 2rem;"),

                  div(style = "text-align: center;",
                      actionButton("create_specs", "Create Specifications",
                                   class = "btn-primary",
                                   style = "background: linear-gradient(135deg, #667eea, #764ba2); border: none; padding: 15px 40px; font-size: 1.2rem; font-weight: 600; border-radius: 50px; color: white; box-shadow: 0 5px 20px rgba(102, 126, 234, 0.3);"),

                      conditionalPanel(
                        condition = "output.specs_created",
                        div(style = "margin-top: 2rem; padding: 0; background: transparent;",

                            # Success message with modern styling
                            div(style = "background: linear-gradient(135deg, #28a745, #20c997); padding: 2rem; border-radius: 16px; text-align: center; margin-bottom: 2rem; box-shadow: 0 8px 32px rgba(40, 167, 69, 0.2);",
                                h4("✓ Specifications Created Successfully",
                                   style = "color: white; margin: 0; font-weight: 600; font-size: 1.4rem;")
                            ),

                            # Specifications summary in cards
                            div(style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 1.5rem; margin-bottom: 2rem;",
                                uiOutput("specs_cards_output")
                            ),

                            # Which factors summary
                            uiOutput("wf_factors_output"),

                            # Methods summary
                            div(style = "background: white; padding: 2rem; border-radius: 16px; border: 1px solid #e9ecef; margin-bottom: 2rem;",
                                h6("Selected Meta-Analysis Methods",
                                   style = "color: #6c757d; margin-bottom: 1rem; font-size: 0.9rem; text-transform: uppercase; letter-spacing: 1px;"),
                                uiOutput("methods_tags_output")
                            ),

                            # Run analysis button
                            div(style = "text-align: center;",
                                actionButton("run_analysis", "Run Multiverse Analysis",
                                             style = "background: linear-gradient(135deg, #667eea, #764ba2); border: none; padding: 20px 50px; font-size: 1.4rem; font-weight: 600; border-radius: 50px; color: white; box-shadow: 0 10px 30px rgba(102, 126, 234, 0.3); text-transform: uppercase; letter-spacing: 1px;"),

                                conditionalPanel(
                                  condition = "output.analysis_running",
                                  div(style = "margin-top: 2rem; background: white; padding: 2rem; border-radius: 16px; border: 1px solid #e9ecef;",
                                      div(style = "text-align: center;",
                                          h5("⏳ Analysis in Progress", style = "color: #667eea; margin-bottom: 1rem;"),
                                          p("This may take several minutes...", style = "color: #6c757d; margin-bottom: 1.5rem;")
                                      ),
                                      div(style = "background: #f8f9fa; border-radius: 8px; height: 8px; overflow: hidden;",
                                          div(style = "height: 100%; background: linear-gradient(135deg, #667eea, #764ba2); animation: progress-animation 2s ease-in-out infinite;")
                                      )
                                  )
                                )
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

        tabPanel("Comprehensive Report",
                 br(),
                 div(style = "max-width: 900px; margin: 0 auto; background: white; padding: 3rem; border-radius: 16px; box-shadow: 0 5px 20px rgba(0,0,0,0.1);",
                     div(style = "text-align: right; margin-bottom: 2rem;",
                         downloadButton("download_report", "Download Report",
                                        class = "btn-primary",
                                        style = "background: linear-gradient(135deg, #667eea, #764ba2); border: none; padding: 10px 20px; border-radius: 25px;")
                     ),
                     div(id = "report-content",
                         style = "line-height: 1.6; color: #2c3e50;",
                         uiOutput("multiverse_report_output")
                     )
                 )
        ),

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
  # Which factors configuration for step 3
  output$wf_config_step3 <- renderUI({
    req(values$data)

    required_cols <- c("study", "es_id", "yi", "vi")
    all_other_cols <- names(values$data)[!names(values$data) %in% required_cols]
    potential_wf_cols <- all_other_cols[!sapply(all_other_cols, function(col) is.numeric(values$data[[col]]))]

    if (length(potential_wf_cols) == 0) {
      return(div(
        style = "text-align: center; padding: 3rem; background: #f8f9fa; border-radius: 12px; border: 2px dashed #dee2e6;",
        h4("No Which Factors Available", style = "color: #6c757d; margin-bottom: 1rem;"),
        p("Your data only contains required columns. Multiverse will vary only across meta-analysis methods.",
          style = "color: #6c757d; margin: 0;")
      ))
    }

    tagList(
      h5("Available Columns for Which Factors:",
         style = "margin-bottom: 1rem; color: #2c3e50;"),

      div(class = "wf-factor-grid",
          lapply(seq_along(potential_wf_cols), function(i) {
            col_name <- potential_wf_cols[i]
            unique_vals <- unique(values$data[[col_name]])

            div(class = "wf-factor-card",
                div(class = "wf-factor-header",
                    div(
                      h6(col_name, class = "wf-factor-name"),
                      p(paste("Values:", paste(head(unique_vals, 3), collapse = ", "),
                              if(length(unique_vals) > 3) paste("... (", length(unique_vals), "total)") else ""),
                        class = "wf-factor-values")
                    )
                ),

                div(class = "wf-decision-buttons",
                    tags$button("E", class = "wf-decision-btn equivalent",
                                onclick = paste0("setDecision('", col_name, "', 'E')"),
                                title = "Equivalent"),
                    tags$button("N", class = "wf-decision-btn non-equivalent",
                                onclick = paste0("setDecision('", col_name, "', 'N')"),
                                title = "Non-equivalent"),
                    tags$button("U", class = "wf-decision-btn uncertain",
                                onclick = paste0("setDecision('", col_name, "', 'U')"),
                                title = "Uncertain"),
                    tags$button("Ignore", class = "wf-decision-btn ignore active",
                                onclick = paste0("setDecision('", col_name, "', 'IGNORE')"),
                                title = "Ignore")
                ),

                # Hidden input to store the decision
                tags$input(type = "hidden",
                           id = paste0("decision_", col_name),
                           value = "IGNORE")
            )
          })
      ),

      # JavaScript for button interactions
      tags$script(HTML("
      function setDecision(colName, decision) {
        // Update hidden input
        document.getElementById('decision_' + colName).value = decision;

        // Update button states
        const card = event.target.closest('.wf-factor-card');
        const buttons = card.querySelectorAll('.wf-decision-btn');
        buttons.forEach(btn => btn.classList.remove('active'));
        event.target.classList.add('active');

        // Trigger Shiny input change
        Shiny.setInputValue('decision_' + colName, decision);
      }
    "))
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
    req(values$data, input$dependency_strategy)

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
        dependencies = input$dependency_strategy,
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
  # Specs cards output
  # Specs cards output
  output$specs_cards_output <- renderUI({
    req(values$specifications)

    total_specs <- nrow(values$specifications)
    unique_multiverses <- length(unique(values$specifications$multiverse_id))

    # Get which factors info
    wf_cols <- names(values$specifications)[grepl("^wf_", names(values$specifications))]
    wf_factors_count <- length(wf_cols)

    tagList(
      div(style = "background: white; padding: 2rem; border-radius: 16px; text-align: center; border: 1px solid #e9ecef; box-shadow: 0 2px 8px rgba(0,0,0,0.04);",
          h3(total_specs, style = "color: #667eea; margin: 0; font-size: 2.5rem; font-weight: 700;"),
          p("Total Specifications", style = "color: #6c757d; margin: 0; font-size: 0.9rem; text-transform: uppercase; letter-spacing: 1px;")
      ),
      div(style = "background: white; padding: 2rem; border-radius: 16px; text-align: center; border: 1px solid #e9ecef; box-shadow: 0 2px 8px rgba(0,0,0,0.04);",
          h3(unique_multiverses, style = "color: #764ba2; margin: 0; font-size: 2.5rem; font-weight: 700;"),
          p("Unique Multiverses", style = "color: #6c757d; margin: 0; font-size: 0.9rem; text-transform: uppercase; letter-spacing: 1px;")
      ),
      div(style = "background: white; padding: 2rem; border-radius: 16px; text-align: center; border: 1px solid #e9ecef; box-shadow: 0 2px 8px rgba(0,0,0,0.04);",
          h3(wf_factors_count, style = "color: #28a745; margin: 0; font-size: 2.5rem; font-weight: 700;"),
          p("Which Factors", style = "color: #6c757d; margin: 0; font-size: 0.9rem; text-transform: uppercase; letter-spacing: 1px;")
      )
    )
  })

  # Methods tags output
  output$methods_tags_output <- renderUI({
    req(values$specifications)

    methods <- unique(values$specifications$ma_method)

    div(style = "display: flex; flex-wrap: wrap; gap: 0.75rem;",
        lapply(methods, function(method) {
          tags$span(method,
                    style = "background: linear-gradient(135deg, #667eea, #764ba2); color: white; padding: 0.5rem 1rem; border-radius: 20px; font-size: 0.85rem; font-weight: 500;")
        })
    )
  })

  # Which factors details output
  output$wf_factors_output <- renderUI({
    req(values$specifications)

    # Get which factors columns
    wf_cols <- names(values$specifications)[grepl("^wf_", names(values$specifications))]

    if (length(wf_cols) == 0) {
      return(
        div(style = "background: white; padding: 2rem; border-radius: 16px; border: 1px solid #e9ecef; text-align: center;",
            h6("Which Factors", style = "color: #6c757d; margin-bottom: 1rem; font-size: 0.9rem; text-transform: uppercase; letter-spacing: 1px;"),
            p("No which factors selected - varying only across meta-analysis methods",
              style = "color: #6c757d; margin: 0; font-style: italic;")
        )
      )
    }

    # Create factor cards
    factor_cards <- lapply(wf_cols, function(wf_col) {
      # Get unique values for this factor
      unique_vals <- unique(values$specifications[[wf_col]])
      clean_name <- gsub("^wf_", "", wf_col)

      div(style = "background: #f8f9fa; padding: 1.5rem; border-radius: 12px; border: 1px solid #e9ecef;",
          h6(clean_name, style = "color: #2c3e50; margin-bottom: 1rem; font-weight: 600;"),
          div(style = "display: flex; flex-wrap: wrap; gap: 0.5rem;",
              lapply(unique_vals, function(val) {
                tags$span(val,
                          style = "background: white; color: #667eea; padding: 0.4rem 0.8rem; border-radius: 16px; font-size: 0.8rem; font-weight: 500; border: 1px solid #dee2e6;")
              })
          )
      )
    })

    div(style = "background: white; padding: 2rem; border-radius: 16px; border: 1px solid #e9ecef; margin-bottom: 2rem;",
        h6("Which Factors Explored",
           style = "color: #6c757d; margin-bottom: 1.5rem; font-size: 0.9rem; text-transform: uppercase; letter-spacing: 1px;"),
        div(style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(250px, 1fr)); gap: 1rem;",
            factor_cards
        )
    )
  })

  # Analysis running status
  values$analysis_running <- FALSE
  output$analysis_running <- reactive({ values$analysis_running })
  outputOptions(output, "analysis_running", suspendWhenHidden = FALSE)

  # Run analysis
  observeEvent(input$run_analysis, {
    req(values$data, values$specifications, input$k_smallest)

    values$analysis_running <- TRUE
    options(metaMultiverse.k_smallest_ma = input$k_smallest)

    showNotification("Running multiverse analysis...", type = "message", duration = NULL, id = "analysis_msg")

    tryCatch({
      values$results <- run_multiverse_analysis(
        data = values$data,
        specifications = values$specifications,
        verbose = TRUE
      )

      values$analysis_running <- FALSE
      removeNotification("analysis_msg")
      showNotification("Analysis completed!", type = "message", duration = 3)

    }, error = function(e) {
      values$analysis_running <- FALSE
      removeNotification("analysis_msg")
      showNotification(paste("Error running analysis:", e$message), type = "error", duration = 5)
    })
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
  # Generate multiverse report
  multiverse_report <- reactive({
    req(values$results)

    tryCatch({
      generate_multiverse_report(
        results = values$results,
        specifications = values$specifications,
        original_data = values$data,
        output_format = "html",
        include_plots = FALSE  # We'll handle plots separately in other tabs
      )
    }, error = function(e) {
      list(report = paste("Error generating report:", e$message))
    })
  })

  # Render the report
  output$multiverse_report_output <- renderUI({
    req(multiverse_report())

    # Convert markdown to HTML if needed
    report_html <- multiverse_report()$report

    # Add custom CSS for better formatting
    div(
      tags$style(HTML("
      #report-content h2 {
        color: #2c3e50;
        border-bottom: 2px solid #667eea;
        padding-bottom: 0.5rem;
        margin-top: 2rem;
      }
      #report-content h3 {
        color: #667eea;
        margin-top: 1.5rem;
      }
      #report-content h4 {
        color: #764ba2;
      }
      #report-content p {
        margin-bottom: 1rem;
        text-align: justify;
      }
      #report-content ul {
        margin-left: 1rem;
      }
      #report-content li {
        margin-bottom: 0.5rem;
      }
      #report-content strong {
        color: #2c3e50;
      }
    ")),
      HTML(report_html)
    )
  })

  # Download handler for the report
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("multiverse_report_", Sys.Date(), ".html")
    },
    content = function(file) {
      req(multiverse_report())

      # Create a complete HTML document
      html_content <- paste0(
        "<!DOCTYPE html>
      <html>
      <head>
        <title>Multiverse Meta-Analysis Report</title>
        <style>
          body { font-family: Arial, sans-serif; max-width: 800px; margin: 0 auto; padding: 2rem; line-height: 1.6; }
          h2 { color: #2c3e50; border-bottom: 2px solid #667eea; padding-bottom: 0.5rem; }
          h3 { color: #667eea; }
          h4 { color: #764ba2; }
          p { text-align: justify; }
          ul { margin-left: 1rem; }
          li { margin-bottom: 0.5rem; }
        </style>
      </head>
      <body>
        <h1>Multiverse Meta-Analysis Report</h1>
        <p><em>Generated on ", Sys.Date(), "</em></p>
        ", multiverse_report()$report, "
      </body>
      </html>"
      )

      writeLines(html_content, file)
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)
