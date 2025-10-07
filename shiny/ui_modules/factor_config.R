# ==============================================================================
# UI_MODULES/FACTOR_CONFIG.R
# ==============================================================================

#' Enhanced Factor Configuration UI Module
#' Enhanced Factor Configuration UI Module
factor_config_ui <- function() {
  div(style = "min-height: 100vh; background: linear-gradient(to bottom, #f8f9fa, #ffffff); padding: 80px 0;",
      div(style = "max-width: 1400px; margin: 0 auto; padding: 0 40px;",

          # Header section
          div(style = "text-align: center; margin-bottom: 4rem;",
              h1("Step 2: Configure Your Analysis",
                 style = "font-size: 3rem; font-weight: 300; color: #2c3e50; margin-bottom: 1.5rem;"),
              p("Set up your multiverse analysis using the enhanced define_factors() approach. Create sophisticated inclusion criteria and custom groupings.",
                style = "font-size: 1.3rem; color: #6c757d; line-height: 1.6; max-width: 800px; margin: 0 auto;")
          ),

          conditionalPanel(
            condition = "output.data_uploaded",

            # Enhanced Factor Selection Section
            get_enhanced_factor_selection_ui(),

            # Methods and Dependencies Section
            get_methods_dependencies_ui(),

            # Enhanced Specifications Section
            get_enhanced_specifications_ui()
          )
      )
  )
}

# ==============================================================================
# UI HELPER FUNCTIONS FOR FACTOR CONFIG (ADD THESE TO THE SAME FILE)
# ==============================================================================

#' Get Enhanced Factor Selection UI
get_enhanced_factor_selection_ui <- function() {
  div(style = "background: white; border-radius: 20px; padding: 3rem; box-shadow: 0 10px 40px rgba(0,0,0,0.08); margin-bottom: 3rem;",
      h3("Which Factors to Explore",
         style = "color: #2c3e50; margin-bottom: 2rem; font-weight: 600;"),

      # Enhanced description (cosmic minimalism)
      div(class = "alert-info",
          tags$strong("🚀 Enhanced Workflow: "),
          "Create sophisticated inclusion criteria with custom groupings. For example, define 'conservative', 'moderate', and 'liberal' inclusion strategies based on risk of bias."
      ),

      # Enhanced Decision Framework Guide
      get_enhanced_decision_framework_guide(),

      # Enhanced factor configuration content
      uiOutput("factor_config_content")
  )
}

#' Get Enhanced Decision Framework Guide
get_enhanced_decision_framework_guide <- function() {
  div(style = "margin-bottom: 3rem;",
      # Simple, clean grid
      div(style = "display: grid; grid-template-columns: repeat(3, 1fr); gap: 2rem; margin-bottom: 2rem;",

          # Equivalent
          div(style = "text-align: center;",
              div(style = "width: 48px; height: 48px; background: #2196F3; color: white; border-radius: 50%; display: flex; align-items: center; justify-content: center; font-size: 1.5rem; font-weight: 300; margin: 0 auto 1rem;", "E"),
              h6("Equivalent", style = "color: #2c3e50; margin-bottom: 0.5rem; font-weight: 400;"),
              p("Same construct, different measures", style = "color: #64748b; font-size: 0.875rem; line-height: 1.4; margin: 0;")
          ),

          # Uncertain
          div(style = "text-align: center;",
              div(style = "width: 48px; height: 48px; background: #FF9800; color: white; border-radius: 50%; display: flex; align-items: center; justify-content: center; font-size: 1.5rem; font-weight: 300; margin: 0 auto 1rem;", "U"),
              h6("Uncertain", style = "color: #2c3e50; margin-bottom: 0.5rem; font-weight: 400;"),
              p("Explore reasonable alternatives", style = "color: #64748b; font-size: 0.875rem; line-height: 1.4; margin: 0;")
          ),

          # Non-equivalent
          div(style = "text-align: center;",
              div(style = "width: 48px; height: 48px; background: #9C27B0; color: white; border-radius: 50%; display: flex; align-items: center; justify-content: center; font-size: 1.5rem; font-weight: 300; margin: 0 auto 1rem;", "N"),
              h6("Non-equivalent", style = "color: #2c3e50; margin-bottom: 0.5rem; font-weight: 400;"),
              p("Different research questions", style = "color: #64748b; font-size: 0.875rem; line-height: 1.4; margin: 0;")
          )
      )
  )
}

#' Get Methods and Dependencies UI
get_methods_dependencies_ui <- function() {
  div(style = "background: white; border-radius: 20px; padding: 3rem; box-shadow: 0 10px 40px rgba(0,0,0,0.08); margin-bottom: 3rem;",
      h3("How to Handle Dependencies",
         style = "color: #2c3e50; margin-bottom: 2rem; font-weight: 600;"),
      p("Choose how to handle cases where studies contribute multiple effect sizes, then select meta-analytic methods.",
        style = "color: #6c757d; margin-bottom: 2rem;"),

      fluidRow(
        # Left side - Dependency Strategy
        column(6, get_dependency_strategy_ui()),

        # Right side - Meta-Analysis Methods
        column(6, get_ma_methods_ui())
      )
  )
}

#' Get Dependency Strategy UI
get_dependency_strategy_ui <- function() {
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

      # Advanced dependency methods
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
}

#' Get Meta-Analysis Methods UI
get_ma_methods_ui <- function() {
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
}

#' Get Enhanced Specifications UI
get_enhanced_specifications_ui <- function() {
  div(style = "background: white; border-radius: 20px; padding: 3rem; box-shadow: 0 10px 40px rgba(0,0,0,0.08); margin-bottom: 3rem;",
      h3("Generate Enhanced Analysis Specifications",
         style = "color: #2c3e50; margin-bottom: 2rem; font-weight: 600;"),

      # Enhanced description
      div(style = "background: #e3f2fd; padding: 1.5rem; border-radius: 12px; margin-bottom: 2rem; border-left: 4px solid #2196F3;",
          h6("🔬 Using define_factors() Workflow", style = "color: #1976D2; margin-bottom: 1rem;"),
          p("Your factor configurations will be processed using the enhanced define_factors() function, which supports sophisticated custom groupings and maintains meaningful factor names throughout the analysis.",
            style = "color: #1565C0; font-size: 0.9rem; margin: 0;")
      ),

      div(style = "text-align: center;",
          actionButton("create_specs", "Create Enhanced Specifications",
                       class = "btn-primary",
                       style = "background: linear-gradient(135deg, #667eea, #764ba2); border: none; padding: 18px 45px; font-size: 1.3rem; font-weight: 600; border-radius: 50px; color: white; box-shadow: 0 8px 25px rgba(102, 126, 234, 0.4); text-transform: uppercase; letter-spacing: 1px;"),

          conditionalPanel(
            condition = "output.specs_created",
            get_enhanced_specs_success_ui()
          )
      )
  )
}

#' Get Enhanced Specs Success UI
get_enhanced_specs_success_ui <- function() {
  div(style = "margin-top: 3rem; padding: 0; background: transparent;",

      # Enhanced success message
      div(style = "background: linear-gradient(135deg, #28a745, #20c997); padding: 2.5rem; border-radius: 20px; text-align: center; margin-bottom: 2rem; box-shadow: 0 10px 40px rgba(40, 167, 69, 0.3);",
          h4("✨ Enhanced Specifications Created Successfully",
             style = "color: white; margin: 0; font-weight: 600; font-size: 1.5rem;"),
          p("Using define_factors() workflow with custom groupings",
            style = "color: white; margin: 0.5rem 0 0 0; font-size: 1rem; opacity: 0.9;")
      ),

      # Enhanced specifications summary
      uiOutput("specs_summary"),

      # Run analysis button
      div(style = "text-align: center; margin-top: 2rem;",
          conditionalPanel(
            condition = "!output.analysis_complete && !output.analysis_running",
            actionButton("run_analysis", "Run Multiverse Analysis",
                         class = "btn-primary",
                         style = "padding: 16px 40px; font-size: 1.125rem;")
          ),

          conditionalPanel(
            condition = "output.analysis_running",
            get_enhanced_analysis_progress_ui()
          ),

          conditionalPanel(
            condition = "output.analysis_complete && !output.analysis_running",
            div(class = "alert-success",
                style = "display: inline-block; margin-bottom: 16px;",
                tags$strong("✓ Analysis Complete! "),
                "Your multiverse has been explored."
            ),
            tags$br(),
            actionButton("goto_step4", "View Results →",
                         class = "btn-primary",
                         style = "padding: 16px 40px; font-size: 1.125rem;")
          )
      )
  )
}

#' Get Enhanced Analysis Progress UI
get_enhanced_analysis_progress_ui <- function() {
  div(class = "card mt-lg",
      div(class = "card-body text-center",
          h4("Analysis Running", class = "mb-md"),
          p("Processing your multiverse specifications...",
            class = "text-muted mb-lg"),
          div(class = "loading-overlay",
              div(class = "loading-spinner")
          ),
          p("This may take several minutes depending on complexity.",
            class = "text-muted", style = "font-size: 0.875rem;")
      )
  )
}
