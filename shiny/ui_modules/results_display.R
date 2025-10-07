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

        # Tabs for different visualizations
        tags$div(class = "mt-xl",
                 style = "border-bottom: 2px solid rgba(0,0,0,0.06); margin-bottom: 32px;",
                 div(class = "d-flex gap-md",
                     style = "margin-bottom: -2px;",
                     actionButton("tab_spec_curve", "Specification Curve",
                                  class = "btn-ghost",
                                  style = "border-radius: 8px 8px 0 0; border-bottom: 3px solid var(--cosmic-accent);"),
                     actionButton("tab_voe", "Vibration of Effects",
                                  class = "btn-ghost",
                                  style = "border-radius: 8px 8px 0 0;"),
                     actionButton("tab_table", "Results Table",
                                  class = "btn-ghost",
                                  style = "border-radius: 8px 8px 0 0;"),
                     actionButton("tab_warnings", "Warnings",
                                  class = "btn-ghost",
                                  style = "border-radius: 8px 8px 0 0;")
                 )
        ),

        # Specification Curve
        div(id = "panel_spec_curve",
            div(class = "card",
                div(class = "card-body",
                    p("Effect sizes across all specifications, ranked from smallest to largest.",
                      class = "text-muted mb-lg"),
                    plotlyOutput("spec_curve", height = "600px")
                )
            )
        ),

        # Vibration of Effects
        div(id = "panel_voe", style = "display: none;",
            div(class = "card",
                div(class = "card-body",
                    p("Relationship between effect sizes and p-values across the multiverse.",
                      class = "text-muted mb-lg"),
                    plotlyOutput("voe_plot", height = "600px")
                )
            )
        ),

        # Results Table
        div(id = "panel_table", style = "display: none;",
            div(class = "card",
                div(class = "card-header",
                    h4(class = "card-title mb-0", "Results Table")
                ),
                div(class = "card-body",
                    DT::dataTableOutput("results_table")
                )
            )
        ),

        # Warnings
        div(id = "panel_warnings", style = "display: none;",
            div(class = "card",
                div(class = "card-header",
                    h4(class = "card-title mb-0", "Analysis Warnings")
                ),
                div(class = "card-body",
                    verbatimTextOutput("warnings_output")
                )
            )
        ),

        # Tab switching JavaScript
        tags$script(HTML("
          $(document).ready(function() {
            $('#tab_spec_curve').click(function() {
              $('.btn-ghost').css('border-bottom', 'none');
              $(this).css('border-bottom', '3px solid var(--cosmic-accent)');
              $('#panel_spec_curve, #panel_voe, #panel_table, #panel_warnings').hide();
              $('#panel_spec_curve').show();
            });

            $('#tab_voe').click(function() {
              $('.btn-ghost').css('border-bottom', 'none');
              $(this).css('border-bottom', '3px solid var(--cosmic-accent)');
              $('#panel_spec_curve, #panel_voe, #panel_table, #panel_warnings').hide();
              $('#panel_voe').show();
            });

            $('#tab_table').click(function() {
              $('.btn-ghost').css('border-bottom', 'none');
              $(this).css('border-bottom', '3px solid var(--cosmic-accent)');
              $('#panel_spec_curve, #panel_voe, #panel_table, #panel_warnings').hide();
              $('#panel_table').show();
            });

            $('#tab_warnings').click(function() {
              $('.btn-ghost').css('border-bottom', 'none');
              $(this).css('border-bottom', '3px solid var(--cosmic-accent)');
              $('#panel_spec_curve, #panel_voe, #panel_table, #panel_warnings').hide();
              $('#panel_warnings').show();
            });
          });
        "))
      )
    )
  )
}
