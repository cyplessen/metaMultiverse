# ==============================================================================
# UTILS/NAVIGATION_HELPERS.R
# ==============================================================================

#' Get Navigation UI Components
get_navigation_ui <- function() {
  tagList(
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

    # JavaScript for menu functionality
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
    "))
  )
}

#' Setup Navigation Server Logic
setup_navigation_server <- function(input, output, session, values) {

  # Navigation observers
  observeEvent(input$goto_step1, { values$current_step <- 1 })
  observeEvent(input$goto_step2, { values$current_step <- 2 })
  observeEvent(input$goto_step3, { values$current_step <- 3 })
  observeEvent(input$goto_step4, { values$current_step <- 4 })
  observeEvent(input$goto_about, { values$current_step <- 5 })

  # Landing page CTA
  observeEvent(input$start_analysis, { values$current_step <- 2 })

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

  # Navigation button rendering
  output$prev_button <- renderUI({
    if (values$current_step > 1 && values$current_step <= 4) {
      actionButton("prev_step", "← Previous", class = "btn-default")
    }
  })

  output$next_button <- renderUI({
    switch(as.character(values$current_step),
           "1" = NULL,
           "2" = if (!is.null(values$data)) {
             actionButton("next_step", "Configure Analysis →", class = "btn-primary")
           } else {
             tags$span("Upload data to continue", style = "color: #999;")
           },
           "3" = if (!is.null(values$spec_output)) {
             actionButton("next_step", "View Results →", class = "btn-primary")
           } else {
             tags$span("Create specifications to continue", style = "color: #999;")
           },
           "4" = NULL
    )
  })
}
