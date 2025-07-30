library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(metaMultiverse)

# Load your package functions (assuming you're in the package directory)
# If running from shiny/ folder, use:
devtools::load_all("..")

# Define UI
# CORRECTED STRUCTURE - Replace your current ui section

ui <- dashboardPage(
  dashboardHeader(
    title = "Multiverse Meta-Analysis",
    tags$li(class = "dropdown",
            tags$a(id = "theme-toggle", href = "#", class = "dropdown-toggle",
                   `data-toggle` = "dropdown", role = "button",
                   `aria-haspopup` = "true", `aria-expanded` = "false",
                   style = "padding: 15px;",
                   tags$i(class = "fa fa-sun-o", id = "theme-icon"),
                   " Light Mode"
            )
    )
  ),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Upload", tabName = "upload", icon = icon("upload")),
      menuItem("Configuration", tabName = "config", icon = icon("cogs")),
      menuItem("Results", tabName = "results", icon = icon("chart-line"))
    )
  ),

  dashboardBody(
    # Clean Minimal CSS
    tags$head(
      tags$style(HTML("
      /* Minimal Modern Shiny App - Clean & Elegant Design */
      @import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&family=Space+Grotesk:wght@300;400;500;600;700&display=swap');

      :root {
        --primary-color: #2563eb;
        --primary-hover: #1d4ed8;
        --success-color: #10b981;
        --warning-color: #f59e0b;
        --danger-color: #ef4444;

        --dark-bg-primary: #0f0f0f;
        --dark-bg-secondary: #1a1a1a;
        --dark-bg-tertiary: #262626;
        --dark-bg-elevated: #2a2a2a;
        --dark-text-primary: #ffffff;
        --dark-text-secondary: #a3a3a3;
        --dark-text-muted: #737373;
        --dark-border: #404040;
        --dark-border-light: #525252;

        --light-bg-primary: #ffffff;
        --light-bg-secondary: #fafafa;
        --light-bg-tertiary: #f5f5f5;
        --light-bg-elevated: #ffffff;
        --light-text-primary: #0a0a0a;
        --light-text-secondary: #525252;
        --light-text-muted: #737373;
        --light-border: #e5e5e5;
        --light-border-light: #d4d4d4;

        --space-1: 0.25rem; --space-2: 0.5rem; --space-3: 0.75rem; --space-4: 1rem;
        --space-5: 1.25rem; --space-6: 1.5rem; --space-8: 2rem; --space-10: 2.5rem;
        --space-12: 3rem; --space-16: 4rem; --space-20: 5rem;

        --radius-sm: 4px; --radius-md: 8px; --radius-lg: 12px; --radius-xl: 16px; --radius-2xl: 24px;

        --shadow-xs: 0 1px 2px 0 rgba(0, 0, 0, 0.05);
        --shadow-sm: 0 1px 3px 0 rgba(0, 0, 0, 0.1), 0 1px 2px -1px rgba(0, 0, 0, 0.1);
        --shadow-md: 0 4px 6px -1px rgba(0, 0, 0, 0.1), 0 2px 4px -2px rgba(0, 0, 0, 0.1);
        --shadow-lg: 0 10px 15px -3px rgba(0, 0, 0, 0.1), 0 4px 6px -4px rgba(0, 0, 0, 0.1);

        --transition-fast: 150ms cubic-bezier(0.4, 0, 0.2, 1);
        --transition-normal: 300ms cubic-bezier(0.4, 0, 0.2, 1);
      }

      * { box-sizing: border-box; margin: 0; padding: 0; }

      body {
        font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif !important;
        font-weight: 400; line-height: 1.6; letter-spacing: -0.011em;
        -webkit-font-smoothing: antialiased; -moz-osx-font-smoothing: grayscale;
        transition: all var(--transition-normal);
      }

      .content-wrapper, .right-side {
        background: none !important; padding: 0 !important; margin: 0 !important;
      }

      .main-header {
        background: var(--dark-bg-elevated) !important; border: none !important;
        box-shadow: 0 1px 0 var(--dark-border) !important; height: 64px !important;
        z-index: 100; backdrop-filter: blur(8px);
      }

      .main-header .navbar {
        background: transparent !important; border: none !important; min-height: 64px !important;
      }

      .main-header .navbar-brand {
        font-family: 'Space Grotesk', sans-serif !important; font-weight: 600 !important;
        font-size: 20px !important; color: var(--dark-text-primary) !important;
        padding: 20px 24px !important; transition: all var(--transition-fast);
        text-decoration: none !important;
      }

      .main-header .navbar-brand:hover {
        color: var(--primary-color) !important; transform: none;
      }

      [data-theme='light'] .main-header {
        background: var(--light-bg-elevated) !important;
        box-shadow: 0 1px 0 var(--light-border) !important;
      }

      [data-theme='light'] .main-header .navbar-brand {
        color: var(--light-text-primary) !important;
      }

      .main-sidebar {
        background: var(--dark-bg-secondary) !important; border: none !important;
        box-shadow: 1px 0 0 var(--dark-border) !important; width: 260px !important;
      }

      [data-theme='light'] .main-sidebar {
        background: var(--light-bg-secondary) !important;
        box-shadow: 1px 0 0 var(--light-border) !important;
      }

      .sidebar-menu { padding: var(--space-6) 0 !important; margin: 0 !important; }

      .sidebar-menu > li > a {
        padding: var(--space-3) var(--space-6) !important;
        color: var(--dark-text-secondary) !important; font-weight: 500 !important;
        font-size: 14px !important; border: none !important; border-radius: 0 !important;
        margin: 0 var(--space-4) var(--space-1) var(--space-4) !important;
        transition: all var(--transition-fast) !important; background: transparent !important;
        display: flex !important; align-items: center !important; text-decoration: none !important;
      }

      .sidebar-menu > li > a i {
        margin-right: var(--space-3) !important; width: 16px !important; text-align: center !important;
      }

      .sidebar-menu > li > a:hover {
        background: var(--dark-bg-tertiary) !important; color: var(--dark-text-primary) !important;
        border-radius: var(--radius-md) !important; transform: none !important; box-shadow: none !important;
      }

      .sidebar-menu > li.active > a {
        background: var(--primary-color) !important; color: white !important;
        border-radius: var(--radius-md) !important; font-weight: 600 !important;
      }

      [data-theme='light'] .sidebar-menu > li > a {
        color: var(--light-text-secondary) !important;
      }

      [data-theme='light'] .sidebar-menu > li > a:hover {
        background: var(--light-bg-tertiary) !important; color: var(--light-text-primary) !important;
      }

      #theme-toggle {
        background: transparent !important; border: 1px solid var(--dark-border) !important;
        color: var(--dark-text-secondary) !important; padding: var(--space-2) var(--space-3) !important;
        border-radius: var(--radius-md) !important; font-weight: 500 !important;
        font-size: 13px !important; transition: all var(--transition-fast) !important;
        margin-right: var(--space-4) !important;
      }

      #theme-toggle:hover {
        background: var(--dark-bg-tertiary) !important; border-color: var(--dark-border-light) !important;
        color: var(--dark-text-primary) !important; transform: none !important; box-shadow: none !important;
      }

      [data-theme='light'] #theme-toggle {
        border-color: var(--light-border) !important; color: var(--light-text-secondary) !important;
      }

      [data-theme='light'] #theme-toggle:hover {
        background: var(--light-bg-tertiary) !important; border-color: var(--light-border-light) !important;
        color: var(--light-text-primary) !important;
      }

      .content-wrapper {
        background: var(--dark-bg-primary) !important; min-height: 100vh;
        padding: var(--space-8) !important; margin-left: 260px !important;
        transition: all var(--transition-normal);
      }

      [data-theme='light'] .content-wrapper {
        background: var(--light-bg-primary) !important;
      }

      .box {
        background: var(--dark-bg-elevated) !important; border: 1px solid var(--dark-border) !important;
        border-radius: var(--radius-xl) !important; box-shadow: var(--shadow-sm) !important;
        margin-bottom: var(--space-8) !important; transition: all var(--transition-normal) !important;
        overflow: hidden;
      }

      .box:hover {
        box-shadow: var(--shadow-md) !important; border-color: var(--dark-border-light) !important;
        transform: translateY(-1px);
      }

      [data-theme='light'] .box {
        background: var(--light-bg-elevated) !important; border-color: var(--light-border) !important;
      }

      [data-theme='light'] .box:hover {
        border-color: var(--light-border-light) !important; box-shadow: var(--shadow-lg) !important;
      }

      .box-header {
        background: transparent !important; border-bottom: 1px solid var(--dark-border) !important;
        padding: var(--space-6) var(--space-8) !important;
      }

      [data-theme='light'] .box-header {
        border-bottom-color: var(--light-border) !important;
      }

      .box-title {
        font-family: 'Space Grotesk', sans-serif !important; font-weight: 600 !important;
        font-size: 18px !important; color: var(--dark-text-primary) !important;
        margin: 0 !important; line-height: 1.4; letter-spacing: -0.025em;
      }

      [data-theme='light'] .box-title {
        color: var(--light-text-primary) !important;
      }

      .box-body { padding: var(--space-8) !important; }

      .info-box {
        background: var(--dark-bg-tertiary) !important; border: 1px solid var(--dark-border) !important;
        border-radius: var(--radius-lg) !important; padding: var(--space-6) !important;
        margin: var(--space-4) 0 !important; transition: all var(--transition-fast) !important;
      }

      .info-box:hover {
        border-color: var(--dark-border-light) !important; transform: translateY(-1px);
      }

      [data-theme='light'] .info-box {
        background: var(--light-bg-tertiary) !important; border-color: var(--light-border) !important;
      }

      [data-theme='light'] .info-box:hover {
        border-color: var(--light-border-light) !important;
      }

      h1, h2, h3, h4, h5, h6 {
        font-family: 'Space Grotesk', sans-serif !important; font-weight: 600 !important;
        color: var(--dark-text-primary) !important; margin-bottom: var(--space-4) !important;
        line-height: 1.3; letter-spacing: -0.025em;
      }

      h1 { font-size: 32px !important; } h2 { font-size: 24px !important; }
      h3 { font-size: 20px !important; } h4 { font-size: 18px !important; }
      h5 { font-size: 16px !important; } h6 { font-size: 14px !important; }

      [data-theme='light'] h1, [data-theme='light'] h2, [data-theme='light'] h3,
      [data-theme='light'] h4, [data-theme='light'] h5, [data-theme='light'] h6 {
        color: var(--light-text-primary) !important;
      }

      p, span, li {
        color: var(--dark-text-secondary) !important; line-height: 1.6; font-size: 14px;
      }

      [data-theme='light'] p, [data-theme='light'] span, [data-theme='light'] li {
        color: var(--light-text-secondary) !important;
      }

      .form-control {
        background: var(--dark-bg-tertiary) !important; border: 1px solid var(--dark-border) !important;
        border-radius: var(--radius-md) !important; color: var(--dark-text-primary) !important;
        padding: var(--space-3) var(--space-4) !important; font-size: 14px !important;
        font-weight: 400 !important; transition: all var(--transition-fast) !important;
        box-shadow: none !important;
      }

      .form-control:focus {
        border-color: var(--primary-color) !important;
        box-shadow: 0 0 0 3px rgba(37, 99, 235, 0.1) !important;
        background: var(--dark-bg-elevated) !important; outline: none !important;
      }

      [data-theme='light'] .form-control {
        background: var(--light-bg-secondary) !important; border-color: var(--light-border) !important;
        color: var(--light-text-primary) !important;
      }

      [data-theme='light'] .form-control:focus {
        background: var(--light-bg-primary) !important;
        box-shadow: 0 0 0 3px rgba(37, 99, 235, 0.1) !important;
      }

      .btn {
        font-family: 'Inter', sans-serif !important; font-weight: 500 !important;
        font-size: 14px !important; padding: var(--space-3) var(--space-5) !important;
        border-radius: var(--radius-md) !important; border: 1px solid transparent !important;
        transition: all var(--transition-fast) !important; text-transform: none !important;
        letter-spacing: 0; cursor: pointer; display: inline-flex !important;
        align-items: center !important; justify-content: center !important;
        gap: var(--space-2) !important;
      }

      .btn-primary {
        background: var(--primary-color) !important; color: white !important;
        border-color: var(--primary-color) !important;
      }

      .btn-primary:hover {
        background: var(--primary-hover) !important; border-color: var(--primary-hover) !important;
        transform: translateY(-1px); box-shadow: var(--shadow-md) !important;
      }

      .btn-success {
        background: var(--success-color) !important; color: white !important;
        border-color: var(--success-color) !important;
      }

      .btn-success:hover {
        background: #059669 !important; border-color: #059669 !important;
        transform: translateY(-1px); box-shadow: var(--shadow-md) !important;
      }

      .btn:disabled {
        opacity: 0.5 !important; cursor: not-allowed !important; transform: none !important;
      }

      .dataTables_wrapper { margin-top: var(--space-6) !important; }

      table.dataTable {
        border-collapse: separate !important; border-spacing: 0 !important;
        background: transparent !important; width: 100% !important;
        border-radius: var(--radius-lg) !important; overflow: hidden !important;
      }

      table.dataTable thead th {
        background: var(--dark-bg-tertiary) !important; color: var(--dark-text-primary) !important;
        font-weight: 600 !important; font-size: 12px !important; text-transform: uppercase;
        letter-spacing: 0.05em; padding: var(--space-4) !important; border: none !important;
        border-bottom: 1px solid var(--dark-border) !important;
      }

      [data-theme='light'] table.dataTable thead th {
        background: var(--light-bg-tertiary) !important; color: var(--light-text-primary) !important;
        border-bottom-color: var(--light-border) !important;
      }

      table.dataTable tbody td {
        background: var(--dark-bg-elevated) !important; color: var(--dark-text-secondary) !important;
        padding: var(--space-4) !important; border: none !important;
        border-bottom: 1px solid var(--dark-border) !important; font-size: 13px;
      }

      table.dataTable tbody tr:hover td {
        background: var(--dark-bg-tertiary) !important;
      }

      [data-theme='light'] table.dataTable tbody td {
        background: var(--light-bg-primary) !important; color: var(--light-text-secondary) !important;
        border-bottom-color: var(--light-border) !important;
      }

      [data-theme='light'] table.dataTable tbody tr:hover td {
        background: var(--light-bg-secondary) !important;
      }

      .nav-tabs {
        border: none !important; margin-bottom: var(--space-6) !important;
        background: var(--dark-bg-tertiary) !important; border-radius: var(--radius-lg) !important;
        padding: var(--space-1) !important; display: flex !important;
      }

      [data-theme='light'] .nav-tabs {
        background: var(--light-bg-tertiary) !important;
      }

      .nav-tabs > li { margin: 0 !important; flex: 1; }

      .nav-tabs > li > a {
        background: transparent !important; border: none !important;
        border-radius: var(--radius-md) !important; color: var(--dark-text-secondary) !important;
        font-weight: 500 !important; font-size: 13px !important;
        padding: var(--space-3) var(--space-4) !important; margin: 0 !important;
        transition: all var(--transition-fast) !important; text-align: center !important;
      }

      .nav-tabs > li > a:hover {
        background: var(--dark-bg-elevated) !important; color: var(--dark-text-primary) !important;
      }

      .nav-tabs > li.active > a {
        background: var(--primary-color) !important; color: white !important; font-weight: 600 !important;
      }

      [data-theme='light'] .nav-tabs > li > a {
        color: var(--light-text-secondary) !important;
      }

      [data-theme='light'] .nav-tabs > li > a:hover {
        background: var(--light-bg-primary) !important; color: var(--light-text-primary) !important;
      }

      .alert {
        border: 1px solid !important; border-radius: var(--radius-lg) !important;
        padding: var(--space-4) !important; font-weight: 400 !important;
        font-size: 14px !important; box-shadow: none !important;
      }

      .alert-info {
        background: rgba(37, 99, 235, 0.1) !important; color: var(--primary-color) !important;
        border-color: rgba(37, 99, 235, 0.2) !important;
      }

      .well {
        background: var(--dark-bg-tertiary) !important; border: 1px solid var(--dark-border) !important;
        border-radius: var(--radius-lg) !important; padding: var(--space-6) !important;
        box-shadow: none !important;
      }

      [data-theme='light'] .well {
        background: var(--light-bg-secondary) !important; border-color: var(--light-border) !important;
      }

      .checkbox input[type='checkbox'], .radio input[type='radio'] {
        width: 16px !important; height: 16px !important; accent-color: var(--primary-color) !important;
      }

      .checkbox label, .radio label {
        font-weight: 400 !important; font-size: 14px !important; margin-left: var(--space-2) !important;
        color: var(--dark-text-secondary) !important;
      }

      [data-theme='light'] .checkbox label, [data-theme='light'] .radio label {
        color: var(--light-text-secondary) !important;
      }

      input[type='file'] {
        padding: var(--space-4) !important; border: 2px dashed var(--dark-border) !important;
        border-radius: var(--radius-lg) !important; background: var(--dark-bg-tertiary) !important;
        color: var(--dark-text-secondary) !important; font-size: 13px !important;
        transition: all var(--transition-fast) !important;
      }

      input[type='file']:hover {
        border-color: var(--primary-color) !important; background: var(--dark-bg-elevated) !important;
      }

      [data-theme='light'] input[type='file'] {
        background: var(--light-bg-secondary) !important; border-color: var(--light-border) !important;
        color: var(--light-text-secondary) !important;
      }

      [data-theme='light'] input[type='file']:hover {
        background: var(--light-bg-primary) !important;
      }

      ::-webkit-scrollbar { width: 6px; height: 6px; }
      ::-webkit-scrollbar-track { background: transparent; }
      ::-webkit-scrollbar-thumb { background: var(--dark-border); border-radius: 3px; }
      ::-webkit-scrollbar-thumb:hover { background: var(--dark-border-light); }
      [data-theme='light'] ::-webkit-scrollbar-thumb { background: var(--light-border); }
      [data-theme='light'] ::-webkit-scrollbar-thumb:hover { background: var(--light-border-light); }

      @keyframes fadeInUp {
        from { opacity: 0; transform: translateY(20px); }
        to { opacity: 1; transform: translateY(0); }
      }

      .box { animation: fadeInUp 0.4s ease-out; }

      .loading-spinner {
        display: inline-block; width: 16px; height: 16px;
        border: 2px solid rgba(255, 255, 255, 0.3); border-radius: 50%;
        border-top-color: currentColor; animation: spin 0.8s linear infinite;
      }

      @keyframes spin { to { transform: rotate(360deg); } }

      *:focus { outline: 2px solid var(--primary-color) !important; outline-offset: 2px !important; }
      ::selection { background: rgba(37, 99, 235, 0.2); color: inherit; }

      @media (max-width: 768px) {
        .content-wrapper { margin-left: 0 !important; padding: var(--space-4) !important; }
        .main-sidebar { width: 100% !important; height: auto !important; position: relative !important; }
        .box-body, .box-header { padding: var(--space-4) !important; }
      }
    "))
    ),

    # Clean JavaScript
    tags$script(HTML("
    $(document).ready(function() {
      function initializeThemeToggle() {
        $('html').attr('data-theme', 'dark');
        $('*').css('transition', 'background-color 200ms ease, color 200ms ease, border-color 200ms ease');

        $('#theme-toggle').click(function(e) {
          e.preventDefault();
          const currentTheme = $('html').attr('data-theme');
          const newTheme = currentTheme === 'dark' ? 'light' : 'dark';
          $('html').css('opacity', '0.98');

          setTimeout(() => {
            $('html').attr('data-theme', newTheme);
            if (newTheme === 'dark') {
              $('#theme-icon').removeClass('fa-moon-o').addClass('fa-sun-o');
              $('#theme-toggle').html('<i class=\"fa fa-sun-o\" id=\"theme-icon\"></i> Light');
            } else {
              $('#theme-icon').removeClass('fa-sun-o').addClass('fa-moon-o');
              $('#theme-toggle').html('<i class=\"fa fa-moon-o\" id=\"theme-icon\"></i> Dark');
            }
            $('html').css('opacity', '1');
          }, 100);
        });
      }

      function addLoadingStates() {
        $('.btn').on('click', function() {
          const btn = $(this);
          const originalText = btn.html();
          if (btn.attr('id') === 'theme-toggle' || btn.prop('disabled')) return;

          btn.html('<span class=\"loading-spinner\"></span>');
          btn.prop('disabled', true);
          btn.css('min-width', btn.outerWidth() + 'px');

          setTimeout(() => {
            btn.html(originalText);
            btn.prop('disabled', false);
            btn.css('min-width', 'auto');
          }, 1500);
        });
      }

      function addCardInteractions() {
        $('.box').hover(
          function() { $(this).css('transform', 'translateY(-2px)'); },
          function() { $(this).css('transform', 'translateY(0)'); }
        );
      }

      initializeThemeToggle();
      addLoadingStates();
      addCardInteractions();
    });
  ")),

    tabItems(
      # Data Upload Tab
      tabItem(tabName = "upload",
              fluidRow(
                box(title = "Welcome to Multiverse Meta-Analysis", status = "info", solidHeader = TRUE, width = 12,
                    div(class = "info-box",
                        h3("About This Application"),
                        p("This application implements a principled approach to multiverse meta-analysis, allowing researchers to systematically explore how different analytical decisions affect their meta-analytic results."),

                        h4("The Multiverse Approach"),
                        p("Rather than making a single set of analytical choices, multiverse analysis explores all reasonable combinations of decisions across different dimensions (Voracek et al., 2021):"),
                        tags$ul(
                          tags$li(tags$strong("Which factors:"), " Decisions about which studies/effects to include (e.g., different populations, measures, conditions)"),
                          tags$li(tags$strong("How factors:"), " Decisions about analytical methods (e.g., meta-analytic estimators, dependency handling)")
                        ),

                        h4("Decision Framework (Del Giudice et al., 2021)"),
                        div(class = "decision-guide",
                            p(tags$strong("The framework categorizes analytical decisions into three types:")),
                            tags$ul(
                              tags$li(tags$strong("Non-equivalent (N):"), " Decisions that represent fundamentally different theoretical or methodological approaches. These create separate 'multiverses' because combining them would be inappropriate. ",
                                      tags$em("Example: Including vs. excluding unpublished studies represents different theoretical stances on publication bias.")),
                              tags$li(tags$strong("Equivalent (E):"), " Decisions that are theoretically interchangeable and represent different ways of operationalizing the same construct. All options should be explored in the same multiverse. ",
                                      tags$em("Example: Different measures of the same psychological construct.")),
                              tags$li(tags$strong("Uncertain (U):"), " Decisions where the equivalence status is unclear or debatable. Treated conservatively as equivalent for comprehensive exploration. ",
                                      tags$em("Example: Different statistical transformations where theoretical justification is ambiguous."))
                            ),
                            p(tags$strong("Guidance for Decision-Making:")),
                            tags$ul(
                              tags$li("Consider the ", tags$em("theoretical implications"), " of each choice"),
                              tags$li("Ask: 'Would combining these decisions answer fundamentally different research questions?'"),
                              tags$li("When in doubt between E and N, consider the ", tags$em("interpretability"), " of results"),
                              tags$li("Use domain expertise and existing literature to guide classifications")
                            )
                        ),

                        p(tags$strong("Reference:"), " Del Giudice, M., & Gangestad, S. W. (2021). ",
                          tags$a("A traveler's guide to the multiverse: Promises, pitfalls, and a framework for the evaluation of analytic decisions",
                                 href = "https://doi.org/10.1177/2515245920954925",
                                 target = "_blank",
                                 style = "color: #ea580c; text-decoration: underline;"), ". ",
                          tags$em("Advances in Methods and Practices in Psychological Science"), ", 4(1)."),
                        p("Voracek, M., Kossmeier, M., & Tran, U. S. (2019). ",
                          tags$a("Which data to meta-analyze, and how? A specification-curve and multiverse-analysis approach to meta-analysis",
                                 href = "https://doi.org/10.1027/2151-2604/a000357",
                                 target = "_blank",
                                 style = "color: #ea580c; text-decoration: underline;"), ". ",
                          tags$em("Zeitschrift für Psychologie"), ", 227(1), 64-82.")
                    )
                )
              ),
              fluidRow(
                box(title = "Upload Data", status = "primary", solidHeader = TRUE, width = 12,
                    fileInput("file", "Choose CSV File",
                              accept = c(".csv")),
                    helpText("Upload a CSV file with columns: study, es_id, yi, vi, and any 'wf_' columns"),

                    conditionalPanel(
                      condition = "output.data_uploaded",
                      h4("Data Preview:"),
                      DT::dataTableOutput("data_preview")
                    )
                )
              )
      ),

      # Configuration Tab
      tabItem(tabName = "config",
              fluidRow(
                box(title = "Which Factors Configuration", status = "info", solidHeader = TRUE, width = 6,
                    conditionalPanel(
                      condition = "output.data_uploaded",
                      h4("Configure Which Factors:"),
                      uiOutput("wf_config"),
                      br(),
                      h4("Select Meta-Analysis Methods:"),
                      div(
                        p("Choose the meta-analytic estimators to include in your multiverse:"),

                        div(
                          style = "border: 1px solid #ddd; padding: 15px; border-radius: 5px;",

                          # Basic Methods Row
                          fluidRow(
                            column(12,
                                   h5("Basic Methods", style = "color: #ea580c; margin-bottom: 10px;")
                            )
                          ),
                          fluidRow(
                            column(3,
                                   div(style = "display: flex; align-items: center; margin-bottom: 8px;",
                                       checkboxInput("method_fe", "Fixed Effects", value = F),
                                       actionButton("info_fe", "", icon = icon("info-circle"), class = "btn btn-xs btn-default",
                                                    style = "margin-left: 5px; padding: 2px 4px; font-size: 10px;")
                                   )
                            ),
                            column(3,
                                   div(style = "display: flex; align-items: center; margin-bottom: 8px;",
                                       checkboxInput("method_reml", "Random Effects (REML)", value = TRUE),
                                       actionButton("info_reml", "", icon = icon("info-circle"), class = "btn btn-xs btn-default",
                                                    style = "margin-left: 5px; padding: 2px 4px; font-size: 10px;")
                                   )
                            ),
                            column(3,
                                   div(style = "display: flex; align-items: center; margin-bottom: 8px;",
                                       checkboxInput("method_pm", "Paule-Mandel"),
                                       actionButton("info_pm", "", icon = icon("info-circle"), class = "btn btn-xs btn-default",
                                                    style = "margin-left: 5px; padding: 2px 4px; font-size: 10px;")
                                   )
                            ),
                            column(3,
                                   div(style = "display: flex; align-items: center; margin-bottom: 8px;",
                                       checkboxInput("method_hk_sj", "Hartung-Knapp/SJ"),
                                       actionButton("info_hk_sj", "", icon = icon("info-circle"), class = "btn btn-xs btn-default",
                                                    style = "margin-left: 5px; padding: 2px 4px; font-size: 10px;")
                                   )
                            )
                          ),

                          hr(style = "margin: 15px 0 10px 0;"),

                          # Dependency Methods Row
                          fluidRow(
                            column(12,
                                   h5("Dependency Handling", style = "color: #ea580c; margin-bottom: 10px;")
                            )
                          ),
                          fluidRow(
                            column(6,
                                   div(style = "display: flex; align-items: center; margin-bottom: 8px;",
                                       checkboxInput("method_3_level", "3-Level Meta-Analysis"),
                                       actionButton("info_3_level", "", icon = icon("info-circle"), class = "btn btn-xs btn-default",
                                                    style = "margin-left: 5px; padding: 2px 4px; font-size: 10px;")
                                   )
                            ),
                            column(6,
                                   div(style = "display: flex; align-items: center; margin-bottom: 8px;",
                                       checkboxInput("method_rve", "Robust Variance Estimation", value = TRUE),
                                       actionButton("info_rve", "", icon = icon("info-circle"), class = "btn btn-xs btn-default",
                                                    style = "margin-left: 5px; padding: 2px 4px; font-size: 10px;")
                                   )
                            )
                          ),

                          hr(style = "margin: 15px 0 10px 0;"),

                          # Publication Bias Methods Rows
                          fluidRow(
                            column(12,
                                   h5("Publication Bias Correction", style = "color: #ea580c; margin-bottom: 10px;")
                            )
                          ),
                          fluidRow(
                            column(4,
                                   div(style = "display: flex; align-items: center; margin-bottom: 8px;",
                                       checkboxInput("method_pet_peese", "PET-PEESE"),
                                       actionButton("info_pet_peese", "", icon = icon("info-circle"), class = "btn btn-xs btn-default",
                                                    style = "margin-left: 5px; padding: 2px 4px; font-size: 10px;")
                                   )
                            ),
                            column(4,
                                   div(style = "display: flex; align-items: center; margin-bottom: 8px;",
                                       checkboxInput("method_pet_peese_corr", "PET-PEESE (corrected)", value = TRUE),
                                       actionButton("info_pet_peese_corr", "", icon = icon("info-circle"), class = "btn btn-xs btn-default",
                                                    style = "margin-left: 5px; padding: 2px 4px; font-size: 10px;")
                                   )
                            ),
                            column(4,
                                   div(style = "display: flex; align-items: center; margin-bottom: 8px;",
                                       checkboxInput("method_puni_star", "p-uniform*", value = TRUE),
                                       actionButton("info_puni_star", "", icon = icon("info-circle"), class = "btn btn-xs btn-default",
                                                    style = "margin-left: 5px; padding: 2px 4px; font-size: 10px;")
                                   )
                            )
                          ),
                          fluidRow(
                            column(4,
                                   div(style = "display: flex; align-items: center; margin-bottom: 8px;",
                                       checkboxInput("method_uwls", "UWLS"),
                                       actionButton("info_uwls", "", icon = icon("info-circle", value = TRUE), class = "btn btn-xs btn-default",
                                                    style = "margin-left: 5px; padding: 2px 4px; font-size: 10px;")
                                   )
                            ),
                            column(4,
                                   div(style = "display: flex; align-items: center; margin-bottom: 8px;",
                                       checkboxInput("method_waap", "WAAP"),
                                       actionButton("info_waap", "", icon = icon("info-circle", value = TRUE), class = "btn btn-xs btn-default",
                                                    style = "margin-left: 5px; padding: 2px 4px; font-size: 10px;")
                                   )
                            ),
                            column(4,
                                   div(style = "display: flex; align-items: center; margin-bottom: 8px;",
                                       checkboxInput("method_bayesmeta", "Bayesmeta"),
                                       actionButton("info_bayesmeta", "", icon = icon("info-circle"), class = "btn btn-xs btn-default",
                                                    style = "margin-left: 5px; padding: 2px 4px; font-size: 10px;")
                                   )
                            )
                          )
                        )
                      ),
                      br(),
                      h4("Select Dependency Handling:"),
                      checkboxGroupInput("dependencies", "Dependencies:",
                                         choices = list(
                                           "Aggregate" = "aggregate",
                                           "Select Max" = "select_max",
                                           "Select Min" = "select_min",
                                           "Modeled" = "modeled"
                                         ),
                                         selected = "aggregate"),
                      br(),
                      numericInput("k_smallest", "Minimum studies threshold (k_smallest_ma):",
                                   value = 5, min = 1, max = 20, step = 1)
                    )
                ),

                box(title = "Run Analysis", status = "success", solidHeader = TRUE, width = 6,
                    conditionalPanel(
                      condition = "output.data_uploaded",
                      br(),
                      actionButton("create_specs", "Create Specifications",
                                   class = "btn-primary", style = "margin-bottom: 10px;"),
                      br(),
                      conditionalPanel(
                        condition = "output.specs_created",
                        h4("Specifications Summary:"),
                        verbatimTextOutput("specs_summary"),
                        br(),
                        actionButton("run_analysis", "Run Multiverse Analysis",
                                     class = "btn-success")
                      )
                    )
                )
              )
      ),

      # Results Tab
      tabItem(tabName = "results",
              fluidRow(
                box(title = "Analysis Results", status = "success", solidHeader = TRUE, width = 12,
                    conditionalPanel(
                      condition = "output.analysis_complete",
                      h4("Results Summary:"),
                      uiOutput("results_summary"),
                      br(),
                      tabsetPanel(
                        tabPanel("Specification Curve",
                                 plotlyOutput("spec_curve", height = "700px")),
                        tabPanel("Vibration of Effects",
                                 fluidRow(
                                   column(3,
                                          wellPanel(
                                            h5("VoE Plot Settings"),
                                            numericInput("voe_cutoff", "Minimum studies:",
                                                         value = 10, min = 1, max = 50, step = 1),
                                            numericInput("voe_hline", "P-value threshold:",
                                                         value = 0.05, min = 0.001, max = 1, step = 0.01),
                                            checkboxInput("voe_colorblind", "Colorblind friendly", value = TRUE),
                                            br(),
                                            actionButton("update_voe", "Update Plot", class = "btn-primary")
                                          )
                                   ),
                                   column(9,
                                          plotlyOutput("voe_plot", height = "600px")
                                   )
                                 )),
                        tabPanel("Results Table",
                                 DT::dataTableOutput("results_table")),
                        tabPanel("Warnings",
                                 verbatimTextOutput("warnings_output"))
                      )
                    )
                )
              )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {

  # Reactive values
  values <- reactiveValues(
    data = NULL,
    specifications = NULL,
    results = NULL,
    decision_map = NULL
  )

  # Create reactive for selected methods
  selected_methods <- reactive({
    methods <- character(0)
    if (input$method_fe) methods <- c(methods, "fe")
    if (input$method_reml) methods <- c(methods, "reml")
    if (input$method_pm) methods <- c(methods, "pm")
    if (input$method_hk_sj) methods <- c(methods, "hk_sj")
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

  # Info button modal handlers
  # Info button modal handlers
  observeEvent(input$info_fe, {
    showModal(modalDialog(
      title = "Fixed Effects Meta-Analysis",
      div(style = "color: #1f2937; background-color: #ffffff;",
          HTML("<p><strong>What it is:</strong> Assumes all studies estimate the same true effect size (no between-study heterogeneity).</p>
             <p><strong>When to use:</strong> When studies are very similar and you expect minimal heterogeneity.</p>
             <p><strong>Implementation:</strong> Uses <code>metafor::rma(method='FE')</code> with inverse-variance weighting.</p>
             <p><strong>Interpretation:</strong> Provides the most precise estimate when homogeneity assumption holds, but may be overly narrow if heterogeneity exists.</p>")),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })

  observeEvent(input$info_reml, {
    showModal(modalDialog(
      title = "Random Effects (REML) Meta-Analysis",
      div(style = "color: #1f2937; background-color: #ffffff;",
          HTML("<p><strong>What it is:</strong> Assumes effect sizes vary across studies due to random between-study differences.</p>
             <p><strong>When to use:</strong> When studies differ in populations, methods, or settings (most common case).</p>
             <p><strong>Implementation:</strong> Uses <code>metafor::rma(method='REML')</code> with restricted maximum likelihood estimation of τ².</p>
             <p><strong>Interpretation:</strong> Accounts for heterogeneity, providing wider confidence intervals that reflect between-study variation.</p>")),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })

  observeEvent(input$info_pm, {
    showModal(modalDialog(
      title = "Paule-Mandel Estimator",
      div(style = "color: #1f2937; background-color: #ffffff;",
          HTML("<p><strong>What it is:</strong> Method-of-moments estimator for between-study variance (τ²).</p>
             <p><strong>When to use:</strong> Alternative to REML when you want a non-iterative heterogeneity estimator.</p>
             <p><strong>Implementation:</strong> Uses <code>metafor::rma(method='PM')</code> with Paule-Mandel τ² estimation.</p>
             <p><strong>Interpretation:</strong> Often gives similar results to REML but uses different mathematical approach for τ² estimation.</p>")),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })

  observeEvent(input$info_hk_sj, {
    showModal(modalDialog(
      title = "Hartung-Knapp/Sidik-Jonkman Method",
      div(style = "color: #1f2937; background-color: #ffffff;",
          HTML("<p><strong>What it is:</strong> Uses improved confidence interval calculation with t-distribution and Sidik-Jonkman τ² estimator.</p>
             <p><strong>When to use:</strong> When you want more conservative confidence intervals, especially with few studies.</p>
             <p><strong>Implementation:</strong> Uses <code>meta::metagen()</code> with HK confidence intervals and SJ heterogeneity estimator.</p>
             <p><strong>Interpretation:</strong> Generally provides wider, more appropriate confidence intervals than standard random effects.</p>")),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })

  observeEvent(input$info_3_level, {
    showModal(modalDialog(
      title = "3-Level Meta-Analysis",
      div(style = "color: #1f2937; background-color: #ffffff;",
          HTML("<p><strong>What it is:</strong> Multilevel model accounting for nesting of effect sizes within studies.</p>
             <p><strong>When to use:</strong> When studies contribute multiple effect sizes (dependency due to clustering).</p>
             <p><strong>Implementation:</strong> Uses <code>metafor::rma.mv()</code> with random effects for effect sizes and studies.</p>
             <p><strong>Interpretation:</strong> Accounts for both within-study and between-study variance components.</p>")),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })

  observeEvent(input$info_rve, {
    showModal(modalDialog(
      title = "Robust Variance Estimation (RVE)",
      div(style = "color: #1f2937; background-color: #ffffff;",
          HTML("<p><strong>What it is:</strong> 3-level model with robust standard errors using cluster-robust variance estimation.</p>
             <p><strong>When to use:</strong> When you have dependent effect sizes and want robust standard errors.</p>
             <p><strong>Implementation:</strong> Uses <code>metafor::rma.mv()</code> followed by <code>metafor::robust()</code> with clubSandwich.</p>
             <p><strong>Interpretation:</strong> More conservative standard errors that account for model misspecification.</p>")),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })

  observeEvent(input$info_pet_peese, {
    showModal(modalDialog(
      title = "PET-PEESE Method",
      div(style = "color: #1f2937; background-color: #ffffff;",
          HTML("<p><strong>What it is:</strong> Precision-Effect Test and Precision-Effect Estimate with Standard Error - corrects for small-study effects.</p>
             <p><strong>When to use:</strong> When you suspect publication bias or small-study effects.</p>
             <p><strong>Implementation:</strong> Regression of effect size on √(variance) (PET) or variance (PEESE). Uses PEESE if PET p < 0.10.</p>
             <p><strong>Interpretation:</strong> Intercept estimates the effect size corrected for small-study bias. May overcorrect in some cases.</p>")),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })

  observeEvent(input$info_pet_peese_corr, {
    showModal(modalDialog(
      title = "PET-PEESE (Corrected)",
      div(style = "color: #1f2937; background-color: #ffffff;",
          HTML("<p><strong>What it is:</strong> PET-PEESE with correction - sets negative estimates to zero.</p>
             <p><strong>When to use:</strong> When using PET-PEESE but effect size cannot be negative by definition.</p>
             <p><strong>Implementation:</strong> Same as PET-PEESE, but if corrected estimate < 0, sets it to 0.</p>
             <p><strong>Interpretation:</strong> Prevents unrealistic negative effect size estimates while maintaining bias correction.</p>")),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })

  observeEvent(input$info_puni_star, {
    showModal(modalDialog(
      title = "p-uniform* Method",
      div(style = "color: #1f2937; background-color: #ffffff;",
          HTML("<p><strong>What it is:</strong> Publication bias correction method using the distribution of p-values.</p>
             <p><strong>When to use:</strong> When you suspect publication bias and have studies with different sample sizes.</p>
             <p><strong>Implementation:</strong> Uses <code>puniform::puni_star()</code> with right-sided testing.</p>
             <p><strong>Interpretation:</strong> Estimates effect size corrected for publication bias by modeling p-value distribution.</p>")),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })

  observeEvent(input$info_uwls, {
    showModal(modalDialog(
      title = "Unweighted Least Squares (UWLS)",
      div(style = "color: #1f2937; background-color: #ffffff;",
          HTML("<p><strong>What it is:</strong> Regression-based method that regresses standardized effect sizes on precision.</p>
             <p><strong>When to use:</strong> As alternative bias correction method, especially with small-study effects.</p>
             <p><strong>Implementation:</strong> Regresses d/se on 1/se using OLS. Intercept estimates bias-corrected effect.</p>
             <p><strong>Interpretation:</strong> Provides effect size estimate adjusted for relationship between effect size and precision.</p>")),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })

  observeEvent(input$info_waap, {
    showModal(modalDialog(
      title = "Weighted Average of Adequately Powered (WAAP)",
      div(style = "color: #1f2937; background-color: #ffffff;",
          HTML("<p><strong>What it is:</strong> Uses only studies with adequate power (SE < |UWLS estimate|/2.8) for final estimate.</p>
             <p><strong>When to use:</strong> When you want to focus on well-powered studies to avoid small-study bias.</p>
             <p><strong>Implementation:</strong> First estimates UWLS, then re-estimates using only adequately powered studies.</p>
             <p><strong>Interpretation:</strong> Conservative approach focusing on studies most likely to detect true effects.</p>")),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })

  observeEvent(input$info_bayesmeta, {
    showModal(modalDialog(
      title = "Bayesian Meta-Analysis",
      div(style = "color: #1f2937; background-color: #ffffff;",
          HTML("<p><strong>What it is:</strong> Bayesian random effects meta-analysis with non-informative priors.</p>
             <p><strong>When to use:</strong> When you want Bayesian inference or credible intervals instead of p-values.</p>
             <p><strong>Implementation:</strong> Uses <code>bayesmeta::bayesmeta()</code> with default priors.</p>
             <p><strong>Interpretation:</strong> Provides posterior median and credible intervals. No p-values (uses Bayesian framework).</p>")),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })

  # Auto-load example data on startup
  observe({
    if (is.null(values$data)) {
      # Try to load example data from your package
      tryCatch({
        # If data_tiny exists in your package
        if (exists("data_tiny")) {
          values$data <- data_tiny
          showNotification("Example dataset loaded! Upload your own CSV to replace it.",
                           type = "message", duration = 5)
        }
      }, error = function(e) {
        # Fallback: create minimal example data
        values$data <- data.frame(
          study = rep(paste0("Study_", 1:10), each = 2),
          es_id = 1:20,
          yi = rnorm(20, mean = 0.3, sd = 0.2),
          vi = runif(20, min = 0.01, max = 0.1),
          wf_population = rep(c("adults", "children"), times = 10),
          wf_measure = rep(c("self_report", "behavioral"), each = 10),
          stringsAsFactors = FALSE
        )
        showNotification("Example dataset created! Upload your own CSV to replace it.",
                         type = "message", duration = 5)
      })
    }
  })

  # Data upload
  observeEvent(input$file, {
    req(input$file)

    values$data <- read.csv(input$file$datapath, stringsAsFactors = FALSE)

    # Validate required columns
    required_cols <- c("study", "es_id", "yi", "vi")
    if (!all(required_cols %in% names(values$data))) {
      showNotification("Missing required columns: study, es_id, yi, vi",
                       type = "error", duration = 5)
      values$data <- NULL
      return()
    }

    showNotification("Data uploaded successfully!", type = "message", duration = 3)
  })

  # Check if data is uploaded
  output$data_uploaded <- reactive({
    !is.null(values$data)
  })
  outputOptions(output, "data_uploaded", suspendWhenHidden = FALSE)

  # Data preview
  output$data_preview <- DT::renderDataTable({
    req(values$data)
    DT::datatable(head(values$data, 100), options = list(scrollX = TRUE))
  })

  # Dynamic UI for which factors configuration

  # Dynamic UI for column mapping and configuration (combined)
  output$wf_config <- renderUI({
    req(values$data)

    # Get all non-required columns (potential which factors)
    required_cols <- c("study", "es_id", "yi", "vi")
    potential_wf_cols <- names(values$data)[!names(values$data) %in% required_cols]

    if (length(potential_wf_cols) == 0) {
      return(div(
        class = "alert alert-warning",
        h4("No potential which factors found"),
        p("Your data only contains the required columns (study, es_id, yi, vi)."),
        p("To use multiverse analysis, add columns representing different analytical choices.")
      ))
    }

    # Create combined interface
    div(
      h4("Configure Which Factors"),
      p("Select columns from your data to use as 'which factors' and set their decision types:"),

      # Dynamic number of which factors
      fluidRow(
        column(6,
               numericInput("n_wf_factors", "Number of which factors to use:",
                            value = min(3, length(potential_wf_cols)),
                            min = 0, max = length(potential_wf_cols), step = 1)
        ),
        column(6,
               br(),
               actionButton("update_wf_mapping", "Update Configuration", class = "btn-primary")
        )
      ),

      hr(),

      # Combined column mapping and decision UI
      uiOutput("combined_wf_config")
    )
  })

  # Generate combined configuration inputs
  output$combined_wf_config <- renderUI({
    req(values$data, input$n_wf_factors)

    if (input$n_wf_factors == 0) {
      return(div(
        class = "alert alert-info",
        p("No which factors selected. Multiverse analysis will only vary across meta-analysis methods and dependency handling.")
      ))
    }

    required_cols <- c("study", "es_id", "yi", "vi")
    potential_cols <- names(values$data)[!names(values$data) %in% required_cols]

    # Create combined inputs for each which factor
    lapply(1:input$n_wf_factors, function(i) {
      div(
        style = "border: 1px solid #ddd; padding: 15px; margin: 10px 0; border-radius: 8px;",

        h5(paste("Which Factor", i)),

        fluidRow(
          # Column selection
          column(4,
                 selectInput(paste0("wf_", i, "_column"),
                             "Select Column:",
                             choices = c("None" = "", potential_cols),
                             selected = if (i <= length(potential_cols)) potential_cols[i] else "")
          ),

          # Decision type
          column(4,
                 conditionalPanel(
                   condition = paste0("input.wf_", i, "_column != ''"),
                   radioButtons(paste0("decision_wf_", i),
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
          ),

          # Column preview
          column(4,
                 conditionalPanel(
                   condition = paste0("input.wf_", i, "_column != ''"),
                   div(style = "margin-top: 10px;",
                       strong("Unique Values:"),
                       br(),
                       textOutput(paste0("wf_", i, "_preview"))
                   )
                 )
          )
        ),

        # Decision guide (shown when column is selected)
        conditionalPanel(
          condition = paste0("input.wf_", i, "_column != ''"),
          div(class = "decision-guide", style = "font-size: 0.9em; margin-top: 10px;",
              tags$strong("Decision Guide:"),
              tags$ul(
                tags$li(tags$strong("E (Equivalent):"), " Different ways of measuring the same construct"),
                tags$li(tags$strong("N (Non-equivalent):"), " Fundamentally different approaches (creates separate multiverses)"),
                tags$li(tags$strong("U (Uncertain):"), " Unclear equivalence (treated as E)"),
                tags$li(tags$strong("Ignore:"), " Exclude from multiverse entirely")
              )
          )
        )
      )
    })
  })

  # Generate column previews (same as before)
  observe({
    req(values$data, input$n_wf_factors)

    for (i in 1:input$n_wf_factors) {
      local({
        ii <- i
        output[[paste0("wf_", ii, "_preview")]] <- renderText({
          col_name <- input[[paste0("wf_", ii, "_column")]]
          if (!is.null(col_name) && col_name != "" && col_name %in% names(values$data)) {
            unique_vals <- unique(values$data[[col_name]])
            if (length(unique_vals) > 4) {
              paste(c(head(unique_vals, 4), "..."), collapse = ", ")
            } else {
              paste(unique_vals, collapse = ", ")
            }
          } else {
            ""
          }
        })
      })
    }
  })


  # Generate column mapping inputs
  output$column_mapping_ui <- renderUI({
    req(values$data, input$n_wf_factors)

    if (input$n_wf_factors == 0) {
      return(p("No which factors selected. Multiverse analysis will use only meta-analysis methods."))
    }

    required_cols <- c("study", "es_id", "yi", "vi")
    potential_cols <- names(values$data)[!names(values$data) %in% required_cols]

    # Create selection inputs for each which factor
    lapply(1:input$n_wf_factors, function(i) {
      fluidRow(
        column(6,
               selectInput(paste0("wf_", i, "_column"),
                           paste("Which Factor", i, "- Select Column:"),
                           choices = c("None" = "", potential_cols),
                           selected = if (i <= length(potential_cols)) potential_cols[i] else "")
        ),
        column(6,
               # Show preview of selected column
               conditionalPanel(
                 condition = paste0("input.wf_", i, "_column != ''"),
                 div(style = "margin-top: 25px;",
                     strong("Values: "),
                     textOutput(paste0("wf_", i, "_preview"), inline = TRUE)
                 )
               )
        )
      )
    })
  })

  # Generate decision configuration UI
  output$decision_config_ui <- renderUI({
    req(values$data, input$n_wf_factors)

    if (input$n_wf_factors == 0) return(NULL)

    # Get currently selected columns
    selected_columns <- sapply(1:input$n_wf_factors, function(i) {
      input[[paste0("wf_", i, "_column")]]
    })

    # Filter out empty selections
    selected_columns <- selected_columns[selected_columns != "" & !is.null(selected_columns)]

    if (length(selected_columns) == 0) {
      return(p("Please select columns for your which factors above."))
    }

    # Create decision type inputs for selected columns
    lapply(seq_along(selected_columns), function(i) {
      col_name <- selected_columns[i]
      unique_values <- unique(values$data[[col_name]])
      unique_values_text <- if(length(unique_values) > 6) {
        paste(c(head(unique_values, 6), "..."), collapse = ", ")
      } else {
        paste(unique_values, collapse = ", ")
      }

      div(
        h5(paste("Configure", col_name, "as Which Factor", i, ":")),
        p(tags$strong("Unique values (", length(unique_values), "):"), unique_values_text),
        radioButtons(paste0("decision_wf_", i),
                     label = paste("Decision type for", col_name),
                     choices = list(
                       "Non-equivalent (N)" = "N",
                       "Equivalent (E)" = "E",
                       "Uncertain (U)" = "U",
                       "Ignore (exclude from multiverse)" = "IGNORE"
                     ),
                     selected = "IGNORE",
                     inline = TRUE),
        div(class = "decision-guide", style = "font-size: 0.9em; margin-top: 5px;",
            tags$strong("Quick Guide:"),
            tags$ul(
              tags$li(tags$strong("N:"), " Fundamental differences (separate multiverses)"),
              tags$li(tags$strong("E:"), " Different ways of measuring the same thing"),
              tags$li(tags$strong("U:"), " Unclear equivalence (treated as E)"),
              tags$li(tags$strong("Ignore:"), " Exclude this factor entirely")
            )
        ),
        hr()
      )
    })
  })

  # Generate column previews
  observe({
    req(values$data, input$n_wf_factors)

    for (i in 1:input$n_wf_factors) {
      local({
        ii <- i
        output[[paste0("wf_", ii, "_preview")]] <- renderText({
          col_name <- input[[paste0("wf_", ii, "_column")]]
          if (!is.null(col_name) && col_name != "" && col_name %in% names(values$data)) {
            unique_vals <- unique(values$data[[col_name]])
            if (length(unique_vals) > 4) {
              paste(c(head(unique_vals, 4), "..."), collapse = ", ")
            } else {
              paste(unique_vals, collapse = ", ")
            }
          } else {
            ""
          }
        })
      })
    }
  })

  # Create specifications using dynamic mapping
  observeEvent(input$create_specs, {
    req(values$data, input$dependencies, input$n_wf_factors)

    if (input$n_wf_factors == 0) {
      # No which factors - just methods and dependencies
      tryCatch({
        specs_result <- create_principled_multiverse_specifications(
          data = values$data,
          wf_vars = character(0),
          ma_methods = selected_methods(),
          dependencies = input$dependencies,
          decision_map = setNames(character(0), character(0))
        )

        values$specifications <- specs_result$specifications
        showNotification(paste("Created", specs_result$number_specs, "specifications (methods only)"),
                         type = "message", duration = 3)
      }, error = function(e) {
        showNotification(paste("Error creating specifications:", e$message),
                         type = "error", duration = 5)
      })
      return()
    }

    # Get selected columns and create mapping
    selected_columns <- sapply(1:input$n_wf_factors, function(i) {
      input[[paste0("wf_", i, "_column")]]
    })

    # Filter out empty selections
    selected_columns <- selected_columns[selected_columns != "" & !is.null(selected_columns)]

    if (length(selected_columns) == 0) {
      showNotification("Please select at least one column for which factors.",
                       type = "error", duration = 5)
      return()
    }

    # Create temporary wf_ columns in data
    temp_data <- values$data
    for (i in seq_along(selected_columns)) {
      temp_data[[paste0("wf_", i)]] <- temp_data[[selected_columns[i]]]
    }

    # Get decisions for active factors
    active_decisions <- character(0)
    for (i in seq_along(selected_columns)) {
      decision <- input[[paste0("decision_wf_", i)]]
      if (!is.null(decision) && decision != "IGNORE") {
        active_decisions[paste0("wf_", i)] <- decision
      }
    }

    if (length(active_decisions) == 0) {
      showNotification("No active which factors found! All factors are set to 'Ignore'.",
                       type = "error", duration = 5)
      return()
    }

    tryCatch({
      specs_result <- create_principled_multiverse_specifications(
        data = temp_data,
        wf_vars = names(active_decisions),
        ma_methods = selected_methods(),
        dependencies = input$dependencies,
        decision_map = active_decisions
      )

      values$specifications <- specs_result$specifications
      values$data <- temp_data  # Update data with wf_ columns

      showNotification(paste("Created", specs_result$number_specs, "specifications"),
                       type = "message", duration = 3)

    }, error = function(e) {
      showNotification(paste("Error creating specifications:", e$message),
                       type = "error", duration = 5)
    })
  })

  # Check if specs are created
  output$specs_created <- reactive({
    !is.null(values$specifications)
  })
  outputOptions(output, "specs_created", suspendWhenHidden = FALSE)

  # Specifications summary
  output$specs_summary <- renderText({
    req(values$specifications)

    # Count active vs ignored factors
    all_wf_cols <- names(values$data)[grepl("^wf_", names(values$data))]
    active_factors <- all_wf_cols[sapply(all_wf_cols, function(col) {
      decision <- input[[paste0("decision_", col)]]
      !is.null(decision) && decision != "IGNORE"
    })]
    ignored_factors <- all_wf_cols[sapply(all_wf_cols, function(col) {
      decision <- input[[paste0("decision_", col)]]
      !is.null(decision) && decision == "IGNORE"
    })]

    summary_text <- paste("Total specifications:", nrow(values$specifications),
                          "\nUnique multiverses:", length(unique(values$specifications$multiverse_id)),
                          "\nMA methods:", paste(unique(values$specifications$ma_method), collapse = ", "),
                          "\nDependencies:", paste(unique(values$specifications$dependency), collapse = ", "),
                          "\nActive which factors:", paste(active_factors, collapse = ", "))

    if (length(ignored_factors) > 0) {
      summary_text <- paste(summary_text,
                            "\nIgnored which factors:", paste(ignored_factors, collapse = ", "))
    }

    summary_text
  })

  # Run analysis using your package function
  observeEvent(input$run_analysis, {
    req(values$data, values$specifications, input$k_smallest)

    # Set the k_smallest_ma option
    options(metaMultiverse.k_smallest_ma = input$k_smallest)

    showNotification("Running multiverse analysis... This may take a while.",
                     type = "message", duration = NULL, id = "analysis_msg")

    tryCatch({
      # Use your actual package function
      values$results <- run_multiverse_analysis(
        data = values$data,
        specifications = values$specifications,
        verbose = TRUE
      )

      removeNotification("analysis_msg")
      showNotification("Analysis completed!", type = "message", duration = 3)

    }, error = function(e) {
      removeNotification("analysis_msg")
      showNotification(paste("Error running analysis:", e$message),
                       type = "error", duration = 5)
      print(e)  # For debugging
    })
  })

  # Check if analysis is complete
  output$analysis_complete <- reactive({
    !is.null(values$results) && !is.null(values$results$results)
  })
  outputOptions(output, "analysis_complete", suspendWhenHidden = FALSE)

  # Results summary
  # Enhanced results summary
  output$results_summary <- renderUI({
    req(values$results)

    if (is.null(values$results$results) || nrow(values$results$results) == 0) {
      return(div(class = "alert alert-warning", "No valid results generated. Check warnings for details."))
    }

    data <- values$results$results

    # Overall summary statistics
    total_specs <- nrow(data)
    n_multiverses <- length(unique(data$multiverse_id))
    mean_effect <- round(mean(data$b, na.rm = TRUE), 3)
    effect_range <- c(round(min(data$b, na.rm = TRUE), 3), round(max(data$b, na.rm = TRUE), 3))
    mean_k <- round(mean(data$k, na.rm = TRUE), 1)

    # Get which factors for group analysis
    wf_cols <- names(data)[grepl("^wf_", names(data))]

    # Create summary by which factors if they exist
    if (length(wf_cols) > 0) {
      group_summary <- data %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(wf_cols))) %>%
        dplyr::summarise(
          n_specs = dplyr::n(),
          mean_b = round(mean(b, na.rm = TRUE), 3),
          mean_ci_lb = round(mean(ci.lb, na.rm = TRUE), 3),
          mean_ci_ub = round(mean(ci.ub, na.rm = TRUE), 3),
          mean_k = round(mean(k, na.rm = TRUE), 1),
          .groups = "drop"
        )

      # Create a nice formatted table
      group_table <- DT::datatable(
        group_summary,
        options = list(
          dom = 't',  # Only show table, no search/pagination
          pageLength = -1,
          scrollX = TRUE,
          columnDefs = list(
            list(className = 'dt-center', targets = '_all')
          )
        ),
        colnames = c(
          wf_cols,
          "N Specs", "Mean Effect", "Mean CI Lower", "Mean CI Upper", "Mean K"
        ),
        rownames = FALSE
      ) %>%
        DT::formatRound(c("mean_b", "mean_ci_lb", "mean_ci_ub", "mean_k"), 3)
    }

    # Create the UI
    fluidRow(
      # Overall Summary Cards
      column(3,
             div(class = "info-box",
                 h4("📊 Total Specifications"),
                 h2(style = "margin: 0; color: #ea580c;", total_specs),
                 p(style = "margin: 5px 0;", paste("Across", n_multiverses, "multiverse(s)"))
             )
      ),
      column(3,
             div(class = "info-box",
                 h4("🎯 Mean Effect Size"),
                 h2(style = "margin: 0; color: #ea580c;", mean_effect),
                 p(style = "margin: 5px 0;", paste("Range:", effect_range[1], "to", effect_range[2]))
             )
      ),
      column(3,
             div(class = "info-box",
                 h4("📚 Average Studies"),
                 h2(style = "margin: 0; color: #ea580c;", mean_k),
                 p(style = "margin: 5px 0;", "Studies per analysis")
             )
      ),
      column(3,
             div(class = "info-box",
                 h4("🔍 Significant Results"),
                 h2(style = "margin: 0; color: #ea580c;",
                    paste0(round(100 * mean(data$pval < 0.05, na.rm = TRUE), 1), "%")),
                 p(style = "margin: 5px 0;", paste(sum(data$pval < 0.05, na.rm = TRUE), "of", total_specs, "specs"))
             )
      ),

      # Group Summary Table (if which factors exist)
      if (length(wf_cols) > 0) {
        column(12,
               br(),
               div(class = "info-box",
                   h4("📋 Summary by Which Factors"),
                   DT::dataTableOutput("group_summary_table", height = "300px")
               )
        )
      }
    )
  })

  # Render the group summary table separately
  output$group_summary_table <- DT::renderDataTable({
    req(values$results)

    if (is.null(values$results$results) || nrow(values$results$results) == 0) {
      return(NULL)
    }

    data <- values$results$results
    wf_cols <- names(data)[grepl("^wf_", names(data))]

    if (length(wf_cols) == 0) {
      return(NULL)
    }

    group_summary <- data %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(wf_cols))) %>%
      dplyr::summarise(
        n_specs = dplyr::n(),
        mean_b = round(mean(b, na.rm = TRUE), 3),
        mean_ci_lb = round(mean(ci.lb, na.rm = TRUE), 3),
        mean_ci_ub = round(mean(ci.ub, na.rm = TRUE), 3),
        mean_k = round(mean(k, na.rm = TRUE), 1),
        pct_sig = round(100 * mean(pval < 0.05, na.rm = TRUE), 1),
        .groups = "drop"
      )

    # Create nice column names
    col_names <- c(
      wf_cols,
      "N Specs", "Mean Effect", "CI Lower", "CI Upper", "Mean K", "% Significant"
    )

    DT::datatable(
      group_summary,
      options = list(
        dom = 't',
        pageLength = -1,
        scrollX = TRUE,
        columnDefs = list(
          list(className = 'dt-center', targets = '_all')
        )
      ),
      colnames = col_names,
      rownames = FALSE
    ) %>%
      DT::formatRound(c("mean_b", "mean_ci_lb", "mean_ci_ub", "mean_k"), 3) %>%
      DT::formatRound("pct_sig", 1)
  }, server = FALSE)

  # Specification curve plot using your package function
  output$spec_curve <- renderPlotly({
    req(values$results)

    if (is.null(values$results$results) || nrow(values$results$results) == 0) {
      return(plotly_empty() %>% add_annotations(text = "No results to display",
                                                showarrow = FALSE))
    }

    tryCatch({
      # Create dynamic factor label lookup
      wf_cols <- names(values$results$results)[grepl("^wf_", names(values$results$results))]
      factor_label_lookup <- setNames(
        paste("Factor", seq_along(wf_cols)),
        wf_cols
      )
      factor_label_lookup[["ma_method"]] <- "Meta-Analysis Method"
      factor_label_lookup[["dependency"]] <- "Dependency Handling"

      # Use your actual plotting function
      plotly_descriptive_spec_curve(
        data = values$results$results,
        factor_label_lookup = factor_label_lookup,
        colorblind_friendly = TRUE,
        interactive = TRUE
      )

    }, error = function(e) {
      # Fallback to simple plot if your function fails
      data <- values$results$results
      data$x_rank <- rank(data$b)

      plot_ly(data, x = ~x_rank, y = ~b, type = 'scatter', mode = 'lines+markers',
              error_y = list(array = ~(ci.ub - b), arrayminus = ~(b - ci.lb)),
              text = ~paste("Method:", ma_method, "<br>Effect:", round(b, 3),
                            "<br>p-value:", round(pval, 3)),
              hovertemplate = "%{text}<extra></extra>") %>%
        layout(title = "Specification Curve (Fallback)",
               xaxis = list(title = "Specification Rank"),
               yaxis = list(title = "Effect Size"))
    })
  })

  # Vibration of Effects plot
  output$voe_plot <- renderPlotly({
    req(values$results)

    if (is.null(values$results$results) || nrow(values$results$results) == 0) {
      return(plotly_empty() %>% add_annotations(text = "No results to display",
                                                showarrow = FALSE))
    }

    tryCatch({
      # Use your actual VoE plotting function
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
      # Fallback to simple scatter plot if VoE function fails
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

  # Update VoE plot when button is clicked
  observeEvent(input$update_voe, {
    output$voe_plot <- renderPlotly({
      req(values$results)

      if (is.null(values$results$results) || nrow(values$results$results) == 0) {
        return(plotly_empty() %>% add_annotations(text = "No results to display",
                                                  showarrow = FALSE))
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

  # Results table
  output$results_table <- DT::renderDataTable({
    req(values$results)

    if (is.null(values$results$results) || nrow(values$results$results) == 0) {
      return(DT::datatable(data.frame(Message = "No results to display")))
    }

    DT::datatable(values$results$results,
                  options = list(scrollX = TRUE, pageLength = 25)) %>%
      DT::formatRound(c("b", "ci.lb", "ci.ub", "pval"), 3)
  })

  # Warnings output
  output$warnings_output <- renderText({
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
