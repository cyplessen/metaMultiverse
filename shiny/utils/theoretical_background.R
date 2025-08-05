# ==============================================================================
# UTILS/THEORETICAL_BACKGROUND.R
# ==============================================================================

#' Get Theoretical Background UI
get_theoretical_background_ui <- function() {
  tagList(
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
  )
}

#' Get Package Info UI
get_package_info_ui <- function() {
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
}
