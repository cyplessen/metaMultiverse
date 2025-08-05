# ==============================================================================
# UI_MODULES/LANDING_PAGE.R
# ==============================================================================

#' Landing Page UI Module
landing_page_ui <- function() {
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
                         p("Traditional meta-analyses rely on single analytical choices that may influence results. Our multiverse approach systematically explores all reasonable analytical decisions, showing exactly how robust your findings are to different methodological choices.",
                           class = "feature-description")
                     )
              ),
              column(4,
                     div(class = "feature-card",
                         div("📊", class = "feature-icon"),
                         h4("Comprehensive Analysis", class = "feature-title"),
                         p("Rather than choosing a single meta-analytic method, explore multiple estimators simultaneously - from basic fixed and random effects to advanced techniques like robust variance estimation and publication bias corrections.",
                           class = "feature-description")
                     )
              ),
              column(4,
                     div(class = "feature-card",
                         div("🎯", class = "feature-icon"),
                         h4("Credible Results", class = "feature-title"),
                         p("Address the growing concern about researcher degrees of freedom in meta-analysis. By pre-specifying and systematically testing multiple analytical approaches, you demonstrate that your findings aren't dependent on arbitrary choices.",
                           class = "feature-description")
                     )
              )
            ),

            # Call to action
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
