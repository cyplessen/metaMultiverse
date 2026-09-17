# ABOUTME: Reporting layer: auto-generated warrant + fixed-decision tables,
# ABOUTME: grid report with anti-probabilistic defaults (justified-count
# ABOUTME: headline, labeled descriptives), preregisterable config export.

#' Auto-generated warrant table for a domain config
#'
#' Every fork (and eligibility filter, and deliberately fixed decision)
#' with its decision type, family, levels, justification basis and text --
#' the machine-readable warrant layer rendered into reports.
#'
#' @param config An \code{mv_domain_config}.
#' @return Data frame: element, name, decision, family, levels, basis,
#'   justification, citation.
#' @export
warrant_table <- function(config) {
  stopifnot(inherits(config, "mv_domain_config"))
  rows <- list()
  for (f in config$forks) {
    rows[[length(rows) + 1]] <- data.frame(
      element = "fork", name = f$name, decision = f$decision,
      family = f$family,
      levels = if (is.null(f$levels)) "<adaptive>" else
        paste(names(f$levels), collapse = ", "),
      basis = f$justification$basis,
      justification = f$justification$text,
      citation = f$justification$citation %||% NA_character_,
      stringsAsFactors = FALSE
    )
  }
  for (nm in names(config$eligibility)) {
    el <- config$eligibility[[nm]]
    rows[[length(rows) + 1]] <- data.frame(
      element = "eligibility", name = nm, decision = "fixed",
      family = NA_character_, levels = el$expr,
      basis = el$justification$basis,
      justification = el$justification$text,
      citation = el$justification$citation %||% NA_character_,
      stringsAsFactors = FALSE
    )
  }
  for (nm in names(config$fixed_decisions)) {
    fd <- config$fixed_decisions[[nm]]
    rows[[length(rows) + 1]] <- data.frame(
      element = "fixed_decision", name = nm, decision = "fixed",
      family = NA_character_, levels = as.character(fd$value),
      basis = fd$justification$basis,
      justification = fd$justification$text,
      citation = fd$justification$citation %||% NA_character_,
      stringsAsFactors = FALSE
    )
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Generate the multiverse report for a grid result
#'
#' Markdown report implementing the reporting constraints:
#' purpose declaration up front; the JUSTIFIED specification count as the
#' headline number (never raw combinatorial size); per-cell effect
#' summaries only; between-estimand divergence labeled as such; descriptive
#' frequencies labeled descriptive and paired with the decomposition;
#' worst-universe audit section; warrant and fixed-decision tables; full
#' attrition. Returns a character vector of markdown lines
#' (Quarto-renderable).
#'
#' @param result An \code{mv_grid_result}.
#' @param purpose Purpose declaration (default: the project stance).
#' @param mid Minimally important difference for the labeled MID
#'   descriptive.
#' @return Character vector of markdown lines.
#' @export
report_grid_result <- function(result,
                               purpose = paste(
                                 "Diagnostic-inferential about divergence",
                                 "among defensible specifications;",
                                 "explicitly not persuasion toward a pooled",
                                 "effect estimate."),
                               mid = 0.24) {
  stopifnot(inherits(result, "mv_grid_result"))
  config <- attr(result$grid, "config")
  df <- result$results
  inf <- df[df$layer == "inference", , drop = FALSE]
  at <- attrition_summary(result)
  cells <- summarize_effects(result)
  audit <- audit_universes(result)

  lines <- c(
    sprintf("# Multiverse report: %s", config$domain),
    "",
    "## Purpose declaration",
    purpose,
    "",
    "## Headline specification count",
    sprintf(
      paste("**%d justified, converged specifications** across %d estimand",
            "cell(s) (planned before viability filtering: %d; every loss is",
            "itemized in the attrition section). Multiverse size is a cost",
            "to justify, not an achievement: each fork's warrant is in the",
            "warrant table below."),
      sum(at$n[at$stage == "converged"]),
      length(unique(inf$cell_id)),
      at$n[at$stage == "planned"]
    ),
    "",
    "## Effect summaries (per estimand cell)",
    "Effect-size quantities are reported per estimand cell only; pooled",
    "effect summaries across cells are refused by the package (two-layer",
    "rule).",
    ""
  )

  for (i in seq_len(nrow(cells))) {
    lines <- c(lines, sprintf(
      "- `%s`: median g = %.3f [q10 %.3f, q90 %.3f], %d specs, sign consistency %.0f%%",
      cells$cell_id[i], cells$median_b[i], cells$q10[i], cells$q90[i],
      cells$n_specs[i], 100 * cells$sign_consistency[i]
    ))
  }

  if (nrow(cells) > 1) {
    div <- summarize_divergence(result)
    lines <- c(lines, "",
               "## Between-estimand divergence (labeled; never pooled)",
               sprintf(
                 "Share of specification variance lying between estimand cells: %.1f%%.",
                 100 * div$variance_between_share
               ))
    for (i in seq_len(nrow(div$between_cell_differences))) {
      d <- div$between_cell_differences[i, ]
      lines <- c(lines, sprintf("- %s vs %s: delta median g = %.3f",
                                d$cell_a, d$cell_b, d$delta_median))
    }
  }

  # labeled descriptive frequencies, paired with decomposition
  dec <- tryCatch(decompose_variance(result), error = function(e) NULL)
  n_p <- sum(!is.na(inf$pval))
  lines <- c(
    lines, "",
    "## Descriptive frequencies (NOT inferential quantities)",
    sprintf(
      paste("Share of specifications with p < .05: %.0f%% (%d of %d; %d",
            "specs carry no p-value). Share with g > %.2f (MID): %.0f%%.",
            "These are descriptive frequencies over non-independent",
            "specifications of one dataset; they are not evidence rates and",
            "must be read with the decomposition below."),
      100 * sum(inf$pval < .05, na.rm = TRUE) / nrow(inf),
      sum(inf$pval < .05, na.rm = TRUE), nrow(inf), nrow(inf) - n_p,
      mid, 100 * mean(inf$b > mid)
    )
  )

  if (!is.null(dec)) {
    lines <- c(lines, "", "## Variance decomposition (what drives divergence)")
    fs <- family_shares(dec)
    for (i in seq_len(nrow(fs))) {
      lines <- c(lines, sprintf("- %s/%s: %.1f%% (MoM %.1f%%)",
                                fs$layer[i], fs$family[i],
                                100 * fs$share[i], 100 * fs$mom_share[i]))
    }
    lines <- c(lines, "", paste0("*", attr(dec, "caveat"), "*"))
    # deflation deliverable: ranked fork drivers
    drivers <- dec[dec$term != "residual", ]
    drivers <- drivers[order(-drivers$share), ]
    lines <- c(lines, "",
               "### Ranked fork drivers (empirical agenda for deflation)")
    for (i in seq_len(nrow(drivers))) {
      lines <- c(lines, sprintf("%d. %s (%.1f%%)", i, drivers$term[i],
                                100 * drivers$share[i]))
    }
  }

  # worst-universe audit
  flagged <- audit[audit$n_flags > 0, , drop = FALSE]
  lines <- c(lines, "", "## Worst-universe audit",
             sprintf(
               paste("%d of %d universes carry audit flags. The multiverse",
                     "is judged by its worst universes; each flagged",
                     "universe is traceable via lookup_spec()."),
               nrow(flagged), nrow(audit)
             ))
  if (nrow(flagged) > 0) {
    worst <- flagged[order(-flagged$n_flags), , drop = FALSE]
    for (i in seq_len(min(10, nrow(worst)))) {
      lines <- c(lines, sprintf("- `%s`: %s", worst$spec_id[i],
                                worst$flags[i]))
    }
  }

  # bias layer
  bias <- df[df$layer == "bias", , drop = FALSE]
  if (nrow(bias) > 0) {
    lines <- c(lines, "", "## Bias-decomposition layer (X forks; excluded from inference)")
    dmap <- attr(result$grid, "decision_map")
    x_forks <- names(dmap)[dmap == "indefensible"]
    for (xf in intersect(x_forks, names(bias))) {
      for (cell in unique(bias$cell_id)) {
        bc <- bias[bias$cell_id == cell & !is.na(bias[[xf]]), ]
        if (nrow(bc) < 2) next
        ref_level <- config$forks[[xf]]$reference
        ref_b <- bc$b[bc[[xf]] == ref_level]
        for (lv in setdiff(unique(bc[[xf]]), ref_level)) {
          lines <- c(lines, sprintf(
            "- `%s`, %s = %s: b = %.3f vs reference %.3f (distortion %.3f)",
            cell, xf, lv, bc$b[bc[[xf]] == lv][1], ref_b[1],
            bc$b[bc[[xf]] == lv][1] - ref_b[1]
          ))
        }
      }
    }
  }

  # attrition
  lines <- c(lines, "", "## Specification attrition")
  for (i in seq_len(nrow(at))) {
    lines <- c(lines, sprintf("- %s: %d", at$stage[i], at$n[i]))
  }

  # decision-ruling crosswalk (rendered from code so it can never drift)
  cw <- decision_crosswalk()
  lines <- c(lines, "", "## Decision rulings (crosswalk)",
             "| ruling | alias | handling |",
             "|---|---|---|")
  for (i in seq_len(nrow(cw))) {
    lines <- c(lines, sprintf("| %s | %s | %s |",
                              cw$ruling[i], cw$alias[i], cw$handling[i]))
  }

  # warrant table
  wt <- warrant_table(config)
  lines <- c(lines, "", "## Warrant table",
             "| element | name | decision | family | levels | basis | justification |",
             "|---|---|---|---|---|---|---|")
  for (i in seq_len(nrow(wt))) {
    lines <- c(lines, sprintf("| %s | %s | %s | %s | %s | %s | %s |",
                              wt$element[i], wt$name[i], wt$decision[i],
                              wt$family[i] %||% "", wt$levels[i],
                              wt$basis[i], wt$justification[i]))
  }

  if (!is.null(config$notes)) {
    lines <- c(lines, "", "## Analyst notes (structured reflection)",
               paste("-", config$notes))
  }
  lines
}

#' Export a preregisterable configuration document
#'
#' Writes a time-stamped Quarto document containing the purpose declaration,
#' full warrant tables, estimand declarations, thresholds and menu of every
#' supplied domain config -- generated BEFORE the pipeline touches data, so
#' the fork configuration is preregisterable.
#'
#' @param configs List of \code{mv_domain_config} objects (or one).
#' @param file Output path (.qmd).
#' @param purpose Purpose declaration text.
#' @return The file path, invisibly.
#' @export
export_prereg_config <- function(configs, file,
                                 purpose = paste(
                                   "Diagnostic-inferential about divergence",
                                   "among defensible specifications;",
                                   "explicitly not persuasion toward a",
                                   "pooled effect estimate.")) {
  if (inherits(configs, "mv_domain_config")) configs <- list(configs)
  stopifnot(all(vapply(configs, inherits, logical(1), "mv_domain_config")))

  lines <- c(
    "---",
    "title: \"Preregistered multiverse configuration\"",
    sprintf("date: \"%s\"", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    "format: html",
    "---",
    "",
    "## Purpose declaration",
    purpose,
    "",
    sprintf("Generated %s, before any pipeline contact with outcome data.",
            format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    ""
  )
  for (cfg in configs) {
    lines <- c(lines,
               sprintf("## Domain: %s", cfg$domain),
               "",
               "### Estimand declaration (static fields)",
               paste0("- ", names(cfg$estimand), ": ",
                      unlist(cfg$estimand)),
               "",
               sprintf("### Thresholds"),
               sprintf("- minimum studies per universe (min_studies): %d",
                       cfg$min_studies),
               sprintf("- strict sensitivity threshold (min_studies_strict): %d",
                       cfg$min_studies_strict),
               if (!is.null(cfg$flag_reference)) {
                 sprintf("- plausibility flag reference: %s",
                         cfg$flag_reference)
               },
               "",
               "### Warrant table", "")
    wt <- warrant_table(cfg)
    lines <- c(lines,
               "| element | name | decision | family | levels | basis | justification | citation |",
               "|---|---|---|---|---|---|---|---|")
    for (i in seq_len(nrow(wt))) {
      lines <- c(lines, sprintf(
        "| %s | %s | %s | %s | %s | %s | %s | %s |",
        wt$element[i], wt$name[i], wt$decision[i],
        ifelse(is.na(wt$family[i]), "", wt$family[i]), wt$levels[i],
        wt$basis[i], wt$justification[i],
        ifelse(is.na(wt$citation[i]), "", wt$citation[i])
      ))
    }
    lines <- c(lines, "")
  }
  writeLines(lines, file)
  invisible(file)
}
