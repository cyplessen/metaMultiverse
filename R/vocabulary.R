# ABOUTME: The package's plain-language vocabulary: canonical names for
# ABOUTME: decision rulings, fork kinds, and plausibility strategies, with
# ABOUTME: alias normalization (E/U/N/X letters, which/how, legacy level
# ABOUTME: names) and the exported decision crosswalk table.

# canonical decision rulings; letters kept as accepted aliases
.decision_aliases <- c(
  equivalent = "equivalent",
  uncertain = "uncertain",
  changes_question = "changes_question",
  indefensible = "indefensible",
  E = "equivalent",
  U = "uncertain",
  N = "changes_question",
  X = "indefensible"
)

.fork_type_aliases <- c(
  data = "data",
  analysis = "analysis",
  which = "data",
  how = "analysis"
)

.plausibility_aliases <- c(
  keep_all = "keep_all",
  drop_triple_flagged = "drop_triple_flagged",
  drop_two_plus_flagged = "drop_two_plus_flagged",
  drop_any_flagged = "drop_any_flagged",
  drop_extreme_effects = "drop_extreme_effects",
  drop_underpowered = "drop_underpowered",
  drop_high_rob = "drop_high_rob",
  # legacy spellings
  all_studies = "keep_all",
  exclude_all_flagged = "drop_triple_flagged",
  exclude_2plus_flags = "drop_two_plus_flagged",
  exclude_1plus_flags = "drop_any_flagged",
  exclude_effect_flag = "drop_extreme_effects",
  exclude_power_flag = "drop_underpowered",
  exclude_rob_flag = "drop_high_rob"
)

normalize_token <- function(x, aliases, what, context = NULL) {
  if (is.null(x) || length(x) != 1 || is.na(x) || !x %in% names(aliases)) {
    stop(if (is.null(context)) "" else paste0(context, ": "),
         what, " must be one of ",
         paste(unique(unname(aliases)), collapse = ", "),
         " (aliases accepted: ",
         paste(setdiff(names(aliases), unname(aliases)), collapse = ", "),
         "); got \"", if (is.null(x)) "NULL" else x, "\".",
         call. = FALSE)
  }
  unname(aliases[[x]])
}

normalize_decision <- function(x, context = NULL) {
  normalize_token(x, .decision_aliases, "decision", context)
}

normalize_fork_type <- function(x, context = NULL) {
  normalize_token(x, .fork_type_aliases, "fork type", context)
}

normalize_plausibility_level <- function(x, context = NULL) {
  vapply(x, function(lv) {
    normalize_token(lv, .plausibility_aliases, "plausibility level", context)
  }, character(1), USE.NAMES = FALSE)
}

#' The decision-ruling crosswalk
#'
#' Canonical plain-language decision rulings, their accepted letter
#' aliases, the handling each ruling triggers, and the lineage to the
#' Del Giudice & Gangestad (2021) framework the classification criteria
#' come from. This table is rendered in the vignette, the reports, and the
#' papers directly from this function, so the published crosswalk can
#' never drift from the code.
#'
#' \code{settled} names the fifth disposition -- decisions deliberately not
#' varied (\code{fixed_decisions} in \code{\link{domain_config}}); it is
#' not a \code{\link{fork}} ruling.
#'
#' @return Data frame: ruling, alias, handling, lineage.
#' @export
decision_crosswalk <- function() {
  data.frame(
    ruling = c("equivalent", "uncertain", "changes_question",
               "indefensible", "settled"),
    alias = c("E", "U", "N", "X", "(fixed_decisions)"),
    handling = c(
      "varies within the grid; movement checked against the equivalence_margin",
      "varies within the grid; tracked as metadata, variance attributed",
      "splits the analysis into estimand cells; effects never pooled across",
      "excluded from inference; bias layer only, distortion quantified",
      "not varied; warrant states why; alternatives go to the bias layer"
    ),
    lineage = c(
      "their Type E (identical)",
      "their Type U definition; handling differs (they split, we track)",
      "their effect-nonequivalence; their 'separate analyses' formalized",
      "their/Simonsohn's exclusion of inferior options, quantified",
      "their Type N quality case: fix to the best-justified option"
    ),
    stringsAsFactors = FALSE
  )
}
