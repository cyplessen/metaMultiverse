# ABOUTME: fork() -- the atomic decision-node object of the multiverse.
# ABOUTME: Carries stable name, plain-language decision ruling
# ABOUTME: (equivalent/uncertain/changes_question/indefensible),
# ABOUTME: clinical/analytic family, and a required warrant.

#' Define a fork (decision node) for a multiverse analysis
#'
#' A fork is one analytic or clinical decision with two or more options
#' ("levels"). Every fork carries a stable name (results are keyed by it --
#' there are no positional slots), a plain-language decision ruling, a fork
#' family, and a machine-readable justification (warrant).
#'
#' Decision rulings (two-layer rule; see \code{\link{decision_crosswalk}}
#' for the mapping to the Del Giudice & Gangestad, 2021, letters):
#' \describe{
#'   \item{equivalent (alias E)}{Alternatives defensibly interchangeable.
#'     Varied within the multiverse; expected to move estimates less than
#'     the \code{equivalence_margin} (flagged post-run if it moves more).}
#'   \item{uncertain (alias U)}{Defensible alternatives with no settled
#'     ranking. Varied within the multiverse and tracked via metadata;
#'     never triggers separate analyses.}
#'   \item{changes_question (alias N)}{Levels answer different clinical
#'     questions (different estimands). Crossing question-changing forks
#'     defines the estimand cells of the grid; effect summaries are never
#'     pooled across cells.}
#'   \item{indefensible (alias X)}{Historically used but not defensible.
#'     Excluded from all inference summaries; runs only in the separate
#'     bias-decomposition layer against the reference specification.}
#' }
#'
#' A fifth disposition, \emph{settled}, applies to decisions deliberately
#' not varied: they are declared as \code{fixed_decisions} in
#' \code{\link{domain_config}}, not as forks.
#'
#' The classification criteria (measurement, effect, and power/precision
#' nonequivalence) come from Del Giudice & Gangestad (2021); the rulings
#' adapt their prescriptions to a divergence target quantity. See
#' \code{\link{decision_crosswalk}} and the vignette.
#'
#' @param name Stable snake_case identifier (e.g. \code{"comparator"}).
#' @param type \code{"data"} (subsets the data via \code{column}; alias
#'   \code{"which"}) or \code{"analysis"} (selects an analysis option,
#'   e.g. estimator; alias \code{"how"}).
#' @param decision One of \code{"equivalent"}, \code{"uncertain"},
#'   \code{"changes_question"}, \code{"indefensible"} (letter aliases
#'   \code{"E"}, \code{"U"}, \code{"N"}, \code{"X"} accepted).
#' @param family \code{"clinical"} or \code{"analytic"} (second taxonomy axis
#'   for the variance decomposition).
#' @param justification A list with elements \code{basis} (one of
#'   \code{"citation"}, \code{"empirical"}, \code{"convention-unresolved"}),
#'   \code{text} (non-empty explanation), and optionally \code{citation}.
#'   Rendered into the auto-generated warrant table.
#' @param column Data column the fork operates on (required for
#'   \code{type = "data"}).
#' @param levels Named list mapping level label -> character vector of values
#'   in \code{column} (data forks), or named list/character vector of options
#'   (analysis forks). Omit if \code{adaptive} is supplied.
#' @param adaptive Optional \code{function(data)} returning the named
#'   \code{levels} list at validation time. Permitted only for data-dependent
#'   level sets (e.g. plausibility flag profiles); the menu of levels must
#'   still be hardcoded inside the function, and levels empty in a given
#'   domain are reported via the fork availability matrix, never silently
#'   dropped.
#' @param reference Label of the reference level (defaults to the first
#'   level). Used by the bias layer (indefensible forks run against the
#'   reference specification) and by leave-one-fork-out diagnostics.
#' @param estimand_field For question-changing forks: name of the
#'   estimand-declaration field this fork fills (defaults to \code{name}).
#' @param equivalence_margin For equivalent forks: absolute effect-size
#'   movement (Hedges' g units) above which the fork is flagged post-run as
#'   moving estimates more than an equivalent fork should (escalated for
#'   re-ruling). Default 0.1. (Alias: \code{tolerance}.)
#' @param overrides For analysis forks other than \code{ma_method}/
#'   \code{dependency}: named list mapping level label -> list of engine
#'   settings (\code{ma_method}, \code{dependency}) that this level imposes.
#'   Used chiefly by indefensible forks in the bias layer (e.g. a
#'   fixed-effect level overriding \code{ma_method = "fe"}).
#' @param tolerance Deprecated alias for \code{equivalence_margin}.
#'
#' @return An object of class \code{"mv_fork"}.
#' @export
fork <- function(name,
                 type = "data",
                 decision = "uncertain",
                 family = c("analytic", "clinical"),
                 justification,
                 column = NULL,
                 levels = NULL,
                 adaptive = NULL,
                 reference = NULL,
                 estimand_field = NULL,
                 equivalence_margin = NULL,
                 overrides = NULL,
                 tolerance = NULL) {

  type <- normalize_fork_type(type, context = paste0("fork \"", name, "\""))
  decision <- normalize_decision(decision,
                                 context = paste0("fork \"", name, "\""))
  family <- match.arg(family)

  if (!is.null(tolerance) && is.null(equivalence_margin)) {
    equivalence_margin <- tolerance
  }
  if (decision == "equivalent" && is.null(equivalence_margin)) {
    equivalence_margin <- 0.1
  }

  if (!is.character(name) || length(name) != 1 || !nzchar(name)) {
    stop("fork `name` must be a single non-empty string.", call. = FALSE)
  }
  if (!grepl("^[a-z][a-z0-9_]*$", name)) {
    stop("fork `name` must be snake_case (got \"", name, "\").", call. = FALSE)
  }

  if (type == "data" && is.null(column)) {
    stop("fork \"", name, "\": `column` is required for data forks.",
         call. = FALSE)
  }

  if (is.null(levels) && is.null(adaptive)) {
    stop("fork \"", name, "\": supply `levels` or an `adaptive` function.",
         call. = FALSE)
  }
  if (!is.null(levels) && !is.null(adaptive)) {
    stop("fork \"", name, "\": `levels` and `adaptive` are mutually exclusive.",
         call. = FALSE)
  }
  if (!is.null(adaptive) && !is.function(adaptive)) {
    stop("fork \"", name, "\": `adaptive` must be a function(data).",
         call. = FALSE)
  }

  if (!is.null(levels)) {
    if (is.character(levels)) {
      levels <- stats::setNames(as.list(levels), levels)
    }
    if (is.null(names(levels)) || any(!nzchar(names(levels)))) {
      stop("fork \"", name, "\": every level must be named.", call. = FALSE)
    }
    if (anyDuplicated(names(levels))) {
      stop("fork \"", name, "\": duplicated level names.", call. = FALSE)
    }
    if (length(levels) < 2 && decision != "indefensible") {
      stop("fork \"", name, "\": a fork needs at least 2 levels ",
           "(a single-option decision is fixed, not a fork).", call. = FALSE)
    }
  }

  justification <- validate_justification(justification, name)

  if (!is.null(reference) && !is.null(levels) &&
      !reference %in% names(levels)) {
    stop("fork \"", name, "\": reference level \"", reference,
         "\" is not among the levels.", call. = FALSE)
  }
  if (is.null(reference) && !is.null(levels)) {
    reference <- names(levels)[1]
  }

  if (decision == "changes_question" && is.null(estimand_field)) {
    estimand_field <- name
  }
  if (decision != "equivalent") equivalence_margin <- NULL

  if (!is.null(overrides)) {
    if (type != "analysis") {
      stop("fork \"", name, "\": `overrides` is only meaningful for ",
           "analysis forks.", call. = FALSE)
    }
    if (!is.list(overrides) || is.null(names(overrides))) {
      stop("fork \"", name, "\": `overrides` must be a named list ",
           "(level -> engine settings).", call. = FALSE)
    }
    if (!is.null(levels)) {
      unknown <- setdiff(names(overrides), names(levels))
      if (length(unknown) > 0) {
        stop("fork \"", name, "\": overrides refer to unknown level(s): ",
             paste(unknown, collapse = ", "), call. = FALSE)
      }
    }
  }

  structure(
    list(
      name = name,
      type = type,
      decision = decision,
      family = family,
      column = column,
      levels = levels,
      adaptive = adaptive,
      reference = reference,
      estimand_field = estimand_field,
      equivalence_margin = equivalence_margin,
      overrides = overrides,
      justification = justification
    ),
    class = "mv_fork"
  )
}

validate_justification <- function(justification, fork_name) {
  if (missing(justification) || is.null(justification)) {
    stop("fork \"", fork_name, "\": a `justification` is required -- every ",
         "fork level in every config carries a machine-readable warrant.",
         call. = FALSE)
  }
  if (is.character(justification) && length(justification) == 1) {
    # convenience: bare string becomes an unresolved-convention warrant
    justification <- list(basis = "convention-unresolved",
                          text = justification)
  }
  if (!is.list(justification) ||
      !all(c("basis", "text") %in% names(justification))) {
    stop("fork \"", fork_name, "\": `justification` must be a list with ",
         "`basis` and `text` (and optionally `citation`).", call. = FALSE)
  }
  ok_basis <- c("citation", "empirical", "convention-unresolved")
  if (!justification$basis %in% ok_basis) {
    stop("fork \"", fork_name, "\": justification basis must be one of ",
         paste(ok_basis, collapse = ", "), ".", call. = FALSE)
  }
  if (justification$basis == "citation" &&
      (is.null(justification$citation) || !nzchar(justification$citation))) {
    stop("fork \"", fork_name, "\": basis \"citation\" requires a ",
         "`citation` entry.", call. = FALSE)
  }
  if (!nzchar(justification$text)) {
    stop("fork \"", fork_name, "\": justification text must be non-empty.",
         call. = FALSE)
  }
  justification
}

#' @export
print.mv_fork <- function(x, ...) {
  lv <- if (is.null(x$levels)) {
    "<adaptive: materialized at validation>"
  } else {
    paste(names(x$levels), collapse = ", ")
  }
  cat(sprintf("<fork> %s [%s/%s, %s]\n", x$name, x$decision, x$family, x$type))
  cat("  levels: ", lv, "\n", sep = "")
  cat("  reference: ", x$reference %||% "<unset>", "\n", sep = "")
  cat("  warrant (", x$justification$basis, "): ",
      x$justification$text, "\n", sep = "")
  invisible(x)
}

`%||%` <- function(a, b) if (is.null(a)) b else a
