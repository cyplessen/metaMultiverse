# ABOUTME: domain_config() -- explicit, hardcoded per-domain configuration:
# ABOUTME: forks, eligibility filters, estimand declaration, thresholds; plus
# ABOUTME: load-time validation against the data (feeds the availability matrix).

#' Define a per-domain multiverse configuration
#'
#' Subgroup viability and fork menus are deliberately NOT abstracted into a
#' universal grid: every domain gets an explicit config holding its forks,
#' eligibility filters (with warrants), estimand declaration, and thresholds.
#' Adaptive logic is permitted only inside individual forks for data-dependent
#' level sets (see \code{\link{fork}}).
#'
#' @param domain Domain identifier (snake_case, e.g. \code{"depression"}).
#' @param forks List of \code{\link{fork}} objects with unique names.
#' @param estimand Named list of static estimand fields (e.g.
#'   \code{list(population = "adults with MDD", intervention = "psychotherapy",
#'   outcome = "depressive symptoms", timepoint = "post-treatment")}).
#'   N forks add their own field per cell at grid-build time.
#' @param eligibility Optional named list of eligibility filters, each a list
#'   with \code{expr} (a string parsed and evaluated in the data) and
#'   \code{justification} (see \code{\link{fork}} for the warrant format).
#'   Filters run before validation and grid building.
#' @param data_source Free-text description or file name of the data snapshot.
#' @param flag_reference Reference distribution key for effect-size
#'   plausibility flagging (\code{"all"}, \code{"dep"}, \code{"psy"},
#'   \code{"ptsd"}; Phase 3).
#' @param min_studies Minimum number of unique studies per universe.
#'   The default of 5 is the convention adopted for the Metapsy multiverse
#'   analyses the architecture was built for, not a universal rule; set it
#'   for your domain. (Alias: \code{min_k}.)
#' @param min_studies_strict Stricter threshold for the sensitivity rerun
#'   (default 10). (Alias: \code{min_k_sensitivity}.)
#' @param min_k,min_k_sensitivity Legacy aliases.
#' @param fixed_decisions Optional named list of decisions deliberately NOT
#'   varied (warrant-based fixing): each a list with \code{value} and
#'   \code{justification}. Rendered as the fixed-decision table in reports so
#'   robustness claims are visibly conditional on the fixed core (empirically
#'   settled choices are fixed, not forked; their alternatives belong in the
#'   X/D layer).
#' @param notes Optional character vector of analyst concerns / structured
#'   reflection notes, carried into reports.
#'
#' @return An object of class \code{"mv_domain_config"}.
#' @export
domain_config <- function(domain,
                          forks,
                          estimand,
                          eligibility = NULL,
                          data_source = NULL,
                          flag_reference = NULL,
                          min_studies = 5,
                          min_studies_strict = 10,
                          fixed_decisions = NULL,
                          notes = NULL,
                          min_k = NULL,
                          min_k_sensitivity = NULL) {
  # legacy aliases
  if (!is.null(min_k)) min_studies <- min_k
  if (!is.null(min_k_sensitivity)) min_studies_strict <- min_k_sensitivity

  if (!is.character(domain) || length(domain) != 1 ||
      !grepl("^[a-z][a-z0-9_]*$", domain)) {
    stop("`domain` must be a single snake_case string.", call. = FALSE)
  }
  if (!is.list(forks) || length(forks) == 0 ||
      !all(vapply(forks, inherits, logical(1), "mv_fork"))) {
    stop("`forks` must be a non-empty list of fork() objects.", call. = FALSE)
  }
  fork_names <- vapply(forks, `[[`, character(1), "name")
  if (anyDuplicated(fork_names)) {
    stop("Duplicated fork names: ",
         paste(unique(fork_names[duplicated(fork_names)]), collapse = ", "),
         call. = FALSE)
  }
  names(forks) <- fork_names

  if (missing(estimand) || !is.list(estimand) || is.null(names(estimand)) ||
      length(estimand) == 0) {
    stop("`estimand` must be a named list of static estimand fields ",
         "(population, intervention, outcome, timepoint, ...).", call. = FALSE)
  }
  n_fields <- unlist(lapply(forks, function(f) {
    if (f$decision == "changes_question") f$estimand_field else NULL
  }))
  clash <- intersect(n_fields, names(estimand))
  if (length(clash) > 0) {
    stop("Estimand field(s) declared both statically and by an N fork: ",
         paste(clash, collapse = ", "), call. = FALSE)
  }

  if (!is.null(eligibility)) {
    if (!is.list(eligibility) || is.null(names(eligibility))) {
      stop("`eligibility` must be a named list.", call. = FALSE)
    }
    for (nm in names(eligibility)) {
      el <- eligibility[[nm]]
      if (!is.list(el) || is.null(el$expr) || !is.character(el$expr)) {
        stop("eligibility \"", nm, "\": needs an `expr` string.", call. = FALSE)
      }
      eligibility[[nm]]$justification <-
        validate_justification(el$justification,
                               paste0("eligibility:", nm))
    }
  }

  if (!is.null(flag_reference) &&
      !flag_reference %in% c("all", "dep", "gad", "psy", "ptsd")) {
    stop("`flag_reference` must be one of all, dep, gad, psy, ptsd.",
         call. = FALSE)
  }
  stopifnot(is.numeric(min_studies), min_studies >= 2,
            is.numeric(min_studies_strict), min_studies_strict >= min_studies)

  if (!is.null(fixed_decisions)) {
    if (!is.list(fixed_decisions) || is.null(names(fixed_decisions))) {
      stop("`fixed_decisions` must be a named list.", call. = FALSE)
    }
    for (nm in names(fixed_decisions)) {
      fd <- fixed_decisions[[nm]]
      if (!is.list(fd) || is.null(fd$value)) {
        stop("fixed decision \"", nm, "\": needs a `value`.", call. = FALSE)
      }
      fixed_decisions[[nm]]$justification <-
        validate_justification(fd$justification, paste0("fixed:", nm))
    }
  }

  structure(
    list(
      domain = domain,
      forks = forks,
      estimand = estimand,
      eligibility = eligibility,
      data_source = data_source,
      flag_reference = flag_reference,
      min_studies = min_studies,
      min_studies_strict = min_studies_strict,
      fixed_decisions = fixed_decisions,
      notes = notes
    ),
    class = "mv_domain_config"
  )
}

#' @export
print.mv_domain_config <- function(x, ...) {
  cat("<domain_config> ", x$domain, "\n", sep = "")
  cat("  estimand: ",
      paste(names(x$estimand), unlist(x$estimand), sep = " = ",
            collapse = "; "), "\n", sep = "")
  for (f in x$forks) {
    cat(sprintf("  - %s [%s/%s, %s]\n", f$name, f$decision, f$family, f$type))
  }
  if (!is.null(x$eligibility)) {
    cat("  eligibility: ", paste(names(x$eligibility), collapse = ", "),
        "\n", sep = "")
  }
  cat("  min_studies: ", x$min_studies, " (strict: ",
      x$min_studies_strict, ")\n", sep = "")
  invisible(x)
}

#' Apply a config's eligibility filters to the data
#'
#' @param config An \code{mv_domain_config}.
#' @param data Data frame.
#' @return The filtered data with an attribute \code{"eligibility_log"}: a
#'   data frame of filter name, expression, rows before/after.
#' @export
apply_eligibility <- function(config, data) {
  stopifnot(inherits(config, "mv_domain_config"))
  log <- data.frame(filter = character(), expr = character(),
                    n_before = integer(), n_after = integer(),
                    stringsAsFactors = FALSE)
  if (!is.null(config$eligibility)) {
    for (nm in names(config$eligibility)) {
      el <- config$eligibility[[nm]]
      n_before <- nrow(data)
      keep <- eval(parse(text = el$expr), envir = data, enclos = baseenv())
      if (!is.logical(keep) || length(keep) != n_before) {
        stop("eligibility \"", nm, "\" (", el$expr,
             ") did not evaluate to a logical vector of nrow(data).",
             call. = FALSE)
      }
      keep[is.na(keep)] <- FALSE
      data <- data[keep, , drop = FALSE]
      log <- rbind(log, data.frame(filter = nm, expr = el$expr,
                                   n_before = n_before, n_after = nrow(data),
                                   stringsAsFactors = FALSE))
    }
  }
  attr(data, "eligibility_log") <- log
  data
}

#' Validate a domain config against its data
#'
#' Materializes adaptive fork levels, checks columns and level values, counts
#' effect sizes and studies per level, and classifies every fork for the
#' availability matrix: \code{"varies"} (>= 2 viable levels),
#' \code{"cannot_vary"} (only one distinct level exists in the data),
#' \code{"too_few_studies"} (< 2 viable levels under the min_studies rule).
#' Hard structural problems (missing column, level whose values never occur)
#' raise informative errors unless \code{strict = FALSE}, in which case they
#' are recorded with status \code{"not_in_data"}.
#'
#' A level is viable iff its largest planned universe reaches
#' \code{config$min_studies} unique studies (user decision D3).
#'
#' @param config An \code{mv_domain_config}.
#' @param data Data frame (after \code{\link{apply_eligibility}}; the function
#'   applies the filters itself if the attribute is missing).
#' @param strict Error on structural problems (default TRUE).
#' @return An object of class \code{"mv_config_validation"}: list with
#'   \code{availability} (data frame: fork, level, decision, family, n_es,
#'   n_studies, viable, status), \code{fork_status} (per-fork summary),
#'   \code{levels} (materialized levels per fork), \code{problems}
#'   (character vector), and \code{data} (the filtered data).
#' @export
validate_domain_config <- function(config, data, strict = TRUE) {
  stopifnot(inherits(config, "mv_domain_config"), is.data.frame(data))

  if (is.null(attr(data, "eligibility_log"))) {
    data <- apply_eligibility(config, data)
  }
  if (!"study" %in% names(data)) {
    stop("Config validation: data has no `study` column.", call. = FALSE)
  }

  problems <- character()
  availability <- list()
  materialized <- list()

  for (f in config$forks) {
    levels <- f$levels
    if (!is.null(f$adaptive)) {
      levels <- f$adaptive(data)
      if (!is.list(levels) || is.null(names(levels))) {
        stop("fork \"", f$name, "\": adaptive() must return a named list ",
             "of levels.", call. = FALSE)
      }
    }

    if (f$type == "analysis") {
      # how forks touch no data column; availability is recorded as requested
      # (estimation failures are logged at run time, not here)
      materialized[[f$name]] <- levels
      availability[[f$name]] <- data.frame(
        fork = f$name, level = names(levels), decision = f$decision,
        family = f$family, n_es = NA_integer_, n_studies = NA_integer_,
        viable = TRUE, status = "varies", stringsAsFactors = FALSE
      )
      next
    }

    if (!f$column %in% names(data)) {
      msg <- paste0("fork \"", f$name, "\": column `", f$column,
                    "` not present in data.")
      if (strict) stop(msg, call. = FALSE)
      problems <- c(problems, msg)
      availability[[f$name]] <- data.frame(
        fork = f$name, level = names(levels) %||% NA_character_,
        decision = f$decision, family = f$family,
        n_es = 0L, n_studies = 0L, viable = FALSE,
        status = "not_in_data",
        stringsAsFactors = FALSE
      )
      next
    }

    col <- data[[f$column]]
    rows <- lapply(levels, function(vals) which(col %in% vals))
    n_es <- vapply(rows, length, integer(1))
    n_studies <- vapply(rows, function(idx) {
      length(unique(data$study[idx]))
    }, integer(1))

    absent <- n_es == 0
    for (lv in names(levels)[absent]) {
      msg <- paste0("fork \"", f$name, "\", level \"", lv,
                    "\": requested values (",
                    paste(levels[[lv]], collapse = ", "),
                    ") do not occur in `", f$column, "`.")
      # hardcoded level absent = config-data mismatch -> informative error;
      # adaptive level empty = expected data-dependence -> availability
      # matrix entry (reported, never silently dropped)
      if (strict && is.null(f$adaptive)) stop(msg, call. = FALSE)
      problems <- c(problems, msg)
    }

    # levels whose realized subsets are identical are pseudo-variation
    # (e.g. all_controls == cau when only cau exists); later duplicates are
    # recorded, not silently kept as fake forks
    signature <- vapply(rows, function(idx) paste(idx, collapse = ","),
                        character(1))
    duplicate_of <- rep(NA_character_, length(levels))
    seen <- character()
    for (i in seq_along(levels)) {
      if (absent[i]) next
      hit <- match(signature[i], seen)
      if (!is.na(hit)) {
        duplicate_of[i] <- names(levels)[which(signature == seen[hit])[1]]
      } else {
        seen <- c(seen, signature[i])
      }
    }
    is_dup <- !is.na(duplicate_of)

    viable <- n_studies >= config$min_studies & !absent & !is_dup

    availability[[f$name]] <- data.frame(
      fork = f$name, level = names(levels), decision = f$decision,
      family = f$family, n_es = n_es, n_studies = n_studies,
      viable = viable,
      status = ifelse(absent, "not_in_data",
               ifelse(is_dup, paste0("same_studies_as_", duplicate_of),
               ifelse(viable, "viable", "too_few_studies"))),
      stringsAsFactors = FALSE
    )
    materialized[[f$name]] <- levels
  }

  availability <- do.call(rbind, availability)
  rownames(availability) <- NULL

  fork_status <- do.call(rbind, lapply(split(availability, availability$fork),
    function(d) {
      n_exist <- sum(d$n_es > 0 | is.na(d$n_es)) # NA = how fork, always exists
      distinct <- sum(!startsWith(d$status, "same_studies_as_") &
                        d$status != "not_in_data")
      status <- if (n_exist == 0) {
        "not_in_data"
      } else if (sum(d$viable) >= 2) {
        "varies"
      } else if (distinct == 1) {
        # only one distinct realized level exists: the fork cannot vary in
        # this domain no matter the study threshold
        "cannot_vary"
      } else {
        # >= 2 distinct levels exist but < 2 clear min_studies
        "too_few_studies"
      }
      data.frame(fork = d$fork[1], decision = d$decision[1],
                 family = d$family[1], n_levels_requested = nrow(d),
                 n_levels_viable = sum(d$viable), status = status,
                 stringsAsFactors = FALSE)
    }))
  rownames(fork_status) <- NULL

  structure(
    list(
      domain = config$domain,
      availability = availability,
      fork_status = fork_status,
      levels = materialized,
      problems = problems,
      data = data
    ),
    class = "mv_config_validation"
  )
}

#' @export
print.mv_config_validation <- function(x, ...) {
  cat("<config validation> ", x$domain, "\n", sep = "")
  print(x$fork_status, row.names = FALSE)
  if (length(x$problems)) {
    cat("problems:\n")
    for (p in x$problems) cat("  - ", p, "\n", sep = "")
  }
  invisible(x)
}
