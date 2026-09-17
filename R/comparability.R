# ABOUTME: Comparability layer (Part 2): cross-domain fork availability
# ABOUTME: matrix, attrition report, and the common-core (intersection)
# ABOUTME: multiverse for like-for-like cross-domain comparison.

#' Cross-domain fork availability matrix
#'
#' Combines per-domain config validations into the domains x forks x levels
#' matrix distinguishing: fork varies / cannot vary (single level in the
#' data) / too few studies / not in data, at both fork and level resolution -- plus
#' method inapplicability observed at run time (estimation failures per
#' ma_method) as an attribute.
#'
#' @param results Named list of \code{mv_grid_result} objects (names =
#'   domains), or of \code{mv_config_validation} objects.
#' @return Data frame: domain, fork, level, decision, family, n_es,
#'   n_studies, viable, status. Attributes: \code{fork_status} (domain x
#'   fork resolution) and \code{method_failures} (domain, ma_method,
#'   n_failed; only when run results are supplied).
#' @export
fork_availability_matrix <- function(results) {
  stopifnot(is.list(results), !is.null(names(results)),
            all(nzchar(names(results))))
  levels_tab <- list()
  fork_tab <- list()
  failures <- list()

  for (dom in names(results)) {
    x <- results[[dom]]
    v <- if (inherits(x, "mv_grid_result")) {
      attr(x$grid, "validation")
    } else if (inherits(x, "mv_config_validation")) {
      x
    } else {
      stop("Element \"", dom, "\" is neither an mv_grid_result nor an ",
           "mv_config_validation.", call. = FALSE)
    }
    av <- v$availability
    av$domain <- dom
    levels_tab[[dom]] <- av
    fs <- v$fork_status
    fs$domain <- dom
    fork_tab[[dom]] <- fs

    if (inherits(x, "mv_grid_result") && nrow(x$attrition) > 0) {
      led <- x$attrition[x$attrition$stage %in%
                           c("estimation_error", "invalid_result"), ,
                         drop = FALSE]
      if (nrow(led) > 0) {
        # spec ids embed fork=level pairs; recover the method per failed spec
        m <- regmatches(led$spec_id,
                        regexpr("ma_method=[^|]+", led$spec_id))
        m <- sub("^ma_method=", "", m)
        if (length(m) > 0) {
          tab <- table(m)
          failures[[dom]] <- data.frame(domain = dom,
                                        ma_method = names(tab),
                                        n_failed = as.integer(tab),
                                        stringsAsFactors = FALSE)
        }
      }
    }
  }

  out <- do.call(rbind, levels_tab)
  rownames(out) <- NULL
  out <- out[, c("domain", setdiff(names(out), "domain"))]
  fork_status <- do.call(rbind, fork_tab)
  rownames(fork_status) <- NULL
  attr(out, "fork_status") <- fork_status
  attr(out, "method_failures") <- if (length(failures)) {
    do.call(rbind, c(failures, list(make.row.names = FALSE)))
  } else NULL
  out
}

#' Combined specification attrition report across domains
#'
#' @param results Named list of \code{mv_grid_result}.
#' @return Data frame: domain x stage counts (planned, defined, lost by
#'   reason class, converged).
#' @export
attrition_report <- function(results) {
  stopifnot(is.list(results), !is.null(names(results)))
  out <- do.call(rbind, lapply(names(results), function(dom) {
    at <- attrition_summary(results[[dom]])
    at$domain <- dom
    at
  }))
  rownames(out) <- NULL
  out[, c("domain", "stage", "n")]
}

#' Common-core (intersection) multiverse across domains
#'
#' Identifies the analytic specification combinations (shared varying forks,
#' N forks excluded -- cells are fixed by \code{cells}) that produced a
#' converged result in EVERY domain, enabling like-for-like comparison.
#' The gap between full-grid and common-core metrics quantifies how much
#' differential specification survival distorts cross-domain rankings (M6).
#'
#' @param results Named list of \code{mv_grid_result}.
#' @param cells Named character vector: domain -> cell_id designating the
#'   common estimand cell for that domain (e.g. the cau stratum).
#' @return List with \code{shared_forks}, \code{common_combos} (data frame
#'   of surviving combinations), and \code{subsets}: named list of per-domain
#'   result rows restricted to the common core.
#' @export
common_core_specs <- function(results, cells) {
  stopifnot(is.list(results), !is.null(names(results)),
            all(names(results) %in% names(cells) |
                  names(cells) %in% names(results)))
  doms <- names(results)

  per_dom <- lapply(doms, function(dom) {
    r <- results[[dom]]
    df <- r$results
    df <- df[df$layer == "inference" & df$cell_id == cells[[dom]], ,
             drop = FALSE]
    if (nrow(df) == 0) {
      warning("Domain ", dom, ": no converged specs in designated cell ",
              cells[[dom]], call. = FALSE)
    }
    df
  })
  names(per_dom) <- doms

  # shared varying forks: intersection over domains of non-N varying forks
  shared <- Reduce(intersect, lapply(doms, function(dom) {
    g <- results[[dom]]$grid
    dm <- attr(g, "decision_map")
    v <- attr(g, "validation")$fork_status
    v$fork[v$status == "varies" & dm[v$fork] != "changes_question"]
  }))
  if (length(shared) == 0) {
    stop("No shared varying forks across domains -- common core undefined.",
         call. = FALSE)
  }

  combo_key <- function(df) {
    apply(df[, shared, drop = FALSE], 1, paste, collapse = "|")
  }
  keys <- lapply(per_dom, function(df) {
    if (nrow(df) == 0) character() else unique(combo_key(df))
  })
  common <- Reduce(intersect, keys)

  subsets <- lapply(per_dom, function(df) {
    if (nrow(df) == 0) return(df)
    df[combo_key(df) %in% common, , drop = FALSE]
  })

  combos <- if (length(common)) {
    parts <- strsplit(common, "|", fixed = TRUE)
    cc <- as.data.frame(do.call(rbind, parts), stringsAsFactors = FALSE)
    names(cc) <- shared
    cc
  } else {
    data.frame()
  }

  list(shared_forks = shared, common_combos = combos, subsets = subsets)
}
