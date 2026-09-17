# ABOUTME: build_specification_grid() -- one crossed grid per domain:
# ABOUTME: estimand cells (N forks) x within-cell E/U/how specs; X forks go to
# ABOUTME: a separate bias layer. Everything downstream is a view of this table.

#' Build the crossed specification grid for a domain
#'
#' Implements the two-layer estimand architecture: every combination of
#' N-fork levels defines an estimand cell; E/U which-forks and how-forks vary
#' within cells; X forks are excluded from the inference grid and populate a
#' separate bias-decomposition layer in which all other forks sit at their
#' reference levels. The grid is built once; summaries are views of it.
#'
#' Forks whose validation status is not \code{"varies"} do not vary: their
#' single viable level (if any) is fixed and recorded in
#' \code{attr(grid, "fixed_forks")}; forks with no viable level are dropped
#' and recorded in \code{attr(grid, "dropped_forks")}. Nothing is silently
#' lost: planned vs defined specification counts are in
#' \code{attr(grid, "attrition")} and suppressed levels in the validation
#' object's availability table.
#'
#' @param config An \code{mv_domain_config}.
#' @param data Data frame for this domain.
#' @param strict Passed to \code{\link{validate_domain_config}}.
#' @return A data frame of class \code{"mv_spec_grid"} with one column per
#'   fork (keyed by fork NAME -- no positional slots), plus \code{domain},
#'   \code{cell_id}, \code{layer} ("inference" or "bias"), \code{spec_id},
#'   \code{row_id}. Attributes: \code{config}, \code{validation},
#'   \code{estimand_cells}, \code{fixed_forks}, \code{dropped_forks},
#'   \code{decision_map}, \code{family_map}, \code{attrition}.
#' @export
build_specification_grid <- function(config, data, strict = TRUE) {
  validation <- validate_domain_config(config, data, strict = strict)
  data <- validation$data
  forks <- config$forks

  status_of <- stats::setNames(validation$fork_status$status,
                               validation$fork_status$fork)

  varying <- list()   # fork name -> character vector of viable level labels
  fixed <- list()     # fork name -> single viable level label
  dropped <- character()

  for (f in forks) {
    if (f$decision == "indefensible") next # handled in the bias layer below
    av <- validation$availability[validation$availability$fork == f$name, ]
    viable_levels <- av$level[av$viable]
    if (status_of[[f$name]] == "varies") {
      varying[[f$name]] <- viable_levels
    } else if (length(viable_levels) == 1) {
      fixed[[f$name]] <- viable_levels
    } else {
      dropped <- c(dropped, f$name)
    }
  }

  decision_map <- vapply(forks, `[[`, character(1), "decision")
  family_map <- vapply(forks, `[[`, character(1), "family")

  n_forks <- names(varying)[decision_map[names(varying)] == "changes_question"]
  eu_forks <- setdiff(names(varying), n_forks)

  # ---- estimand cells (cross of N-fork viable levels) -----------------------
  if (length(n_forks) > 0) {
    cells <- expand.grid(lapply(varying[n_forks], identity),
                         stringsAsFactors = FALSE,
                         KEEP.OUT.ATTRS = FALSE)
    cells$cell_id <- apply(cells, 1, function(r) {
      paste(paste(n_forks, r, sep = "="), collapse = "|")
    })
  } else {
    cells <- data.frame(cell_id = "main", stringsAsFactors = FALSE)
  }

  # estimand declaration per cell: static fields + one field per N fork.
  # N forks fixed at a single level still shape the estimand -> recorded too.
  estimand_cells <- lapply(seq_len(nrow(cells)), function(i) {
    decl <- config$estimand
    for (nf in n_forks) {
      decl[[forks[[nf]]$estimand_field]] <- cells[[nf]][i]
    }
    for (fx in names(fixed)) {
      if (decision_map[[fx]] == "changes_question") {
        decl[[forks[[fx]]$estimand_field]] <- fixed[[fx]]
      }
    }
    decl
  })
  names(estimand_cells) <- cells$cell_id

  # ---- inference grid -------------------------------------------------------
  grid_dims <- c(varying[n_forks], varying[eu_forks])
  if (length(grid_dims) == 0) {
    stop("Domain \"", config$domain, "\": no fork varies after validation -- ",
         "there is no multiverse to build. See the availability table.",
         call. = FALSE)
  }
  grid <- expand.grid(grid_dims, stringsAsFactors = FALSE,
                      KEEP.OUT.ATTRS = FALSE)
  for (fx in names(fixed)) grid[[fx]] <- fixed[[fx]]
  grid$layer <- "inference"

  planned_n <- prod(vapply(validation$levels[names(validation$levels) %in%
                                               setdiff(names(forks), names(which(decision_map == "indefensible")))],
                           length, integer(1)))

  # ---- method x dependency compatibility ------------------------------------
  if (all(c("ma_method", "dependency") %in% names(grid))) {
    compat <- do.call(rbind, lapply(unique(grid$ma_method), function(m) {
      reg <- .ma_method_registry[[m]]
      if (is.null(reg)) {
        stop("ma_method \"", m, "\" is not registered. Available: ",
             paste(list_ma_methods(), collapse = ", "), call. = FALSE)
      }
      data.frame(ma_method = m, dependency = reg$deps,
                 stringsAsFactors = FALSE)
    }))
    before <- nrow(grid)
    grid <- merge(grid, compat, by = c("ma_method", "dependency"))
    incompatible_n <- before - nrow(grid)
  } else {
    incompatible_n <- 0L
  }

  # ---- bias layer (X forks) -------------------------------------------------
  x_forks <- names(forks)[decision_map == "indefensible"]
  bias_rows <- NULL
  if (length(x_forks) > 0) {
    ref_spec <- lapply(names(c(varying, fixed)), function(nm) {
      if (nm %in% names(fixed)) fixed[[nm]] else forks[[nm]]$reference
    })
    names(ref_spec) <- names(c(varying, fixed))
    bias_rows <- do.call(rbind, lapply(x_forks, function(xf) {
      levels <- names(validation$levels[[xf]]) %||%
        names(forks[[xf]]$levels)
      per_cell <- lapply(seq_len(nrow(cells)), function(i) {
        base <- as.data.frame(ref_spec, stringsAsFactors = FALSE)
        for (nf in n_forks) base[[nf]] <- cells[[nf]][i]
        base <- base[rep(1, length(levels)), , drop = FALSE]
        base[[xf]] <- levels
        base$x_fork <- xf
        base
      })
      do.call(rbind, per_cell)
    }))
    bias_rows$layer <- "bias"
    # X columns absent from the inference grid; align columns
    for (col in setdiff(names(bias_rows), names(grid))) grid[[col]] <- NA
    for (col in setdiff(names(grid), names(bias_rows))) bias_rows[[col]] <- NA
    grid <- rbind(grid, bias_rows[, names(grid)])
  }

  # ---- ids ------------------------------------------------------------------
  if (length(n_forks) > 0) {
    grid$cell_id <- apply(grid[, n_forks, drop = FALSE], 1, function(r) {
      paste(paste(n_forks, r, sep = "="), collapse = "|")
    })
  } else {
    grid$cell_id <- "main"
  }
  grid$domain <- config$domain

  fork_cols <- intersect(names(forks), names(grid))
  grid$spec_id <- vapply(seq_len(nrow(grid)), function(i) {
    pairs <- paste(fork_cols,
                   vapply(fork_cols, function(fc) {
                     v <- grid[[fc]][i]
                     if (is.na(v)) "." else v
                   }, character(1)),
                   sep = "=")
    paste0(config$domain, "::", grid$layer[i], "::",
           paste(pairs, collapse = "|"))
  }, character(1))
  stopifnot(!anyDuplicated(grid$spec_id))
  grid$row_id <- seq_len(nrow(grid))

  structure(
    grid,
    class = c("mv_spec_grid", "data.frame"),
    config = config,
    validation = validation,
    estimand_cells = estimand_cells,
    fixed_forks = fixed,
    dropped_forks = dropped,
    decision_map = decision_map,
    family_map = family_map,
    attrition = list(
      planned = planned_n,
      defined = sum(grid$layer == "inference"),
      incompatible_method_dependency = incompatible_n,
      bias_layer = sum(grid$layer == "bias")
    )
  )
}

#' Summarize a specification grid
#'
#' @param object An \code{mv_spec_grid}.
#' @param ... Unused.
#' @export
summary.mv_spec_grid <- function(object, ...) {
  at <- attr(object, "attrition")
  cat("<specification grid> ", attr(object, "config")$domain, "\n", sep = "")
  cat("  estimand cells: ",
      length(attr(object, "estimand_cells")), "\n", sep = "")
  cat("  inference specs (defined): ", at$defined,
      "  [planned before viability/compatibility: ", at$planned, "]\n",
      sep = "")
  cat("  bias-layer specs: ", at$bias_layer, "\n", sep = "")
  if (length(attr(object, "fixed_forks"))) {
    fx <- attr(object, "fixed_forks")
    cat("  fixed forks: ",
        paste(names(fx), unlist(fx), sep = " = ", collapse = "; "),
        "\n", sep = "")
  }
  if (length(attr(object, "dropped_forks"))) {
    cat("  dropped forks (no viable level): ",
        paste(attr(object, "dropped_forks"), collapse = ", "), "\n", sep = "")
  }
  invisible(object)
}
