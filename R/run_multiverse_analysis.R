#' Run Multiverse Meta-Analysis
#'
#' Executes meta-analyses across all specifications from a multiverse setup.
#' Handles specification failures gracefully and captures warnings for review.
#'
#' @param spec_output Output from \code{\link{create_multiverse_specifications}} containing
#'   specifications and metadata. Legacy support for direct specifications data frame
#'   (deprecated).
#' @param verbose Logical. Print progress messages and summary. Default: FALSE
#' @param progress Logical. Display progress bar. Default: TRUE
#'
#' @return Object of class \code{multiverse_result} containing:
#' \describe{
#'   \item{results}{Data frame with one row per unique meta-analysis, including:
#'     \itemize{
#'       \item \code{b}: Effect size estimate
#'       \item \code{ci.lb}, \code{ci.ub}: 95\% confidence interval
#'       \item \code{pval}: P-value (NA for Bayesian methods)
#'       \item \code{k}: Number of studies
#'       \item \code{set}: Study IDs included
#'       \item \code{full_set}: Binary (1 = all studies, 0 = subset)
#'       \item All which/how factor columns
#'       \item \code{multiverse_id}: Identifier for separate multiverses
#'     }
#'   }
#'   \item{multiverse_warnings}{Character vector of warnings/errors}
#'   \item{n_warnings}{Total warning count}
#'   \item{n_attempted}{Specifications attempted}
#'   \item{n_successful}{Successful analyses}
#'   \item{n_final}{Unique results after deduplication}
#'   \item{factor_groups}{Custom grouping definitions}
#'   \item{specifications}{Original specification grid}
#'   \item{data}{Original dataset}
#' }
#'
#' @details
#' For each specification, the function:
#' \enumerate{
#'   \item Filters data based on which factors
#'   \item Handles dependencies (aggregate/model/select)
#'   \item Applies the meta-analytic method
#'   \item Captures results or errors
#' }
#'
#' Failed specifications don't stop execution. Access warnings via
#' \code{result$multiverse_warnings} for debugging.
#'
#' @examples
#' \dontrun{
#' # Complete pipeline
#' results <- data %>%
#'   check_data_multiverse() %>%
#'   define_factors(
#'     Population = "pop_type",
#'     Quality = list("quality",
#'                   decision = "U",
#'                   groups = list(
#'                     high = c("A", "B"),
#'                     all = c("A", "B", "C")
#'                   ))
#'   ) %>%
#'   create_multiverse_specifications() %>%
#'   run_multiverse_analysis(verbose = TRUE)
#'
#' # Check warnings
#' results$multiverse_warnings
#' }
#'
#' @seealso
#' \code{\link{define_factors}} for factor setup
#' \code{\link{create_multiverse_specifications}} for specification generation
#' \code{\link{plot_spec_curve}}, \code{\link{plot_voe}} for visualization
#'
#' @export
run_multiverse_analysis <- function(spec_output, verbose = FALSE, progress = TRUE) {

  # Handle both direct specifications and full output
  if (inherits(spec_output, "data.frame")) {
    # Legacy: direct specifications data frame
    specifications <- spec_output
    data <- NULL
    factor_groups <- attr(specifications, "factor_groups")
    warning("Direct specification input is deprecated. Use create_multiverse_specifications() output.")
  } else if (inherits(spec_output, "list") && "specifications" %in% names(spec_output)) {
    # New: output from create_multiverse_specifications
    specifications <- spec_output$specifications
    factor_setup <- spec_output$factor_setup
    data <- factor_setup$data
    factor_groups <- factor_setup$factor_groups
  } else {
    stop("Input must be output from create_multiverse_specifications()")
  }

  # Validate we have what we need
  if (is.null(data)) {
    stop("Cannot find data. Use the full pipeline starting with define_factors()")
  }

  # Input validation
  if (!inherits(data, "data.frame")) stop("'data' must be a data frame")
  if (!inherits(specifications, "data.frame")) stop("'specifications' must be a data frame")
  if (nrow(specifications) == 0) stop("'specifications' cannot be empty")

  # Check for factor_groups in specifications attributes if not provided
  if (is.null(factor_groups)) {
    factor_groups <- attr(specifications, "factor_groups")
  }

  # Unvalidated-data warning
  if (is.null(attr(data, "multiverse_validated"))) {
    warning("Data appears unvalidated. Consider running check_data_multiverse() first.")
  }

  multiverse_warnings <- character(0)
  n_specs <- nrow(specifications)

  if (verbose) {
    cat("[*] Starting multiverse analysis...\n")
    if (!is.null(factor_groups) && length(factor_groups) > 0) {
      cat("   Using custom factor groupings for", length(factor_groups), "factors\n")
    }
  }

  if (progress) {
    pb <- txtProgressBar(min = 0, max = n_specs, style = 3, width = 60, char = "=")
  }

  # Main analysis loop
  results <- lapply(seq_len(n_specs), function(i) {
    if (progress) setTxtProgressBar(pb, i)

    spec_i <- specifications[i, , drop = FALSE]
    spec_info <- paste0("[dep=", spec_i$dependency, ", method=", spec_i$ma_method, "]")

    tryCatch({
      withCallingHandlers({
        general_multiverse(i, data, specifications, factor_groups)
      },
      warning = function(w) {
        multiverse_warnings <<- c(
          multiverse_warnings,
          paste0("Spec ", i, " ", spec_info, ": warning: ", w$message)
        )
        invokeRestart("muffleWarning")
      })
    },
    error = function(e) {
      multiverse_warnings <<- c(
        multiverse_warnings,
        paste0("Spec ", i, " ", spec_info, ": FAILED: ", e$message)
      )
      if (verbose) {
        cat(sprintf("\n[!] Specification %d %s failed: %s\n",
                    i, spec_info, e$message))
      }
      NULL
    })
  })

  if (progress) close(pb)

  # Process results
  successes <- results[!sapply(results, is.null)]
  if (length(successes) == 0) {
    warning(sprintf("All %d specifications failed. Check your data and specifications.", n_specs))
    return(structure(
      list(
        results = NULL,
        multiverse_warnings = multiverse_warnings,
        n_warnings = length(multiverse_warnings),
        n_attempted = n_specs,
        n_successful = 0,
        n_final = 0,
        factor_groups = factor_groups,
        specifications = specifications,
        data = data
      ),
      class = "multiverse_result"
    ))
  }

  # Combine and clean results
  final_df <- do.call(rbind, successes)
  full_set_val <- paste(seq_len(nrow(data)), collapse = ",")
  final_df$full_set <- as.numeric(final_df$set == full_set_val)

  core_vars <- c("b", "ci.lb", "ci.ub", "k", "set")
  final_df <- final_df[complete.cases(final_df[, core_vars]), ]

  dup_key <- c("b", "set", "ma_method", "dependency")
  final_df <- final_df[!duplicated(final_df[, dup_key]), ]

  # Create final result object (only once!)
  result <- structure(
    list(
      results = final_df,
      multiverse_warnings = multiverse_warnings,
      n_warnings = length(multiverse_warnings),
      n_attempted = n_specs,
      n_successful = length(successes),
      n_final = nrow(final_df),
      factor_groups = factor_groups,
      specifications = specifications,
      data = data
    ),
    class = "multiverse_result"
  )

  if (verbose) {
    cat("\n[OK] Multiverse analysis completed!\n")
    cat("   Results:", nrow(final_df), "unique meta-analyses\n")
    cat("   Warnings:", length(multiverse_warnings), "\n")
    cat("   Success rate:", round(100 * length(successes) / n_specs, 1), "%\n")
    if (length(multiverse_warnings) > 0) {
      cat("\n[i] Access warnings with: result$multiverse_warnings\n")
    }
  }

  return(result)
}
