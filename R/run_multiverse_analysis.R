#' Run Multiverse Analysis
#'
#' Applies multiverse meta-analysis across all specifications and combines results into a unified data frame.
#'
#' @param data Data frame. Contains the dataset with all potential studies and factors
#'   for the multiverse analysis.
#' @param specifications Data frame. Specifications generated using
#'   \code{\link{create_multiverse_specifications}}. Includes columns for "Which" and "How" factors.
#' @param verbose Logical. If TRUE, prints progress messages and warnings summary.
#' @param progress Logical. If TRUE, displays a progress bar (default: TRUE).
#'
#' @return A data frame containing the aggregated results of the multiverse analysis, including:
#'   - Effect size estimate (\code{b}),
#'   - Confidence intervals (\code{ci.lb} and \code{ci.ub}),
#'   - P-value (\code{pval}),
#'   - Number of studies included in the subset (\code{k}),
#'   - The subset of study IDs included in the analysis (\code{set}),
#'   - The "full set" indicator (\code{full_set}).
#'
#' @details
#' The function iterates through each row of the `specifications` data frame and applies
#' the corresponding filters and meta-analysis methods using \code{\link{general_multiverse}}.
#' Results are combined, deduplicated, and adjusted as needed. If "pet-peese (corrected)" is included
#' in \code{how_methods}, corrected PET-PEESE results are appended to the final output.
#'
#' Steps:
#' 1. Iterates through specifications to apply \code{\link{general_multiverse}}.
#' 2. Combines individual results into a unified data frame.
#' 3. Adds an indicator (\code{full_set}) to identify if all studies are included in the analysis.
#' 4. Removes missing values.
#' 5. Deduplicates results by keeping the first occurrence of duplicate rows.
#'
#' @seealso
#' \code{\link{create_multiverse_specifications}},
#' \code{\link{general_multiverse}},
#'
#' @importFrom stats complete.cases
#' @examples
#' # Example usage:
#'data <- data.frame(
#'  study = rep(paste0("S", 1:6), each = 2),
#'  es_id = 1:12,
#'  yi = rnorm(12, mean = 0.5),
#'  vi = runif(12, min = 0.01, max = 0.1),
#'  wf_1 = rep(c("A", "B"), times = 6)
#')
#'specifications <- data.frame(
#'  wf_1 = c("A", "B"),
#'  dependency = "aggregate",
#'  ma_method = "reml"
#')
#'
#' # Run function
#' run_multiverse_analysis(data, specifications)
#'
#' @export
run_multiverse_analysis <- function(data, specifications, verbose = FALSE, progress = TRUE) {

  multiverse_warnings <- character(0)
  n_specs <- nrow(specifications)

  # Initialize progress bar if requested
  if (progress) {
    # Check if we're in a Shiny environment
    if (exists(".shiny_session", envir = .GlobalEnv) ||
        !is.null(getOption("shiny.in.session")) ||
        any(grepl("shiny", search()))) {

      # In Shiny: use withProgress and incProgress
      shiny::withProgress(message = 'Running multiverse analysis...', value = 0, {

        # Step 1: Run multiverse analysis for each specification
        results <- lapply(seq_len(n_specs), function(i) {
          # Update progress
          shiny::incProgress(1/n_specs, detail = paste("Processing specification", i, "of", n_specs))

          tryCatch({
            withCallingHandlers({
              general_multiverse(i, data, specifications)
            },
            warning = function(w) {
              multiverse_warnings <<- c(multiverse_warnings, w$message)
              invokeRestart("muffleWarning")
            })
          },
          error = function(e) {
            multiverse_warnings <<- c(multiverse_warnings,
                                      paste0("Specification ", i, " ERROR: ", e$message))
            NULL
          })
        })

      })

    } else {
      # Not in Shiny: use txtProgressBar
      pb <- txtProgressBar(min = 0, max = n_specs, style = 3, width = 50, char = "=")

      # Step 1: Run multiverse analysis for each specification
      results <- lapply(seq_len(n_specs), function(i) {
        # Update progress bar
        setTxtProgressBar(pb, i)

        tryCatch({
          withCallingHandlers({
            general_multiverse(i, data, specifications)
          },
          warning = function(w) {
            multiverse_warnings <<- c(multiverse_warnings, w$message)
            invokeRestart("muffleWarning")
          })
        },
        error = function(e) {
          multiverse_warnings <<- c(multiverse_warnings,
                                    paste0("Specification ", i, " ERROR: ", e$message))
          NULL
        })
      })

      close(pb)
      cat("\n") # New line after progress bar
    }

  } else {
    # No progress bar
    results <- lapply(seq_len(n_specs), function(i) {
      tryCatch({
        withCallingHandlers({
          general_multiverse(i, data, specifications)
        },
        warning = function(w) {
          multiverse_warnings <<- c(multiverse_warnings, w$message)
          invokeRestart("muffleWarning")
        })
      },
      error = function(e) {
        multiverse_warnings <<- c(multiverse_warnings,
                                  paste0("Specification ", i, " ERROR: ", e$message))
        NULL
      })
    })
  }

  # Remove NULL results (from skipped specifications)
  results <- results[!sapply(results, is.null)]

  # Check if we have any valid results
  if (length(results) == 0) {
    warning("No valid specifications could be processed. All were skipped or failed.")
    result <- list(
      results = NULL,
      multiverse_warnings = multiverse_warnings,
      n_warnings = length(multiverse_warnings)
    )
    class(result) <- "multiverse_result"
    return(result)
  }

  # Step 2: Combine results into a single data frame
  final_results <- do.call(rbind, results)

  # Step 3: Add full_set indicator
  final_results$full_set <- as.numeric(final_results$set == paste(1:nrow(data), collapse = ","))

  # Step 4: Remove missing values (except pval due to Bayesian methods)
  final_results <- final_results[complete.cases(final_results[, !(names(final_results) %in% "pval")]), ]

  # Step 5: Remove duplicates, keeping first occurrence (more specific which factors)
  final_results <- final_results[!duplicated(final_results[, c("b", "set", "ma_method")]), ]

  result <- list(
    results = final_results,
    multiverse_warnings = multiverse_warnings,
    n_warnings = length(multiverse_warnings)
  )

  if (verbose && length(multiverse_warnings) > 0) {
    message(sprintf("Analysis completed with %d warnings. Access with result$multiverse_warnings",
                    length(multiverse_warnings)))
  }

  class(result) <- "multiverse_result"
  return(result)
}
