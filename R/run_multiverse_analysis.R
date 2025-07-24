#' Run Multiverse Analysis
#'
#' Applies multiverse meta-analysis across all specifications and combines results into a unified data frame.
#'
#' @param data_multiverse Data frame. Contains the dataset with all potential studies and factors
#'   for the multiverse analysis.
#' @param specifications Data frame. Specifications generated using
#'   \code{\link{create_multiverse_specifications}}. Includes columns for "Which" and "How" factors.
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
#' \code{\link{add_pet_peese_corrected}}
#'
#' @importFrom stats complete.cases
#' @examples
#' # Example usage:
#'data_multiverse <- data.frame(
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
#' run_multiverse_analysis(data_multiverse, specifications)
#'
#' @export
run_multiverse_analysis <- function(data_multiverse, specifications) {
  # Step 1: Run multiverse analysis for each specification
  results <- lapply(seq_len(nrow(specifications)), function(i) {
    general_multiverse(i, data_multiverse, specifications)
  })

  # Step 2: Combine results into a single data frame
  final_results <- do.call(rbind, results)

  # Step 3: Add full_set indicator
  final_results$full_set <- as.numeric(final_results$set == paste(1:nrow(data_multiverse), collapse = ","))

  # Step 4: Remove missing values (except pval due to Bayesian methods)
  final_results <- final_results[ complete.cases(final_results[ , !(names(final_results) %in% "pval") ]), ]

  # Step 5: Remove duplicates, keeping first occurrence (more specific which factors)
  final_results <- final_results[!duplicated(final_results[, c("b", "set", "ma_method")]), ]

  # Return final results
  return(final_results)
}
