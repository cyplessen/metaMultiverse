#' Run Multiverse Analysis
#'
#' Applies multiverse meta-analysis across all specifications and combines results into a unified data frame.
#'
#' @param data_multiverse Data frame. Contains the dataset with all potential studies and factors
#'   for the multiverse analysis.
#' @param specifications Data frame. Specifications generated using
#'   \code{\link{create_multiverse_specifications}}. Includes columns for "Which" and "How" factors.
#' @param how_methods Character vector. A list of meta-analysis methods to apply for the "How" factors.
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
#' 6. Adds PET-PEESE corrected results if specified.
#'
#' @seealso
#' \code{\link{create_multiverse_specifications}},
#' \code{\link{general_multiverse}},
#' \code{\link{add_pet_peese_corrected}}
#'
#' @importFrom stats complete.cases
#' @examples
#' # Example usage:
#' final_results <- run_multiverse_analysis(
#'   data_multiverse = my_data,
#'   specifications = my_specifications,
#'   how_methods = c("reml", "fe", "p-uniform", "pet-peese (corrected)")
#' )
#'
#' @export
run_multiverse_analysis <- function(data_multiverse, specifications, how_methods) {
  # Step 1: Run multiverse analysis for each specification
  results <- lapply(seq_len(nrow(specifications)), function(i) {
    general_multiverse(i, data_multiverse, specifications, how_methods)
  })

  # Step 2: Combine results into a single data frame
  final_results <- do.call(rbind, results)

  # Step 3: Add full_set indicator
  final_results$full_set <- as.numeric(final_results$set == paste(1:nrow(data_multiverse), collapse = ","))

  # Step 4: Remove missing values
  final_results <- final_results[stats::complete.cases(final_results), ]

  # Step 5: Remove duplicates, keeping first occurrence (more specific which factors)
  final_results <- final_results[!duplicated(final_results[, c("b", "set", "ma_method")]), ]

  # Step 6: Add PET-PEESE corrected results if specified
  if ("pet-peese (corrected)" %in% how_methods) {
    final_results <- add_pet_peese_corrected(final_results)
  }

  # Return final results
  return(final_results)
}
