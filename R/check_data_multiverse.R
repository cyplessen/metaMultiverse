#' Check Data for Multiverse Analysis
#'
#' This function validates a dataset to ensure it meets the requirements
#' for multiverse analysis. It checks for the presence of required columns,
#' correct data types, unique effect size IDs, and missing values.
#'
#' @param data A data frame containing the dataset to validate.
#'   It must include columns for study metadata, effect size, and variances.
#' @return Logical. Returns TRUE if all checks pass; otherwise, an error or warning is thrown.
#' @details The function performs the following checks:
#' \itemize{
#'   \item Ensures all required columns (study, es_id, yi, vi, and wf_* columns) are present.
#'   \item Checks that columns have the correct data types.
#'   \item Validates that es_id values are unique.
#'   \item Warns if any required column contains missing values.
#'   \item Verifies that all wf_* columns are character variables.
#'   \item Checks for reasonable effect size and variance values.
#'   \item Validates that variances are positive.
#'   \item Warns about potential outliers in effect sizes.
#' }
#' @examples
#' # Example dataset
#'    example_data <-  data.frame(
#'               study = as.character(c("Study 1", "Study 2")),
#'               es_id = as.numeric(1:2),
#'               yi = as.numeric(c(0.5, 0.6)),
#'               vi = as.numeric(c(0.02, 0.03)),
#'               sei = as.numeric(c(0.14, 0.17)),
#'               wf_1 = as.character(c("A", "B"))
#'               )
#'
#' # Run the check
#' check_data_multiverse(example_data)
#' @export
check_data_multiverse <- function(data) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data.frame")
  }

  if (nrow(data) == 0) {
    stop("Input data is empty (0 rows)")
  }

  # Required columns
  required_columns <- c(
    "study", "es_id", "yi", "vi"
  )

  # Dynamically detect wf columns (e.g., wf_1, wf_2, ...)
  wf_columns <- grep("^wf", colnames(data), value = TRUE)

  # Combine required and dynamically detected wf columns
  all_required_columns <- c(required_columns, wf_columns)

  # Check if all required columns are present
  missing_columns <- setdiff(all_required_columns, colnames(data))
  if (length(missing_columns) > 0) {
    stop(paste("Missing required columns:", paste(missing_columns, collapse = ", ")))
  }

  # Check data types
  expected_types <- list(
    study = "character",
    es_id = "numeric",
    yi = "numeric",
    vi = "numeric"
  )

  type_errors <- c()
  for (col in names(expected_types)) {
    if (col %in% names(data)) {
      actual_type <- class(data[[col]])[1]
      expected_type <- expected_types[[col]]

      if (expected_type == "character" && !is.character(data[[col]])) {
        type_errors <- c(type_errors, sprintf("Column '%s' is %s but should be character", col, actual_type))
      } else if (expected_type == "numeric" && !is.numeric(data[[col]])) {
        type_errors <- c(type_errors, sprintf("Column '%s' is %s but should be numeric", col, actual_type))
      }
    }
  }

  if (length(type_errors) > 0) {
    stop(paste("Data type errors found:\n", paste(type_errors, collapse = "\n")))
  }

  # Check unique es_id
  if (anyDuplicated(data$es_id)) {
    stop("Duplicate values found in 'es_id'. Each effect size must have a unique identifier.")
  }

  # Dynamic validation for wf variables (e.g., categorical levels)
  wf_errors <- c()
  for (wf_col in wf_columns) {
    if (!is.character(data[[wf_col]])) {
      actual_type <- class(data[[wf_col]])[1]
      wf_errors <- c(wf_errors, sprintf("Column '%s' is %s but should be character", wf_col, actual_type))
    }
  }

  if (length(wf_errors) > 0) {
    stop(paste("Which factor column errors:\n", paste(wf_errors, collapse = "\n")))
  }

  # Additional validation checks

  # Check for non-finite values in numeric columns
  numeric_cols <- c("yi", "vi")
  for (col in numeric_cols) {
    if (any(!is.finite(data[[col]]), na.rm = TRUE)) {
      non_finite_count <- sum(!is.finite(data[[col]]), na.rm = TRUE)
      stop(paste("Found", non_finite_count, "non-finite values (Inf, -Inf, NaN) in column", col))
    }
  }

  # Check for positive variances - make this an error since negative variances are impossible
  if (any(data$vi <= 0, na.rm = TRUE)) {
    negative_vi_count <- sum(data$vi <= 0, na.rm = TRUE)
    stop(paste("Found", negative_vi_count, "non-positive variance values. All variances must be positive."))
  }

  # Check for extremely large effect sizes (potential data entry errors)
  extreme_yi <- abs(data$yi) > 10
  if (any(extreme_yi, na.rm = TRUE)) {
    extreme_count <- sum(extreme_yi, na.rm = TRUE)
    warning(paste("Found", extreme_count, "effect sizes with absolute value > 10. Please verify these are correct."))
  }

  # Check for unreasonably large Cohen's d (common calculation error)
  large_d <- abs(data$yi) > 2.5
  if (any(large_d, na.rm = TRUE)) {
    large_d_count <- sum(large_d, na.rm = TRUE)
    warning(paste("Found", large_d_count, "unreasonably large d detected (|d| > 2.5). Check if SD and SE were confused when calculating SMD."))
  }

  # Check for extremely large variances (potential data entry errors)
  extreme_vi <- data$vi > 100
  if (any(extreme_vi, na.rm = TRUE)) {
    extreme_var_count <- sum(extreme_vi, na.rm = TRUE)
    warning(paste("Found", extreme_var_count, "variances > 100. Please verify these are correct."))
  }

  # Check for minimum number of studies
  n_studies <- length(unique(data$study))
  if (n_studies < 3) {
    warning(paste("Only", n_studies, "unique studies found. Meta-analysis typically requires at least 3 studies."))
  }

  # Check for studies with only one effect size each
  effects_per_study <- table(data$study)
  single_effect_studies <- sum(effects_per_study == 1)
  if (single_effect_studies == length(effects_per_study)) {
    message("All studies contribute exactly one effect size. Dependency handling may not be necessary.")
  } else if (single_effect_studies > 0) {
    message(paste(single_effect_studies, "studies contribute only one effect size,",
                  length(effects_per_study) - single_effect_studies, "studies contribute multiple effect sizes."))
  }

  # Check for missing values in required columns
  missing_values <- colSums(is.na(data[all_required_columns]))
  if (any(missing_values > 0)) {
    warning("Some required columns contain missing values:\n",
            paste(names(missing_values[missing_values > 0]),
                  missing_values[missing_values > 0],
                  sep = ": ", collapse = "\n"))
  }

  # Check for consistency between yi and vi (rough sanity check)
  if ("sei" %in% names(data)) {
    # If standard error is available, check consistency with variance
    sei_from_vi <- sqrt(data$vi)
    sei_diff <- abs(data$sei - sei_from_vi)
    large_diff <- sei_diff > 0.01 & !is.na(sei_diff)
    if (any(large_diff, na.rm = TRUE)) {
      warning(paste("Found", sum(large_diff, na.rm = TRUE),
                    "cases where sei and sqrt(vi) differ substantially. Please verify data consistency."))
    }
  }

  # Success message if all checks pass
  message("Data validation passed. Dataset is ready for multiverse analysis.")
  attr(data, "multiverse_validated") <- TRUE
  attr(data, "validation_timestamp") <- Sys.time()
  return(data)
}
