#' Check Data for Multiverse Analysis
#'
#' This function validates a dataset to ensure it meets the requirements
#' for multiverse analysis. It checks for the presence of required columns,
#' correct data types, unique effect size IDs, and missing values.
#'
#' @param data A data frame containing the dataset to validate.
#'   It must include columns for study metadata, effect size, and variances.
#' @return Logical. Returns `TRUE` if all checks pass; otherwise, an error or warning is thrown.
#' @details The function performs the following checks:
#' \itemize{
#'   \item Ensures all required columns (`study`, `es_id`, `yi`, `vi`, and `wf_*` columns) are present.
#'   \item Checks that columns have the correct data types.
#'   \item Validates that `es_id` values are unique.
#'   \item Warns if any required column contains missing values.
#'   \item Verifies that all `wf_*` columns are character variables.
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
  # Required columns
  required_columns <- c(
    "study", "es_id", "yi", "vi"
  )

  # Dynamically detect wf columns (e.g., wf_1, wf_2, ...)
  wf_columns <- grep("^wf_", colnames(data), value = TRUE)

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

  type_mismatches <- purrr::map_lgl(names(expected_types), function(col) {
    if (!inherits(data[[col]], expected_types[[col]])) {
      message(sprintf("Column '%s' has incorrect type. Expected: %s", col, expected_types[[col]]))
      return(TRUE)
    }
    FALSE
  })

  if (any(type_mismatches)) {
    stop("One or more columns have incorrect data types. See messages above.")
  }

  # Check for missing values
  missing_values <- colSums(is.na(data[all_required_columns]))
  if (any(missing_values > 0)) {
    warning("Some required columns contain missing values:\n",
            paste(names(missing_values[missing_values > 0]),
                  missing_values[missing_values > 0],
                  sep = ": ", collapse = "\n"))
  }

  # Check unique es_id
  if (anyDuplicated(data$es_id)) {
    stop("Duplicate values found in 'es_id'. Each effect size must have a unique identifier.")
  }

  # Dynamic validation for wf variables (e.g., categorical levels)
  wf_checks <- purrr::map(wf_columns, function(wf_col) {
    if (!is.character(data[[wf_col]])) {
      message(sprintf("Column '%s' should be a character variable.", wf_col))
      return(FALSE)
    }
    TRUE
  })

  if (!all(unlist(wf_checks))) {
    stop("One or more 'wf_' columns have incorrect data types.")
  }

  # Success message if all checks pass
  message("Data check passed. Dataset is valid.")
  return(TRUE)
}
