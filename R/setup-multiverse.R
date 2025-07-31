#' Create Which-Factor Mapping
#'
#' Creates a mapping between user's original variable names and internal wf_* columns,
#' preserving original names for display purposes while standardizing for analysis.
#'
#' @param data Data frame containing the dataset
#' @param which_factors Named character vector mapping display labels to column names,
#'   e.g., c("Population Type" = "pop_type", "Study Design" = "design")
#' @param auto_detect Logical. If TRUE, automatically detects existing wf_* columns
#' @return A list containing the prepared data and mapping information
#'
#' @examples
#' # User has their own column names
#' data <- data.frame(
#'   study = c("Study1", "Study2"),
#'   es_id = 1:2,
#'   yi = c(0.5, 0.3),
#'   vi = c(0.02, 0.03),
#'   population = c("Adults", "Children"),
#'   study_design = c("RCT", "Observational")
#' )
#'
#' # Map to which factors with meaningful labels
#' wf_setup <- setup_which_factors(
#'   data,
#'   which_factors = c("Population Type" = "population",
#'                     "Study Design" = "study_design")
#' )
#'
#' @export
setup_which_factors <- function(data, which_factors = NULL, auto_detect = TRUE) {

  # Input validation
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data.frame")
  }

  # Auto-detect existing wf_* columns if requested
  existing_wf <- grep("^wf_[0-9]+$", names(data), value = TRUE)

  if (auto_detect && length(existing_wf) > 0) {
    # Use existing wf_* columns
    message("Found existing wf_* columns: ", paste(existing_wf, collapse = ", "))

    # Create a basic mapping for existing columns
    wf_mapping <- data.frame(
      wf_internal = existing_wf,
      original_name = existing_wf,
      display_label = paste("Factor", gsub("wf_", "", existing_wf)),
      stringsAsFactors = FALSE
    )

    return(list(
      data = data,
      wf_mapping = wf_mapping,
      wf_vars = existing_wf
    ))
  }

  # Require which_factors if no existing wf_* columns
  if (is.null(which_factors)) {
    stop("No existing wf_* columns found. Please provide 'which_factors' mapping.")
  }

  # Validate which_factors
  if (!is.character(which_factors) || is.null(names(which_factors))) {
    stop("'which_factors' must be a named character vector: c('Display Label' = 'column_name')")
  }

  # Check that all specified columns exist
  missing_cols <- setdiff(which_factors, names(data))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in data: ", paste(missing_cols, collapse = ", "))
  }

  # Create internal wf_* column names
  n_factors <- length(which_factors)
  wf_internal <- paste0("wf_", seq_len(n_factors))

  # Create the mapping dataframe
  wf_mapping <- data.frame(
    wf_internal = wf_internal,
    original_name = unname(which_factors),
    display_label = names(which_factors),
    stringsAsFactors = FALSE
  )

  # Create a copy of data with renamed columns
  data_prepared <- data
  for (i in seq_len(n_factors)) {
    original_col <- which_factors[i]
    new_col <- wf_internal[i]
    data_prepared[[new_col]] <- data_prepared[[original_col]]

    # Convert to character if not already
    if (!is.character(data_prepared[[new_col]])) {
      data_prepared[[new_col]] <- as.character(data_prepared[[new_col]])
    }
  }

  return(list(
    data = data_prepared,
    wf_mapping = wf_mapping,
    wf_vars = wf_internal
  ))
}

#' Get Original Variable Names for Display
#'
#' Retrieves original variable names and labels for use in plots and reports
#'
#' @param wf_mapping Data frame created by setup_which_factors
#' @param wf_internal Character vector of internal wf_* column names
#' @return Named character vector for display purposes
#'
#' @export
get_display_labels <- function(wf_mapping, wf_internal = NULL) {
  if (is.null(wf_internal)) {
    wf_internal <- wf_mapping$wf_internal
  }

  labels <- wf_mapping$display_label[match(wf_internal, wf_mapping$wf_internal)]
  names(labels) <- wf_internal
  return(labels)
}

#' Get Original Column Names
#'
#' Retrieves original column names for reference
#'
#' @param wf_mapping Data frame created by setup_which_factors
#' @param wf_internal Character vector of internal wf_* column names
#' @return Named character vector of original column names
#'
#' @export
get_original_names <- function(wf_mapping, wf_internal = NULL) {
  if (is.null(wf_internal)) {
    wf_internal <- wf_mapping$wf_internal
  }

  original <- wf_mapping$original_name[match(wf_internal, wf_mapping$wf_internal)]
  names(original) <- wf_internal
  return(original)
}

#' Enhanced Data Validation for Multiverse Analysis
#'
#' Updated version of check_data_multiverse that works with the new which-factor system
#'
#' @param data Data frame to validate
#' @param wf_setup List returned from setup_which_factors (optional)
#' @return Validated data with metadata
#'
#' @export
check_data_multiverse_enhanced <- function(data, wf_setup = NULL) {

  # Use the wf_setup if provided, otherwise fall back to original validation
  if (!is.null(wf_setup)) {
    data <- wf_setup$data
    wf_columns <- wf_setup$wf_vars

    # Add the mapping as an attribute for later use
    attr(data, "wf_mapping") <- wf_setup$wf_mapping
  } else {
    # Fall back to original detection method
    wf_columns <- grep("^wf_[0-9]+$", colnames(data), value = TRUE)
  }

  # Run original validation logic (abbreviated for space)
  # ... [include all your existing validation code here] ...

  # Required columns
  required_columns <- c("study", "es_id", "yi", "vi")
  all_required_columns <- c(required_columns, wf_columns)

  # Check if all required columns are present
  missing_columns <- setdiff(all_required_columns, colnames(data))
  if (length(missing_columns) > 0) {
    stop(paste("Missing required columns:", paste(missing_columns, collapse = ", ")))
  }

  # Success message
  if (!is.null(wf_setup)) {
    message("Data validation passed with which-factor mapping:")
    for (i in 1:nrow(wf_setup$wf_mapping)) {
      message(sprintf("  %s ('%s') -> %s",
                      wf_setup$wf_mapping$display_label[i],
                      wf_setup$wf_mapping$original_name[i],
                      wf_setup$wf_mapping$wf_internal[i]))
    }
  } else {
    message("Data validation passed. Dataset is ready for multiverse analysis.")
  }

  attr(data, "multiverse_validated") <- TRUE
  attr(data, "validation_timestamp") <- Sys.time()
  return(data)
}

#' Enhanced General Multiverse Function
#'
#' Updated to work with the enhanced which-factor system
#'
#' @inheritParams general_multiverse
#' @export
general_multiverse_enhanced <- function(i, data_multiverse, specifications, k_smallest_ma = getOption("metaMultiverse.k_smallest_ma", 5)) {

  # Get the wf_mapping if available
  wf_mapping <- attr(data_multiverse, "wf_mapping")

  # Your existing logic here...
  out <- list()
  dat <- as.data.frame(data_multiverse)

  # Apply "Which" factors dynamically
  for (wf in names(specifications)[grepl("^wf_", names(specifications))]) {
    filter_value <- specifications[[wf]][i]
    if (!grepl("^total_", filter_value)) {
      dat <- dat[dat[[wf]] == filter_value, ]
    }
  }

  # ... rest of your existing logic ...

  # When returning results, you can now include display information
  out <- data.frame(
    multiverse_id = specifications$multiverse_id[i],
    specifications[i, ],
    b     = as.numeric(mod$b),
    ci.lb = as.numeric(mod$ci.lb),
    ci.ub = as.numeric(mod$ci.ub),
    pval  = as.numeric(mod$pval),
    k     = nrow(dat),
    set   = paste(dat$es_id, collapse = ",")
  )

  # Add display labels as attributes for plotting/reporting
  if (!is.null(wf_mapping)) {
    attr(out, "wf_mapping") <- wf_mapping
  }

  return(out)
}

# Example usage workflow:
example_usage <- function() {
  # 1. User provides data with their own column names
  user_data <- data.frame(
    study = c("Smith2020", "Jones2021", "Brown2019"),
    es_id = 1:3,
    yi = c(0.5, 0.3, 0.7),
    vi = c(0.02, 0.03, 0.025),
    population_type = c("Adults", "Children", "Adults"),
    study_design = c("RCT", "Observational", "RCT"),
    stringsAsFactors = FALSE
  )

  # 2. Setup which factors with meaningful labels
  wf_setup <- setup_which_factors(
    user_data,
    which_factors = c(
      "Population Type" = "population_type",
      "Study Design" = "study_design"
    )
  )

  # 3. Validate the data
  validated_data <- check_data_multiverse_enhanced(user_data, wf_setup)

  # 4. For plotting/reporting, get display labels
  display_labels <- get_display_labels(wf_setup$wf_mapping)
  # Returns: c(wf_1 = "Population Type", wf_2 = "Study Design")

  # 5. Get original column names if needed
  original_names <- get_original_names(wf_setup$wf_mapping)
  # Returns: c(wf_1 = "population_type", wf_2 = "study_design")

  return(list(
    data = validated_data,
    display_labels = display_labels,
    original_names = original_names
  ))
}
