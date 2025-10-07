#' Create Multiverse Specifications
#'
#' Generates a grid of analysis specifications from factor definitions.
#' Each specification represents one unique combination of study selection
#' criteria, meta-analytic method, and dependency handling approach.
#'
#' @param factor_setup Output from \code{\link{define_factors}} containing
#'   factor definitions, decision types, and optional custom groupings
#' @param ma_methods Character vector of meta-analytic methods.
#'   Default: NULL (uses all available methods)
#' @param dependencies Character vector of dependency handling strategies.
#'   Default: c("aggregate", "modeled")
#'
#' @return List containing:
#' \describe{
#'   \item{specifications}{Data frame where each row is one analysis specification with:
#'     \itemize{
#'       \item Which factor columns (wf_*) with levels or group names
#'       \item \code{ma_method}: Meta-analytic method to apply
#'       \item \code{dependency}: Dependency handling strategy
#'       \item \code{multiverse_id}: Identifier for separate multiverses (from N-type factors)
#'       \item \code{row_id}: Unique specification identifier
#'     }
#'   }
#'   \item{number_specs}{Total number of specifications generated}
#'   \item{factor_setup}{Original factor setup (preserved for downstream use)}
#' }
#'
#' @details
#' Specification generation follows these rules:
#' \itemize{
#'   \item \strong{E/U factors}: Create options for each level plus "total_*" (all combined)
#'   \item \strong{N factors}: Create separate multiverses for each level (no combinations)
#'   \item \strong{Custom groups}: Use group names instead of raw data levels
#'   \item \strong{Valid combinations}: Only includes method-dependency pairs that are compatible
#' }
#'
#' Available methods retrieved via \code{\link{list_ma_methods}}.
#' Invalid method-dependency combinations are automatically filtered out.
#'
#' @examples
#' \dontrun{
#' # Basic usage with defaults
#' specs <- setup %>%
#'   create_multiverse_specifications()
#'
#' # Specific methods only
#' specs <- setup %>%
#'   create_multiverse_specifications(
#'     ma_methods = c("reml", "fe", "pet-peese"),
#'     dependencies = "aggregate"
#'   )
#'
#' # Pipeline usage
#' results <- data %>%
#'   check_data_multiverse() %>%
#'   define_factors(
#'     Population = "pop_type|E",
#'     Quality = "quality|N"
#'   ) %>%
#'   create_multiverse_specifications() %>%
#'   run_multiverse_analysis()
#' }
#'
#' @seealso
#' \code{\link{define_factors}} for factor setup
#' \code{\link{list_ma_methods}} for available methods
#' \code{\link{run_multiverse_analysis}} for executing specifications
#'
#' @export
create_multiverse_specifications <- function(factor_setup,
                                             ma_methods = NULL,
                                             dependencies = c("aggregate", "modeled")) {

  # Validate input
  if (!inherits(factor_setup, "multiverse_factor_setup")) {
    stop("Input must be output from define_factors()")
  }

  # Extract everything we need from factor_setup
  data <- factor_setup$data
  wf_vars <- factor_setup$factors$wf_internal
  decision_map <- factor_setup$decision_map
  factor_groups <- factor_setup$factor_groups

  # Use all available methods if not specified
  if (is.null(ma_methods)) {
    ma_methods <- list_ma_methods()
    message("Using all available meta-analytic methods: ",
            paste(ma_methods, collapse = ", "))
  }

  # Validate methods
  available_methods <- list_ma_methods()
  invalid_methods <- setdiff(ma_methods, available_methods)
  if (length(invalid_methods) > 0) {
    stop("Invalid ma_methods: ", paste(invalid_methods, collapse = ", "),
         "\nAvailable: ", paste(available_methods, collapse = ", "))
  }

  # Build factor levels (with custom group support)
  wf_factors <- lapply(wf_vars, function(wf) {

    # Check for custom groups
    if (!is.null(factor_groups) && wf %in% names(factor_groups)) {
      # Use custom group names
      return(names(factor_groups[[wf]]))
    }

    # Standard: use data levels
    vals <- unique(data[[wf]])
    vals <- vals[!is.na(vals)]

    if (decision_map[[wf]] == "N") {
      # Non-equivalent: just raw levels
      vals
    } else {
      # E or U: add "total_" option
      c(vals, paste0("total_", wf))
    }
  })
  names(wf_factors) <- wf_vars

  # Generate specifications grid
  grid_args <- c(wf_factors,
                 list(dependency = dependencies,
                      ma_method = ma_methods))

  specs <- do.call(expand.grid, grid_args)

  # Add multiverse IDs for N-type factors
  type_N <- names(decision_map)[decision_map == "N"]
  if (length(type_N) > 0) {
    specs$multiverse_id <- apply(
      specs[, type_N, drop = FALSE],
      1, paste, collapse = "|"
    )
  } else {
    specs$multiverse_id <- "main"
  }

  # Filter valid method-dependency combinations
  dep_table <- purrr::map_dfr(ma_methods, function(m) {
    tibble::tibble(
      ma_method = m,
      dependency = .ma_method_registry[[m]]$deps
    )
  })

  specs <- specs |>
    dplyr::inner_join(dep_table, by = c("ma_method", "dependency")) |>
    dplyr::mutate(row_id = dplyr::row_number())

  # Attach factor info for downstream use
  attr(specs, "factor_groups") <- factor_groups
  attr(specs, "factor_info") <- factor_setup$factors

  # Summary message
  n_specs <- nrow(specs)
  n_multiverses <- length(unique(specs$multiverse_id))
  n_factors <- length(wf_vars)

  cat("\n[*] Multiverse Specifications Created\n")
  cat("========================================================\n")
  cat(sprintf("  %d specifications\n", n_specs))
  cat(sprintf("  %d multiverse(s)\n", n_multiverses))
  cat(sprintf("  %d factors included\n", n_factors))
  cat(sprintf("  %d methods x %d dependencies\n",
              length(ma_methods), length(dependencies)))

  if (length(type_N) > 0) {
    cat(sprintf("\n[!] Non-equivalent factors creating separate analyses:\n"))
    for (f in type_N) {
      idx <- which(factor_setup$factors$wf_internal == f)
      label <- factor_setup$factors$label[idx]
      cat(sprintf("   - %s\n", label))
    }
  }

  return(list(
    specifications = specs,
    number_specs = n_specs,
    factor_setup = factor_setup  # Keep original for reference
  ))
}
