## tests/testthat/test-estimators.R
## --------------------------------

# Skip slow tests if requested
skip_if(getOption("metaMultiverse.skip_slow", FALSE))

# Helper function to create valid test data
create_estimator_test_data <- function() {
  data.frame(
    study = paste0("Study_", 1:10),
    es_id = 1:10,
    yi = c(0.3, 0.5, 0.7, -0.2, 0.6, 0.4, 0.8, 0.1, -0.3, 0.9),
    vi = c(0.02, 0.03, 0.01, 0.04, 0.02, 0.03, 0.01, 0.05, 0.02, 0.01),
    stringsAsFactors = FALSE
  )
}

# Helper function to safely get estimator functions
get_estimator_if_exists <- function(name) {
  if (exists(name, where = getNamespace("metaMultiverse"))) {
    return(get(name, envir = getNamespace("metaMultiverse")))
  }
  return(NULL)
}

# Define all estimator functions organized by category
est_funs <- list()

# Standard methods
est_funs$fit_fe <- get_estimator_if_exists("fit_fe")
est_funs$fit_reml <- get_estimator_if_exists("fit_reml")
est_funs$fit_pm <- get_estimator_if_exists("fit_pm")
est_funs$fit_hk_sj <- get_estimator_if_exists("fit_hk_sj")

# Bias methods
est_funs$fit_pet.peese <- get_estimator_if_exists("fit_pet.peese")
est_funs$fit_pet.peese_corrected <- get_estimator_if_exists("fit_pet.peese_corrected")
est_funs$fit_puni_star <- get_estimator_if_exists("fit_puni_star")
est_funs$fit_uwls <- get_estimator_if_exists("fit_uwls")
est_funs$fit_waap <- get_estimator_if_exists("fit_waap")

# Dependency modeling methods
est_funs$fit_three_level <- get_estimator_if_exists("fit_three_level")
est_funs$fit_rve <- get_estimator_if_exists("fit_rve")

# Optional slow methods
est_funs$fit_bayesmeta <- get_estimator_if_exists("fit_bayesmeta")

# Remove NULL entries (functions that don't exist)
est_funs <- est_funs[!sapply(est_funs, is.null)]

# Core required fields
core_fields <- c("b", "ci.lb", "ci.ub", "pval")

# =============================================================================
# BASIC FUNCTIONALITY TESTS
# =============================================================================

test_that("all estimators return universe_result objects with core fields", {
  test_data <- create_estimator_test_data()

  for (fn_name in names(est_funs)) {
    res <- est_funs[[fn_name]](test_data)

    # Test object class (expect_s3_class doesn't take label argument)
    expect_s3_class(res, "universe_result")

    # Test core fields present
    expect_true(all(core_fields %in% names(res)),
                info = paste("Function:", fn_name, "missing core fields"))

    # Test field types and lengths
    expect_true(is.numeric(res$b) && length(res$b) == 1,
                info = paste(fn_name, "$b should be numeric scalar"))
    expect_true(is.numeric(res$ci.lb) && length(res$ci.lb) == 1,
                info = paste(fn_name, "$ci.lb should be numeric scalar"))
    expect_true(is.numeric(res$ci.ub) && length(res$ci.ub) == 1,
                info = paste(fn_name, "$ci.ub should be numeric scalar"))
    expect_true(is.numeric(res$pval) && length(res$pval) == 1,
                info = paste(fn_name, "$pval should be numeric scalar"))

    # Test method attribute exists
    expect_true(!is.null(attr(res, "method")),
                info = paste(fn_name, "should have method attribute"))
  }
})

test_that("estimators handle reasonable effect sizes correctly", {
  test_data <- create_estimator_test_data()

  # Exclude methods that might fail with small sample or have strict requirements
  stable_methods <- c("fit_fe", "fit_reml", "fit_uwls", "fit_pm")

  for (fn_name in stable_methods) {
    if (fn_name %in% names(est_funs)) {
      res <- est_funs[[fn_name]](test_data)

      # Should return finite values for reasonable data
      expect_true(is.finite(res$b),
                  info = paste(fn_name, "should return finite estimate"))
      expect_true(is.finite(res$ci.lb) && is.finite(res$ci.ub),
                  info = paste(fn_name, "should return finite confidence intervals"))

      # CI should be ordered correctly
      expect_true(res$ci.lb <= res$ci.ub,
                  info = paste(fn_name, "CI bounds should be ordered correctly"))

      # P-value should be between 0 and 1 (or NA for Bayesian methods)
      if (!is.na(res$pval)) {
        expect_true(res$pval >= 0 && res$pval <= 1,
                    info = paste(fn_name, "p-value should be between 0 and 1"))
      }
    }
  }
})

# =============================================================================
# EDGE CASES AND ERROR HANDLING
# =============================================================================

test_that("estimators handle empty data gracefully", {
  empty_data <- data.frame(
    study = character(0),
    es_id = numeric(0),
    yi = numeric(0),
    vi = numeric(0)
  )

  for (fn_name in names(est_funs)) {
    # Should not crash, should return universe_NA
    expect_no_error(
      res <- est_funs[[fn_name]](empty_data)
    )

    res <- est_funs[[fn_name]](empty_data)
    expect_s3_class(res, "universe_result")
    # Most should return NA values for empty data
    expect_true(is.na(res$b) || is.finite(res$b),
                info = paste(fn_name, "should handle empty data appropriately"))
  }
})

test_that("estimators handle single observation", {
  single_data <- data.frame(
    study = "Study_1",
    es_id = 1,
    yi = 0.5,
    vi = 0.02
  )

  # Some methods can handle single observations, others cannot
  for (fn_name in names(est_funs)) {
    expect_no_error(
      res <- est_funs[[fn_name]](single_data)
    )

    res <- est_funs[[fn_name]](single_data)
    expect_s3_class(res, "universe_result")
  }
})

test_that("estimators handle identical values", {
  identical_data <- data.frame(
    study = paste0("Study_", 1:5),
    es_id = 1:5,
    yi = rep(0.5, 5),
    vi = rep(0.02, 5)
  )

  for (fn_name in names(est_funs)) {
    expect_no_error(
      res <- est_funs[[fn_name]](identical_data)
    )

    res <- est_funs[[fn_name]](identical_data)
    expect_s3_class(res, "universe_result")
  }
})

test_that("estimators handle extreme values", {
  extreme_data <- data.frame(
    study = paste0("Study_", 1:6),
    es_id = 1:6,
    yi = c(10, -8, 0.1, 0.2, 15, -12),  # Very large effect sizes
    vi = c(0.001, 0.002, 0.1, 0.2, 0.001, 0.002)  # Mix of precisions
  )

  for (fn_name in names(est_funs)) {
    expect_no_error(
      res <- est_funs[[fn_name]](extreme_data)
    )

    res <- est_funs[[fn_name]](extreme_data)
    expect_s3_class(res, "universe_result")
  }
})

# =============================================================================
# METHOD-SPECIFIC TESTS
# =============================================================================

test_that("PET-PEESE returns appropriate method labels", {
  test_data <- create_estimator_test_data()
  res <- metaMultiverse::fit_pet.peese(test_data)

  method_attr <- attr(res, "method")
  expect_true(method_attr %in% c("PET", "PEESE", "PET (failed)", "PEESE (failed)"),
              info = "PET-PEESE should return appropriate method label")
})

test_that("PET-PEESE corrected handles negative estimates", {
  # Create data likely to produce negative PET-PEESE estimate
  negative_data <- data.frame(
    study = paste0("Study_", 1:8),
    es_id = 1:8,
    yi = c(-0.1, -0.2, 0.1, 0.05, -0.05, 0.02, -0.15, 0.03),
    vi = c(0.01, 0.02, 0.08, 0.09, 0.07, 0.10, 0.01, 0.12)  # Small studies negative
  )

  res_regular <- metaMultiverse::fit_pet.peese(negative_data)
  res_corrected <- metaMultiverse::fit_pet.peese_corrected(negative_data)

  expect_s3_class(res_regular, "universe_result")
  expect_s3_class(res_corrected, "universe_result")

  # Corrected version should not be negative
  if (!is.na(res_corrected$b)) {
    expect_true(res_corrected$b >= 0,
                info = "Corrected PET-PEESE should not return negative estimates")
  }
})

test_that("p-uniform* uses appropriate direction", {
  # Test with positive effects (should use right-sided)
  positive_data <- data.frame(
    study = paste0("Study_", 1:6),
    es_id = 1:6,
    yi = c(0.3, 0.5, 0.7, 0.4, 0.6, 0.8),
    vi = c(0.02, 0.03, 0.01, 0.04, 0.02, 0.01)
  )

  res_pos <- metaMultiverse::fit_puni_star(positive_data)
  expect_s3_class(res_pos, "universe_result")
  method_attr <- attr(res_pos, "method")
  if (!grepl("failed", method_attr)) {
    expect_true(grepl("right", method_attr),
                info = "Positive effects should use right-sided testing")
  }

  # Test with negative effects (should use left-sided)
  negative_data <- data.frame(
    study = paste0("Study_", 1:6),
    es_id = 1:6,
    yi = c(-0.3, -0.5, -0.7, -0.4, -0.6, -0.2),
    vi = c(0.02, 0.03, 0.01, 0.04, 0.02, 0.01)
  )

  res_neg <- metaMultiverse::fit_puni_star(negative_data)
  expect_s3_class(res_neg, "universe_result")
  method_attr <- attr(res_neg, "method")
  if (!grepl("failed", method_attr)) {
    expect_true(grepl("left", method_attr),
                info = "Negative effects should use left-sided testing")
  }
})

test_that("WAAP reports powered study information", {
  test_data <- create_estimator_test_data()
  res <- metaMultiverse::fit_waap(test_data)

  expect_s3_class(res, "universe_result")
  method_attr <- attr(res, "method")

  # Should report powered study count or failure reason
  expect_true(grepl("WAAP", method_attr),
              info = "WAAP should have method attribute containing 'WAAP'")

  if (!grepl("failed", method_attr)) {
    expect_true(grepl("\\d+/\\d+", method_attr),
                info = "WAAP should report powered/total study counts")
  }
})

test_that("Bayesian methods handle p-values correctly", {
  test_data <- create_estimator_test_data()
  res <- metaMultiverse::fit_bayesmeta(test_data)

  expect_s3_class(res, "universe_result")
  # Bayesian methods should return NA for p-value
  expect_true(is.na(res$pval),
              info = "Bayesian methods should return NA for p-value")
})

# =============================================================================
# DEPENDENCY MODELING TESTS
# =============================================================================

test_that("dependency modeling methods work with multiple studies", {
  # Create data with multiple effect sizes per study
  dependency_data <- data.frame(
    study = c("Study_A", "Study_A", "Study_B", "Study_B", "Study_C"),
    es_id = 1:5,
    yi = c(0.3, 0.5, 0.7, 0.2, 0.6),
    vi = c(0.02, 0.03, 0.01, 0.04, 0.02)
  )

  dependency_methods <- c("fit_three_level", "fit_rve")

  for (fn_name in dependency_methods) {
    if (fn_name %in% names(est_funs)) {
      res <- est_funs[[fn_name]](dependency_data)
      expect_s3_class(res, "universe_result")

      # Should return finite estimates for reasonable data
      if (!is.na(res$b)) {
        expect_true(is.finite(res$b),
                    info = paste(fn_name, "should return finite estimate"))
      }
    }
  }
})

# =============================================================================
# INTEGRATION WITH REGISTRY TESTS
# =============================================================================

test_that("all registered methods have corresponding functions", {
  skip_if_not(exists("list_ma_methods"))

  registered_methods <- list_ma_methods()
  cat("Registered methods found:", paste(registered_methods, collapse = ", "), "\n")

  # Define the expected function mapping organized by category
  function_map <- list(
    # Standard methods
    "fe" = "fit_fe",
    "reml" = "fit_reml",
    "pm" = "fit_pm",
    "hk-sj" = "fit_hk_sj",

    # Bias methods
    "pet-peese" = "fit_pet.peese",
    "pet-peese-corrected" = "fit_pet.peese_corrected",
    "p-uniform" = "fit_puni_star",
    "uwls" = "fit_uwls",
    "waap" = "fit_waap",

    # Dependency modeling methods
    "three-level" = "fit_three_level",
    "rve" = "fit_rve",

    # Optional slow methods
    "bayesmeta" = "fit_bayesmeta"
  )

  for (method in registered_methods) {
    expected_function <- function_map[[method]]
    if (!is.null(expected_function)) {
      function_exists <- exists(expected_function, where = getNamespace("metaMultiverse"))
      if (!function_exists) {
        cat("MISSING: Method", method, "expects function", expected_function, "but it doesn't exist\n")
      }
      expect_true(function_exists,
                  label = paste("Method", method, "should have function", expected_function))
    } else {
      # For debugging: show which methods are registered but not mapped
      cat("UNMAPPED: Registered method '", method, "' not found in test function_map\n")
      # Don't fail the test, just warn
      expect_true(TRUE, label = paste("Method", method, "not in function_map (skipped)"))
    }
  }
})

test_that("method registry consistency", {
  skip_if_not(exists(".ma_method_registry"))
  skip_if_not(exists("list_ma_methods"))

  test_data <- create_estimator_test_data()

  # Test only methods that should definitely exist
  expected_basic_methods <- c("fe", "reml", "uwls", "pm")
  registered_methods <- list_ma_methods()

  # Only test methods that are both expected and registered
  methods_to_test <- intersect(expected_basic_methods, registered_methods)

  for (method in methods_to_test) {
    registry_entry <- .ma_method_registry[[method]]
    expect_true(!is.null(registry_entry),
                label = paste("Method", method, "should have registry entry"))

    if (!is.null(registry_entry)) {
      expect_true(is.function(registry_entry$fun),
                  label = paste("Method", method, "should have function in registry"))

      # Test that the function works (but don't require specific results for edge cases)
      expect_no_error(
        result <- registry_entry$fun(test_data)
      )

      # Test that result is a universe_result
      result <- registry_entry$fun(test_data)
      expect_s3_class(result, "universe_result")
    }
  }
})

# =============================================================================
# PERFORMANCE AND ROBUSTNESS TESTS
# =============================================================================

test_that("estimators complete within reasonable time", {
  test_data <- create_estimator_test_data()

  # Test subset of methods for performance (exclude potentially slow ones)
  fast_methods <- c("fit_fe", "fit_reml", "fit_uwls", "fit_pm")

  for (fn_name in fast_methods) {
    start_time <- Sys.time()
    res <- est_funs[[fn_name]](test_data)
    end_time <- Sys.time()

    duration <- as.numeric(difftime(end_time, start_time, units = "secs"))

    # Should complete within 2 seconds for basic methods
    expect_true(duration < 2,
                info = paste(fn_name, "should complete quickly, took", round(duration, 3), "seconds"))
  }
})

test_that("estimators are reproducible", {
  test_data <- create_estimator_test_data()

  # Test a few methods for reproducibility
  reproducible_methods <- c("fit_fe", "fit_reml", "fit_uwls")

  for (fn_name in reproducible_methods) {
    res1 <- est_funs[[fn_name]](test_data)
    res2 <- est_funs[[fn_name]](test_data)

    expect_equal(res1$b, res2$b, tolerance = 1e-10,
                 info = paste(fn_name, "should be reproducible"))
    expect_equal(res1$ci.lb, res2$ci.lb, tolerance = 1e-10,
                 info = paste(fn_name, "CI should be reproducible"))
    expect_equal(res1$ci.ub, res2$ci.ub, tolerance = 1e-10,
                 info = paste(fn_name, "CI should be reproducible"))
  }
})
