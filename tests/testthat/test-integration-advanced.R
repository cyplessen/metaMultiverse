# tests/testthat/test-integration-advanced.R
# Advanced integration tests for edge cases and special features

# ==============================================================================
# ADVANCED TEST 1: Complex Custom Groupings
# ==============================================================================

test_that("complex custom groupings work end-to-end", {
  data("data_digDep")

  results <- data_digDep %>%
    check_data_multiverse() %>%
    define_factors(
      # Multiple custom groups with ACTUAL levels from data
      Guidance = list(
        column = "wf_2",
        decision = "U",
        groups = list(
          high_support = "guided",
          medium_support = c("guided", "automated encouragement"),
          low_support = c("minimal to no support", "human encouragement"),
          all_support = c("guided", "automated encouragement", "minimal to no support", "human encouragement")
        )
      ),
      Population = "wf_3|E"
    ) %>%
    create_multiverse_specifications(
      ma_methods = "reml",
      dependencies = "aggregate"
    ) %>%
    run_multiverse_analysis(verbose = FALSE, progress = FALSE)

  # Verify results were generated successfully
  expect_true(results$n_successful > 0)
  expect_s3_class(results$results, "data.frame")

  # Check that custom groups were used (should have factor column in results)
  # The exact column name depends on internal mapping, so just check structure
  expect_true(ncol(results$results) > 10)  # Should have many columns including factors
})

# ==============================================================================
# ADVANCED TEST 2: All Meta-Analytic Methods
# ==============================================================================

test_that("all available methods can be run in pipeline", {
  skip_on_cran()  # Skip on CRAN due to long runtime

  data("data_digDep")

  # Get all available methods
  all_methods <- list_ma_methods()

  # Remove bayesmeta if slow
  methods_to_test <- setdiff(all_methods, "bayesmeta")

  results <- data_digDep %>%
    check_data_multiverse() %>%
    define_factors(Population = "wf_3|E") %>%
    create_multiverse_specifications(
      ma_methods = methods_to_test,
      dependencies = "aggregate"
    ) %>%
    run_multiverse_analysis(verbose = FALSE, progress = FALSE)

  # Most methods should produce at least some results
  methods_used <- unique(results$results$ma_method)
  expect_true(length(methods_used) >= 5)

  # Check each method produced valid results
  for (method in methods_used) {
    method_results <- results$results %>%
      filter(ma_method == method)

    expect_true(nrow(method_results) > 0)
    expect_true(all(is.finite(method_results$b)))
  }
})

# ==============================================================================
# ADVANCED TEST 3: Three-Level and RVE Methods
# ==============================================================================

test_that("dependency modeling methods (3-level, RVE) work with modeled dependency", {
  data("data_digDep")

  # These methods only work with "modeled" dependency
  results <- data_digDep %>%
    check_data_multiverse() %>%
    define_factors(Population = "wf_3|E") %>%
    create_multiverse_specifications(
      ma_methods = c("three-level", "rve"),
      dependencies = "modeled"
    ) %>%
    run_multiverse_analysis(verbose = FALSE, progress = FALSE)

  # Should have attempted the analyses
  expect_true(results$n_attempted > 0)

  # If successful, verify results structure
  if (results$n_successful > 0) {
    expect_s3_class(results$results, "data.frame")
    expect_true(all(results$results$dependency == "modeled"))
  }
})

# ==============================================================================
# ADVANCED TEST 4: Multiple Multiverses from Type N
# ==============================================================================

test_that("multiple factors create complex multiverse structure", {
  data("data_digDep")

  results <- data_digDep %>%
    check_data_multiverse() %>%
    define_factors(
      Population = "wf_3|E",    # Multiple populations
      Guidance = "wf_2|U"        # Different guidance levels
    ) %>%
    create_multiverse_specifications(
      ma_methods = "reml",
      dependencies = "aggregate"
    ) %>%
    run_multiverse_analysis(verbose = FALSE, progress = FALSE)

  # Should create multiple specifications
  expect_true(results$n_attempted >= 10)

  # Each specification should have results
  expect_true(results$n_successful > 0)
})

# ==============================================================================
# ADVANCED TEST 5: Minimum k Threshold
# ==============================================================================

test_that("k_smallest_ma option works correctly", {
  data("data_digDep")

  # Set high threshold
  options(metaMultiverse.k_smallest_ma = 50)

  results_strict <- data_digDep %>%
    check_data_multiverse() %>%
    define_factors(Population = "wf_3|E") %>%
    create_multiverse_specifications(
      ma_methods = "reml",
      dependencies = "aggregate"
    ) %>%
    run_multiverse_analysis(verbose = FALSE, progress = FALSE)

  # Set low threshold
  options(metaMultiverse.k_smallest_ma = 3)

  results_permissive <- data_digDep %>%
    check_data_multiverse() %>%
    define_factors(Population = "wf_3|E") %>%
    create_multiverse_specifications(
      ma_methods = "reml",
      dependencies = "aggregate"
    ) %>%
    run_multiverse_analysis(verbose = FALSE, progress = FALSE)

  # Permissive should have more successful analyses
  expect_true(results_permissive$n_successful >= results_strict$n_successful)

  # Reset to default
  options(metaMultiverse.k_smallest_ma = 5)
})

# ==============================================================================
# ADVANCED TEST 6: Factor Groups Preserved
# ==============================================================================

test_that("factor groups are preserved and accessible throughout pipeline", {
  data("data_digDep")

  factor_setup <- data_digDep %>%
    check_data_multiverse() %>%
    define_factors(
      Guidance = list(
        column = "wf_2",
        decision = "U",
        groups = list(
          high = "guided",
          low = c("minimal to no support", "automated encouragement")
        )
      )
    )

  # Check factor_groups exist and contain custom groups
  expect_true(!is.null(factor_setup$factor_groups))
  expect_true(length(factor_setup$factor_groups) > 0)

  # Check custom group names are present
  first_factor_groups <- factor_setup$factor_groups[[1]]
  expect_true("high" %in% names(first_factor_groups))
  expect_true("low" %in% names(first_factor_groups))

  # Create specs
  specs <- factor_setup %>%
    create_multiverse_specifications(
      ma_methods = "reml",
      dependencies = "aggregate"
    )

  # Factor groups should be preserved
  expect_true(!is.null(specs$factor_setup$factor_groups))

  # Run analysis
  results <- specs %>%
    run_multiverse_analysis(verbose = FALSE, progress = FALSE)

  # Should complete successfully
  expect_true(results$n_successful > 0)
})

# ==============================================================================
# ADVANCED TEST 7: Empty Factor Levels
# ==============================================================================

test_that("pipeline handles factors with levels that produce zero studies", {
  data("data_digDep")

  # Some factor levels might result in 0 studies after filtering
  results <- data_digDep %>%
    check_data_multiverse() %>%
    define_factors(
      Population = "wf_3|E",
      Guidance = "wf_2|U",
      Quality = "wf_7|U"
    ) %>%
    create_multiverse_specifications(
      ma_methods = "reml",
      dependencies = "aggregate"
    ) %>%
    run_multiverse_analysis(verbose = FALSE, progress = FALSE)

  # Should complete (some specs may fail due to insufficient k)
  expect_s3_class(results, "multiverse_result")
  expect_true(results$n_attempted > 0)

  # If there are warnings, they should be documented
  if (results$n_warnings > 0) {
    expect_true(length(results$multiverse_warnings) > 0)
  }
})

# ==============================================================================
# ADVANCED TEST 8: Publication Bias Methods
# ==============================================================================

test_that("publication bias correction methods work in pipeline", {
  data("data_digDep")

  bias_methods <- c("pet-peese", "pet-peese-corrected", "p-uniform", "uwls", "waap")

  results <- data_digDep %>%
    check_data_multiverse() %>%
    define_factors(Population = "wf_3|E") %>%
    create_multiverse_specifications(
      ma_methods = bias_methods,
      dependencies = c("aggregate", "select_max")  # p-uniform needs select
    ) %>%
    run_multiverse_analysis(verbose = FALSE, progress = FALSE)

  # Should have attempted multiple methods
  expect_true(results$n_attempted > 0)

  # Check which methods produced results
  if (results$n_successful > 0) {
    methods_used <- unique(results$results$ma_method)
    expect_true(length(methods_used) > 0)

    # At least some bias correction methods should work
    expect_true(any(methods_used %in% bias_methods))
  }
})

# ==============================================================================
# ADVANCED TEST 9: Results Filtering and Subsetting
# ==============================================================================

test_that("results can be filtered and analyzed post-hoc", {
  data("data_digDep")

  results <- data_digDep %>%
    check_data_multiverse() %>%
    define_factors(
      Population = "wf_3|E",
      Guidance = "wf_2|U"
    ) %>%
    create_multiverse_specifications(
      ma_methods = c("fe", "reml"),
      dependencies = c("aggregate", "select_max")
    ) %>%
    run_multiverse_analysis(verbose = FALSE, progress = FALSE)

  # Filter by method
  reml_only <- results$results %>% filter(ma_method == "reml")
  expect_true(nrow(reml_only) > 0)
  expect_true(all(reml_only$ma_method == "reml"))

  # Filter by k
  well_powered <- results$results %>% filter(k >= 20)
  if (nrow(well_powered) > 0) {
    expect_true(all(well_powered$k >= 20))
  }

  # Filter by significance
  significant <- results$results %>% filter(pval < 0.05)
  if (nrow(significant) > 0) {
    expect_true(all(significant$pval < 0.05))
  }
})

# ==============================================================================
# ADVANCED TEST 10: Consistency Across Different Specifications
# ==============================================================================

test_that("similar specifications produce consistent results", {
  data("data_digDep")

  # Run two similar analyses
  results1 <- data_digDep %>%
    check_data_multiverse() %>%
    define_factors(Population = "wf_3|E") %>%
    create_multiverse_specifications(
      ma_methods = "reml",
      dependencies = "aggregate"
    ) %>%
    run_multiverse_analysis(verbose = FALSE, progress = FALSE)

  results2 <- data_digDep %>%
    check_data_multiverse() %>%
    define_factors(Population = "wf_3|E") %>%
    create_multiverse_specifications(
      ma_methods = "reml",
      dependencies = "aggregate"
    ) %>%
    run_multiverse_analysis(verbose = FALSE, progress = FALSE)

  # Should produce same number of specs
  expect_equal(results1$n_attempted, results2$n_attempted)
  expect_equal(results1$n_successful, results2$n_successful)

  # Effect sizes should be very similar (allowing for numeric precision)
  expect_equal(results1$results$b, results2$results$b, tolerance = 1e-10)
})

# ==============================================================================
# ADVANCED TEST 11: Pipeline with All Factor Types Together
# ==============================================================================

test_that("E and U factors work correctly together in complex design", {
  data("data_digDep")

  results <- data_digDep %>%
    check_data_multiverse() %>%
    define_factors(
      # Type E
      Population = "wf_3|E",
      Technology = "wf_1|E",

      # Type U
      Guidance = "wf_2|U"
    ) %>%
    create_multiverse_specifications(
      ma_methods = c("fe", "reml"),
      dependencies = "aggregate"
    ) %>%
    run_multiverse_analysis(verbose = FALSE, progress = FALSE)

  # Should have many specifications
  expect_true(results$n_attempted > 20)

  # Should have successful results
  expect_true(results$n_successful > 0)

  # Results should have finite effect sizes
  expect_true(all(is.finite(results$results$b)))
})

# ==============================================================================
# ADVANCED TEST 12: Plotting with Complex Results
# ==============================================================================

test_that("plotting functions handle complex multiverse results", {
  data("data_digDep")

  results <- data_digDep %>%
    check_data_multiverse() %>%
    define_factors(
      Population = "wf_3|E",
      Guidance = "wf_2|U"
    ) %>%
    create_multiverse_specifications(
      ma_methods = c("fe", "reml", "pm"),
      dependencies = c("aggregate", "select_max")
    ) %>%
    run_multiverse_analysis(verbose = FALSE, progress = FALSE)

  # Both plotting functions should work
  expect_no_error(plot_spec_curve(results, interactive = FALSE))
  expect_no_error(plot_voe(results, interactive = FALSE))

  # Interactive versions should also work
  expect_no_error(plot_spec_curve(results, interactive = TRUE))
  expect_no_error(plot_voe(results, interactive = TRUE))
})
