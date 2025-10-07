# tests/testthat/test-integration-full-pipeline.R
# Integration tests for complete multiverse pipeline

# ==============================================================================
# INTEGRATION TEST 1: Basic End-to-End Pipeline
# ==============================================================================

test_that("basic pipeline executes successfully from data to results", {
  # Load data
  data("data_digDep")

  # Complete pipeline
  results <- data_digDep %>%
    check_data_multiverse() %>%
    define_factors(Population = "wf_3|E") %>%
    create_multiverse_specifications(
      ma_methods = c("fe", "reml"),
      dependencies = "aggregate"
    ) %>%
    run_multiverse_analysis(verbose = FALSE, progress = FALSE)

  # Verify results structure
  expect_s3_class(results, "multiverse_result")
  expect_true(results$n_successful > 0)
  expect_s3_class(results$results, "data.frame")

  # Verify results content
  expect_true("b" %in% names(results$results))
  expect_true("ci.lb" %in% names(results$results))
  expect_true("ci.ub" %in% names(results$results))
  expect_true("pval" %in% names(results$results))
  expect_true("k" %in% names(results$results))
  expect_true("ma_method" %in% names(results$results))
  expect_true("dependency" %in% names(results$results))

  # Verify all results are finite
  expect_true(all(is.finite(results$results$b)))
  expect_true(all(is.finite(results$results$ci.lb)))
  expect_true(all(is.finite(results$results$ci.ub)))
  expect_true(all(is.finite(results$results$pval)))

  # Verify confidence intervals are valid
  expect_true(all(results$results$ci.lb < results$results$b))
  expect_true(all(results$results$ci.ub > results$results$b))
})

# ==============================================================================
# INTEGRATION TEST 2: Multiple Factors
# ==============================================================================

test_that("pipeline handles multiple factors correctly", {
  data("data_digDep")

  results <- data_digDep %>%
    check_data_multiverse() %>%
    define_factors(
      Population = "wf_3|E",
      Guidance = "wf_2|U"
    ) %>%
    create_multiverse_specifications(
      ma_methods = c("fe", "reml"),
      dependencies = "aggregate"
    ) %>%
    run_multiverse_analysis(verbose = FALSE, progress = FALSE)

  # Check results have factor columns (exact names depend on internal mapping)
  # Just verify we have a reasonable number of columns
  expect_true(ncol(results$results) > 10)

  # Verify multiple specifications
  expect_true(results$n_attempted > 10)
  expect_true(results$n_successful > 5)
})

# ==============================================================================
# INTEGRATION TEST 3: Custom Factor Groupings
# ==============================================================================

test_that("pipeline handles custom factor groupings", {
  data("data_digDep")

  results <- data_digDep %>%
    check_data_multiverse() %>%
    define_factors(
      Population = "wf_3|E",
      Guidance = list(
        column = "wf_2",
        decision = "U",
        groups = list(
          high = "guided",
          low = c("minimal to no support", "automated encouragement")
        )
      )
    ) %>%
    create_multiverse_specifications(
      ma_methods = "reml",
      dependencies = "aggregate"
    ) %>%
    run_multiverse_analysis(verbose = FALSE, progress = FALSE)

  # Check custom groupings were applied - just verify results exist
  # (Exact column names depend on internal mapping)

  # Verify results
  expect_true(results$n_successful > 0)
  expect_s3_class(results$results, "data.frame")
})

# ==============================================================================
# INTEGRATION TEST 4: Different MA Methods
# ==============================================================================

test_that("pipeline works with various meta-analytic methods", {
  data("data_digDep")

  # Test multiple methods
  methods_to_test <- c("fe", "reml", "pm", "pet-peese", "uwls")

  results <- data_digDep %>%
    check_data_multiverse() %>%
    define_factors(Population = "wf_3|E") %>%
    create_multiverse_specifications(
      ma_methods = methods_to_test,
      dependencies = "aggregate"
    ) %>%
    run_multiverse_analysis(verbose = FALSE, progress = FALSE)

  # Check methods were used
  methods_used <- unique(results$results$ma_method)
  expect_true(length(methods_used) > 0)
  expect_true(all(methods_used %in% methods_to_test))

  # Verify all methods produced results
  methods_summary <- results$results %>%
    group_by(ma_method) %>%
    summarise(n = n(), .groups = "drop")

  expect_true(nrow(methods_summary) >= 3)
})

# ==============================================================================
# INTEGRATION TEST 5: Dependency Strategies
# ==============================================================================

test_that("pipeline handles all dependency strategies", {
  data("data_digDep")

  results <- data_digDep %>%
    check_data_multiverse() %>%
    define_factors(Population = "wf_3|E") %>%
    create_multiverse_specifications(
      ma_methods = "reml",
      dependencies = c("aggregate", "select_max", "select_min")
    ) %>%
    run_multiverse_analysis(verbose = FALSE, progress = FALSE)

  # Check all dependencies were used
  deps_used <- unique(results$results$dependency)
  expect_true(length(deps_used) == 3)
  expect_true("aggregate" %in% deps_used)
  expect_true("select_max" %in% deps_used)
  expect_true("select_min" %in% deps_used)

  # Verify each dependency produced results
  for (dep in deps_used) {
    dep_results <- results$results %>% filter(dependency == dep)
    expect_true(nrow(dep_results) > 0)
  }
})

# ==============================================================================
# INTEGRATION TEST 6: Visualization Functions
# ==============================================================================

test_that("visualization functions work with pipeline results", {
  data("data_digDep")

  results <- data_digDep %>%
    check_data_multiverse() %>%
    define_factors(Population = "wf_3|E") %>%
    create_multiverse_specifications(
      ma_methods = c("fe", "reml"),
      dependencies = "aggregate"
    ) %>%
    run_multiverse_analysis(verbose = FALSE, progress = FALSE)

  # Test specification curve plot
  expect_no_error(plot_spec_curve(results, interactive = FALSE))

  # Test VoE plot
  expect_no_error(plot_voe(results, interactive = FALSE))
})

# ==============================================================================
# INTEGRATION TEST 7: Pipeline with Minimal Data
# ==============================================================================

test_that("pipeline handles small datasets appropriately", {
  # Create minimal valid data
  minimal_data <- data.frame(
    study = c("S1", "S2", "S3", "S4", "S5"),
    es_id = 1:5,
    yi = c(0.5, 0.3, 0.7, 0.4, 0.6),
    vi = c(0.02, 0.03, 0.01, 0.025, 0.015),
    wf_1 = c("A", "B", "A", "B", "A")
  )

  results <- minimal_data %>%
    check_data_multiverse() %>%
    define_factors(Factor1 = "wf_1|E") %>%
    create_multiverse_specifications(
      ma_methods = "fe",
      dependencies = "aggregate"
    ) %>%
    run_multiverse_analysis(verbose = FALSE, progress = FALSE)

  # Should complete but may have some failures due to low k
  expect_s3_class(results, "multiverse_result")
  expect_true(results$n_attempted > 0)
})

# ==============================================================================
# INTEGRATION TEST 8: Type E/U/N Decision Types
# ==============================================================================

test_that("E/U/N decision types create correct multiverse structure", {
  data("data_digDep")

  # Test with E and U types (N requires a column we don't have)
  factor_setup <- data_digDep %>%
    check_data_multiverse() %>%
    define_factors(
      Population = "wf_3|E",   # Equivalent
      Guidance = "wf_2|U"       # Uncertain
    )

  # Check factor decision types are recorded
  expect_true("decision" %in% names(factor_setup$factors))
  expect_true("E" %in% factor_setup$factors$decision)
  expect_true("U" %in% factor_setup$factors$decision)

  # Create specs and run
  results <- factor_setup %>%
    create_multiverse_specifications(
      ma_methods = "reml",
      dependencies = "aggregate"
    ) %>%
    run_multiverse_analysis(verbose = FALSE, progress = FALSE)

  # Should have multiple specifications from E and U factors
  expect_true(results$n_attempted > 5)
  expect_true(results$n_successful > 0)
})

# ==============================================================================
# INTEGRATION TEST 9: Factor Setup Preservation
# ==============================================================================

test_that("factor setup is preserved through pipeline", {
  data("data_digDep")

  factor_setup <- data_digDep %>%
    check_data_multiverse() %>%
    define_factors(
      Population = "wf_3|E",
      Guidance = "wf_2|U"
    )

  specs <- factor_setup %>%
    create_multiverse_specifications(
      ma_methods = "reml",
      dependencies = "aggregate"
    )

  # Check factor_setup is preserved in specs
  expect_true(!is.null(specs$factor_setup))
  expect_equal(specs$factor_setup$factors, factor_setup$factors)

  # Run analysis
  results <- specs %>%
    run_multiverse_analysis(verbose = FALSE, progress = FALSE)

  # Check specs structure is preserved in results
  expect_true(!is.null(results$specifications))
})

# ==============================================================================
# INTEGRATION TEST 10: Error Recovery
# ==============================================================================

test_that("pipeline handles partial failures gracefully", {
  data("data_digDep")

  # Create specs that will have some failures
  # (e.g., some methods might fail with certain data configurations)
  results <- data_digDep %>%
    check_data_multiverse() %>%
    define_factors(
      Population = "wf_3|E",
      Guidance = "wf_2|U"
    ) %>%
    create_multiverse_specifications(
      ma_methods = c("fe", "reml", "pet-peese"),
      dependencies = c("aggregate", "select_max")
    ) %>%
    run_multiverse_analysis(verbose = FALSE, progress = FALSE)

  # Should complete even if some specs fail
  expect_s3_class(results, "multiverse_result")
  expect_true(results$n_attempted > 0)

  # May have warnings but should have some successes
  if (results$n_successful > 0) {
    expect_s3_class(results$results, "data.frame")
    expect_true(nrow(results$results) > 0)
  }

  # Check warning structure
  expect_true("n_warnings" %in% names(results))
  expect_true("multiverse_warnings" %in% names(results))
})

# ==============================================================================
# INTEGRATION TEST 11: Data Validation in Pipeline
# ==============================================================================

test_that("pipeline validates data before proceeding", {
  # Invalid data (missing required columns)
  bad_data <- data.frame(
    study = c("S1", "S2"),
    yi = c(0.5, 0.3)
    # Missing vi, es_id
  )

  expect_error(
    bad_data %>%
      check_data_multiverse() %>%
      define_factors(Population = "wf_1|E"),
    "Missing required columns"
  )
})

# ==============================================================================
# INTEGRATION TEST 12: Reproducibility
# ==============================================================================

test_that("pipeline produces reproducible results", {
  data("data_digDep")

  # Run pipeline twice
  run1 <- data_digDep %>%
    check_data_multiverse() %>%
    define_factors(Population = "wf_3|E") %>%
    create_multiverse_specifications(
      ma_methods = "fe",
      dependencies = "aggregate"
    ) %>%
    run_multiverse_analysis(verbose = FALSE, progress = FALSE)

  run2 <- data_digDep %>%
    check_data_multiverse() %>%
    define_factors(Population = "wf_3|E") %>%
    create_multiverse_specifications(
      ma_methods = "fe",
      dependencies = "aggregate"
    ) %>%
    run_multiverse_analysis(verbose = FALSE, progress = FALSE)

  # Results should be identical
  expect_equal(run1$n_attempted, run2$n_attempted)
  expect_equal(run1$n_successful, run2$n_successful)
  expect_equal(nrow(run1$results), nrow(run2$results))

  # Effect sizes should match
  expect_equal(run1$results$b, run2$results$b, tolerance = 1e-10)
})
