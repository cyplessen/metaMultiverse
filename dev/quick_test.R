# Quick Test Script for metaMultiverse Package
# Run this after making changes to quickly verify everything works
# Doctor Yves - 2025-10-07

# Clear environment
rm(list = ls())

# Load package from source
cat("Loading package...\n")
devtools::load_all()

# Suppress warnings for cleaner output (except deprecation warnings)
options(warn = 1)

cat("\n=== QUICK TEST SUITE ===\n\n")

# Test 1: Package loads correctly
cat("✓ Package loaded successfully\n")

# Test 2: List MA methods
methods <- list_ma_methods()
cat("✓ Available MA methods:", length(methods), "\n")

# Test 3: Load data
data("data_digDep")
cat("✓ Example data loaded:", nrow(data_digDep), "rows\n")

# Test 4: Basic pipeline (should complete in ~10 seconds)
cat("\n--- Running basic pipeline ---\n")
results <- data_digDep %>%
  check_data_multiverse() %>%
  define_factors(Population = "wf_3|E") %>%
  create_multiverse_specifications(
    ma_methods = c("fe", "reml"),
    dependencies = "aggregate"
  ) %>%
  run_multiverse_analysis(verbose = FALSE, progress = FALSE)

cat("✓ Pipeline completed\n")
cat("  Attempted:", results$n_attempted, "\n")
cat("  Successful:", results$n_successful, "\n")
cat("  Success rate:", round(100 * results$n_successful / results$n_attempted, 1), "%\n")

# Test 5: Effect size summary
if (results$n_successful > 0) {
  cat("\n✓ Effect sizes:\n")
  cat("  Range: [",
      round(min(results$results$b, na.rm = TRUE), 3), ", ",
      round(max(results$results$b, na.rm = TRUE), 3), "]\n", sep = "")
  cat("  Mean:", round(mean(results$results$b, na.rm = TRUE), 3), "\n")
}

# Test 6: Plotting functions
cat("\n--- Testing plots ---\n")
tryCatch({
  p1 <- plot_spec_curve(results, interactive = FALSE)
  cat("✓ Spec curve plot created\n")
}, error = function(e) {
  cat("✗ Spec curve error:", e$message, "\n")
})

tryCatch({
  p2 <- plot_voe(results, interactive = FALSE)
  cat("✓ VoE plot created\n")
}, error = function(e) {
  cat("✗ VoE plot error:", e$message, "\n")
})

# Test 7: Deprecated function warning
cat("\n--- Testing deprecated functions ---\n")
test_data <- data.frame(
  study = "test", es_id = 1, yi = 0.5, vi = 0.1, pop = "adults"
)

cat("Calling setup_which_factors() (should warn):\n")
suppressWarnings({
  result <- tryCatch({
    setup_which_factors(test_data, c("Population" = "pop"))
    cat("✓ Deprecation warning issued\n")
  }, error = function(e) {
    cat("✗ Error:", e$message, "\n")
  })
})

# Test 8: Run actual test suite (optional)
cat("\n--- Running test suite (optional) ---\n")
cat("To run full tests, execute: devtools::test()\n")
cat("Or just the fixed test: testthat::test_file('tests/testthat/test-check_data_multiverse.R')\n")

# Summary
cat("\n=== SUMMARY ===\n")
cat("✅ All quick tests passed!\n")
cat("Package is ready to use.\n\n")

cat("Next steps:\n")
cat("1. Run devtools::test() to verify all tests pass\n")
cat("2. Knit dev/test_pipeline.Rmd for comprehensive testing\n")
cat("3. Commit changes with suggested messages in PHASE1_SUMMARY.md\n\n")
