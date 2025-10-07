# Development Testing Files

This directory contains testing and development scripts for the **metaMultiverse** package.

---

## Quick Reference

### 🚀 Fastest Test (30 seconds)
```r
source("dev/quick_test.R")
```
Runs essential checks to verify package works after changes.

### 📊 Comprehensive Test (~5 minutes)
```r
rmarkdown::render("dev/test_pipeline.Rmd")
# Or just open in RStudio and click "Knit"
```
Generates full HTML report with 8 test suites covering all features.

### ✅ Official Test Suite
```r
devtools::test()
```
Runs all package tests (required before commits).

---

## Files in This Directory

### `quick_test.R`
**Purpose:** Fast sanity check after making code changes

**What it tests:**
- Package loads
- Basic pipeline works
- Plots generate
- Deprecated functions warn correctly

**Run time:** ~30 seconds

**When to use:**
- After modifying R files
- Before committing changes
- Quick verification during development

**How to run:**
```r
source("dev/quick_test.R")
```

---

### `test_pipeline.Rmd`
**Purpose:** Comprehensive testing with visual output

**What it tests:**
1. ✅ Basic pipeline (minimal example)
2. ✅ Deprecated functions (should warn)
3. ✅ Full pipeline with multiple factors
4. ✅ Visualization functions (static + interactive)
5. ✅ Custom factor groupings
6. ✅ Edge cases and error handling
7. ✅ Different MA methods
8. ✅ Principled multiverse (Type E/U/N)

**Run time:** ~5 minutes

**Output:** Beautiful HTML report with results, plots, and diagnostics

**When to use:**
- Before releasing new version
- After major refactoring
- To demo package features
- For documentation screenshots

**How to run:**
```r
# Option 1: In RStudio
# Open test_pipeline.Rmd and click "Knit"

# Option 2: Command line
rmarkdown::render("dev/test_pipeline.Rmd")

# Option 3: From R
knitr::knit("dev/test_pipeline.Rmd")
browseURL("dev/test_pipeline.html")
```

---

### Other Files (Historical)

- `metaMultiverseDevelopment.Rmd` - Earlier development document
- `prinicpled_multiverse.qmd` - Principled multiverse exploration
- `plots/` - Development plots

---

## Typical Development Workflow

### 1. Making Changes
```r
# Edit R files in R/ directory
# ...make your changes...
```

### 2. Quick Check
```r
# Load changes
devtools::load_all()

# Quick test
source("dev/quick_test.R")
```

### 3. Comprehensive Check
```r
# Full test suite
devtools::test()

# Visual verification
rmarkdown::render("dev/test_pipeline.Rmd")
```

### 4. Commit
```r
# If all tests pass:
# git add .
# git commit -m "Your message"
```

---

## Test Results Interpretation

### quick_test.R Output

```
=== QUICK TEST SUITE ===

✓ Package loaded successfully
✓ Available MA methods: 11
✓ Example data loaded: 289 rows

--- Running basic pipeline ---
Data validation passed. Dataset is ready for multiverse analysis.
✓ Pipeline completed
  Attempted: 4
  Successful: 4
  Success rate: 100 %

✓ Effect sizes:
  Range: [0.487, 0.545]
  Mean: 0.516

--- Testing plots ---
✓ Spec curve plot created
✓ VoE plot created

--- Testing deprecated functions ---
✓ Deprecation warning issued

=== SUMMARY ===
✅ All quick tests passed!
```

**Interpretation:** All systems go! ✅

---

### test_pipeline.Rmd Output

The rendered HTML contains:
- 📊 8 comprehensive test suites
- 📈 Multiple plots and visualizations
- 📝 Detailed results tables
- ✅ Pass/fail indicators for each test
- 💡 Session info for reproducibility

**Interpretation:** Review HTML for any ❌ or ⚠️ symbols

---

## Troubleshooting

### "Package not found" error
```r
# Make sure you're in the package root:
setwd("/Users/cyp/Documents/Work/1_Projects/multiverse-package/metaMultiverse")

# Then load:
devtools::load_all()
```

### Tests fail after changes
```r
# Check what broke:
devtools::test()

# Common issues:
# 1. Function signature changed → update tests
# 2. Return value changed → update expectations
# 3. New dependency → add to DESCRIPTION
```

### .Rmd won't knit
```r
# Check for missing packages:
rmarkdown::pandoc_version()  # Should be > 2.0

# Install if needed:
install.packages(c("rmarkdown", "knitr"))
```

---

## Adding New Tests

### To quick_test.R
Add new test blocks following this pattern:
```r
cat("\n--- Testing new feature ---\n")
tryCatch({
  # Your test code here
  cat("✓ Test passed\n")
}, error = function(e) {
  cat("✗ Error:", e$message, "\n")
})
```

### To test_pipeline.Rmd
Add new section:
````markdown
# Test 9: My New Feature

Description of what you're testing.

```{r test-new-feature}
cat("=== MY NEW FEATURE TEST ===\n\n")

# Test code here

cat("✅ Test complete\n")
```
````

---

## Best Practices

1. ✅ **Run quick_test.R after every significant change**
2. ✅ **Run full test suite before commits**
3. ✅ **Knit test_pipeline.Rmd before releases**
4. ✅ **Keep tests up-to-date with API changes**
5. ✅ **Document expected behavior in tests**

---

## Questions?

See `PHASE1_SUMMARY.md` in the package root for:
- Verification steps
- Commit message templates
- Next steps in development

Happy testing, Doctor Yves! 🐕🧪
