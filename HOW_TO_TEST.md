# How to Test Your Changes - Quick Guide for Doctor Yves

**TL;DR:** Three ways to test, from fastest to most thorough.

---

## 🚀 Option 1: Super Quick Test (30 seconds)

**When:** After any code changes, before committing

**How:**
```r
source("dev/quick_test.R")
```

**What you'll see:**
```
=== QUICK TEST SUITE ===
✓ Package loaded successfully
✓ Available MA methods: 11
✓ Example data loaded: 289 rows
✓ Pipeline completed
✓ Spec curve plot created
✓ VoE plot created
✓ Deprecation warning issued
✅ All quick tests passed!
```

**Interpretation:**
- All ✓ = Good to go! ✅
- Any ✗ = Something broke, needs fixing ❌

---

## 📊 Option 2: Comprehensive Test (~5 minutes)

**When:** Before pushing to GitHub, after major changes

**How (in RStudio):**
1. Open `dev/test_pipeline.Rmd`
2. Click "Knit" button
3. Wait for HTML report to generate
4. Review the output

**How (in R console):**
```r
rmarkdown::render("dev/test_pipeline.Rmd")
browseURL("dev/test_pipeline.html")
```

**What you'll see:**
- Beautiful HTML report with 8 test suites
- Plots and visualizations
- Detailed results tables
- Pass/fail indicators

**Interpretation:**
- Look for ✅ symbols = tests passed
- Look for ❌ or ⚠️ symbols = needs attention

---

## ✅ Option 3: Official Test Suite

**When:** Before committing, required for pull requests

**How:**
```r
devtools::test()
```

**What you'll see:**
```
ℹ Testing metaMultiverse
✔ | F W S  OK | Context
✔ |     0    11 | check_data_multiverse [1.2s]
✔ |     0     8 | class
✔ |     0    12 | create_principled_multiverse_specifications
...
══ Results ═══════════════════════════════════════
Duration: 15.3 s

[ FAIL 0 | WARN 0 | SKIP 0 | PASS 94 ]

Nice work!
```

**Interpretation:**
- `FAIL 0` = All tests passed! ✅
- `FAIL >0` = Some tests failed, check output ❌

---

## 🎯 Recommended Workflow

### For Small Changes (editing 1-2 functions):
```r
# 1. Make your changes to R files

# 2. Load the changes
devtools::load_all()

# 3. Quick test
source("dev/quick_test.R")

# 4. If quick test passes, commit!
```

### For Larger Changes (refactoring, new features):
```r
# 1. Make your changes

# 2. Load changes
devtools::load_all()

# 3. Quick test
source("dev/quick_test.R")

# 4. Run test suite
devtools::test()

# 5. Visual verification
rmarkdown::render("dev/test_pipeline.Rmd")

# 6. If everything passes, commit!
```

---

## 🔍 Testing Your Recent Changes

### Test the deprecation warnings work:
```r
source("dev/quick_test.R")
# Look for the "Testing deprecated functions" section
# Should see: "✓ Deprecation warning issued"
```

### Test the fixed assertions work:
```r
devtools::test()
# Look for: "✔ | test-check_data_multiverse"
# Should show all tests passing
```

### Visual verification everything works:
```r
# Open and knit test_pipeline.Rmd
# Review the HTML output
# All sections should show ✅
```

---

## ❓ Common Questions

### Q: Which test should I run first?
**A:** Always start with `source("dev/quick_test.R")` - it's fast and catches most issues.

### Q: Where does `k_smallest_ma` parameter go?
**A:** It's NOT in `create_multiverse_specifications()`. It can be set globally:
```r
# Set globally (default is 5)
options(metaMultiverse.k_smallest_ma = 10)

# Then run analysis normally
results <- specs %>% run_multiverse_analysis()
```
Or it's used internally by `general_multiverse()` which is called by `run_multiverse_analysis()`.

### Q: Do I need to run all three every time?
**A:** No! Quick test for minor changes. Full suite before commits.

### Q: What if quick_test.R passes but devtools::test() fails?
**A:** Check which specific test file is failing:
```r
testthat::test_file("tests/testthat/test-FILENAME.R")
```

### Q: What if I just want to test one specific feature?
**A:** Use the interactive R console:
```r
devtools::load_all()
data("data_digDep")

# Test your specific feature
result <- data_digDep %>%
  check_data_multiverse() %>%
  define_factors(Pop = "wf_3|E") %>%
  # ... etc
```

---

## 🐛 Debugging Failed Tests

### Step 1: Identify what failed
```r
devtools::test()
# Look for ✖ symbols and read the error message
```

### Step 2: Run just that test file
```r
# Example:
testthat::test_file("tests/testthat/test-check_data_multiverse.R")
```

### Step 3: Debug interactively
```r
# Load the package
devtools::load_all()

# Copy the failing test code and run it line by line
# This lets you see exactly where it breaks
```

### Step 4: Fix and re-test
```r
# Edit the R file or test file
# Save changes
devtools::load_all()  # Reload
source("dev/quick_test.R")  # Quick check
```

---

## ✅ Verification Checklist

Before committing your Phase 1 changes:

- [ ] `source("dev/quick_test.R")` passes
- [ ] `devtools::test()` passes with FAIL 0
- [ ] `dev/test_pipeline.Rmd` knits without errors
- [ ] Deprecated functions issue warnings (test in quick_test.R)
- [ ] Check_data_multiverse returns data frame (test suite verifies)
- [ ] All plots generate correctly (test_pipeline.Rmd shows them)

---

## 📝 Files You Created

1. ✅ **dev/quick_test.R** - Your fast sanity check
2. ✅ **dev/test_pipeline.Rmd** - Comprehensive test suite with visualizations
3. ✅ **dev/README.md** - Full documentation of dev/ folder
4. ✅ **HOW_TO_TEST.md** - This file (quick reference)

---

## 🎉 You're All Set!

To test your Phase 1 changes right now:

```r
# In RStudio or R console:
setwd("/Users/cyp/Documents/Work/1_Projects/multiverse-package/metaMultiverse")
source("dev/quick_test.R")
```

Should take ~30 seconds and show all ✓ marks.

Then you're ready to commit! 🚀

Good luck, Yves Dog! 🐕💻
