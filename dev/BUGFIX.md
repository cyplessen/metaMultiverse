# Bug Fix: k_smallest_ma Parameter

**Date:** 2025-10-07
**Issue:** Test files incorrectly used `k_smallest_ma` parameter in `create_multiverse_specifications()`

---

## Problem

The test files had:
```r
specs <- factor_setup %>%
  create_multiverse_specifications(
    ma_methods = c("fe", "reml"),
    dependencies = "aggregate",
    k_smallest_ma = 5  # ❌ This parameter doesn't exist here!
  )
```

This caused error:
```
Error in create_multiverse_specifications(...) :
  unused argument (k_smallest_ma = 5)
```

---

## Root Cause

`k_smallest_ma` is a parameter for `general_multiverse()`, not `create_multiverse_specifications()`.

**Where it actually lives:**
- Used internally by `general_multiverse()`
- Can be set globally via: `options(metaMultiverse.k_smallest_ma = 5)`
- Default value: 5
- Controls minimum number of studies required for a valid meta-analysis

---

## Fix Applied

**Files fixed:**
1. ✅ `dev/test_pipeline.Rmd` - Removed 5 instances
2. ✅ `dev/quick_test.R` - Removed 1 instance
3. ✅ `HOW_TO_TEST.md` - Added explanation in FAQ

**Correct usage:**
```r
# Option 1: Use default (k_smallest_ma = 5)
specs <- factor_setup %>%
  create_multiverse_specifications(
    ma_methods = c("fe", "reml"),
    dependencies = "aggregate"
  ) %>%
  run_multiverse_analysis()

# Option 2: Change globally
options(metaMultiverse.k_smallest_ma = 10)
specs <- factor_setup %>%
  create_multiverse_specifications(
    ma_methods = c("fe", "reml"),
    dependencies = "aggregate"
  ) %>%
  run_multiverse_analysis()  # Will use k_smallest_ma = 10
```

---

## Testing

Now you can run:
```r
source("dev/quick_test.R")
```

Should complete without errors! ✅

---

## Lesson Learned

When creating test examples, always check the actual function signatures rather than assuming parameters exist. The `k_smallest_ma` parameter is internal to `general_multiverse()` and is accessed via options, not passed directly through the pipeline.

---

✅ **FIXED**
