pet_toy <- tibble::tibble(
  yi  = c(0.40, 0.55, 0.30, 0.10, 0.60),
  vi  = c(0.04, 0.03, 0.05, 0.06, 0.02)   # variances
)



## tiny deterministic sample for fast unit-tests
set.seed(1)
digDep_tiny <- metaMultiverse::data_digDep |>
  dplyr::slice_sample(n = 10)
