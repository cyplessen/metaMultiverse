## tiny deterministic sample for fast unit-tests
set.seed(1)
data_tiny <- metaMultiverse::data_digDep |>
  dplyr::slice_sample(n = 10)
