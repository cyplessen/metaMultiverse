## tiny deterministic sample for fast unit-tests
set.seed(1)
create_data_tiny <- function() {
  set.seed(1)
  metaMultiverse::data_digDep |>
    dplyr::slice_sample(n = 10)
}
