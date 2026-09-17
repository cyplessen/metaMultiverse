# Phase 1: fork objects, domain configs, load-time validation, crossed grid
# with estimand cells, U-in-metadata, X bias layer. Prototyped on simulated
# data only (per project rule: no real Metapsy data in mechanism tests).

warrant <- function(txt = "convention used in prior Metapsy reanalyses") {
  list(basis = "convention-unresolved", text = txt)
}

sim <- simulate_multiverse_data(n_studies = 40, seed = 7)

comparator_fork <- fork(
  name = "comparator", type = "data", decision = "changes_question", family = "clinical",
  column = "comparator",
  levels = list(wl = "wl", cau = "cau",
                all_controls = c("wl", "cau", "other")),
  reference = "all_controls",
  justification = list(basis = "citation",
                       text = "control condition changes the clinical question",
                       citation = "Cuijpers et al. 2019")
)

format_fork <- fork(
  name = "delivery_format", type = "data", decision = "uncertain", family = "clinical",
  column = "format",
  levels = list(individual = "ind", group = "grp",
                any_format = c("ind", "grp")),
  reference = "any_format", justification = warrant()
)

method_fork <- fork(
  name = "ma_method", type = "analysis", decision = "uncertain", family = "analytic",
  levels = c("reml", "pm"), reference = "reml", justification = warrant()
)

dependency_fork <- fork(
  name = "dependency", type = "analysis", decision = "uncertain", family = "analytic",
  levels = c("aggregate", "modeled"), reference = "aggregate",
  justification = warrant()
)

fe_fork <- fork(
  name = "fixed_effect_model", type = "analysis", decision = "indefensible", family = "analytic",
  levels = c("random_effects", "fixed_effect"), reference = "random_effects",
  justification = list(basis = "citation",
                       text = "FE under clear heterogeneity is indefensible; bias layer only",
                       citation = "Borenstein et al. 2010")
)

base_estimand <- list(population = "simulated adults",
                      intervention = "simulated psychotherapy",
                      outcome = "symptoms", timepoint = "post")

make_config <- function(forks, ...) {
  domain_config(domain = "simulated", forks = forks,
                estimand = base_estimand, ...)
}

# --- fork() validation -------------------------------------------------------

test_that("fork() enforces naming, levels, justification, reference", {
  expect_error(fork("Bad Name", "data", "equivalent", "analytic", warrant(),
                    column = "x", levels = list(a = "a", b = "b")),
               "snake_case")
  expect_error(fork("f", "data", "equivalent", "analytic", warrant(), column = "x",
                    levels = list(a = "a")), "at least 2 levels")
  expect_error(fork("f", "data", "equivalent", "analytic", column = "x",
                    levels = list(a = "a", b = "b"), justification = NULL),
               "justification")
  expect_error(fork("f", "data", "equivalent", "analytic",
                    list(basis = "citation", text = "no cite given"),
                    column = "x", levels = list(a = "a", b = "b")),
               "citation")
  expect_error(fork("f", "data", "equivalent", "analytic", warrant(), column = "x",
                    levels = list(a = "a", b = "b"), reference = "zzz"),
               "reference")
  expect_error(fork("f", "data", "equivalent", "analytic", warrant(),
                    levels = list(a = "a", b = "b")), "column")
})

test_that("bare-string justification becomes an unresolved-convention warrant", {
  f <- fork("f", "data", "uncertain", "analytic", "just a convention",
            column = "x", levels = list(a = "a", b = "b"))
  expect_equal(f$justification$basis, "convention-unresolved")
})

test_that("reference defaults to first level; E forks carry tolerance", {
  f <- fork("f", "data", "equivalent", "analytic", warrant(), column = "x",
            levels = list(a = "a", b = "b"))
  expect_equal(f$reference, "a")
  expect_equal(f$equivalence_margin, 0.1)
  u <- fork("u", "data", "uncertain", "analytic", warrant(), column = "x",
            levels = list(a = "a", b = "b"))
  expect_null(u$equivalence_margin)
})

# --- domain_config() ---------------------------------------------------------

test_that("domain_config rejects duplicate fork names and estimand clashes", {
  expect_error(make_config(list(comparator_fork, comparator_fork)),
               "Duplicated fork names")
  expect_error(
    domain_config("simulated", list(comparator_fork),
                  estimand = c(base_estimand, list(comparator = "any"))),
    "declared both"
  )
})

test_that("eligibility filters validate, apply, and log", {
  cfg <- make_config(
    list(comparator_fork, method_fork, dependency_fork),
    eligibility = list(
      drop_group = list(expr = "format != 'grp'", justification = warrant())
    )
  )
  filtered <- apply_eligibility(cfg, sim)
  expect_true(all(filtered$format != "grp"))
  log <- attr(filtered, "eligibility_log")
  expect_equal(log$filter, "drop_group")
  expect_true(log$n_after < log$n_before)

  expect_error(
    make_config(list(comparator_fork),
                eligibility = list(bad = list(expr = "format != 'grp'"))),
    "justification"
  )
})

# --- validate_domain_config --------------------------------------------------

test_that("validation errors on missing columns and absent levels (strict)", {
  bad_col <- fork("nope", "data", "uncertain", "analytic", warrant(),
                  column = "not_a_column", levels = list(a = "a", b = "b"))
  cfg <- make_config(list(bad_col, method_fork, dependency_fork))
  expect_error(validate_domain_config(cfg, sim), "not_a_column")

  ghost <- fork("ghost", "data", "uncertain", "analytic", warrant(),
                column = "comparator",
                levels = list(wl = "wl", ghost_level = "does_not_occur"))
  cfg2 <- make_config(list(ghost, method_fork, dependency_fork))
  expect_error(validate_domain_config(cfg2, sim), "do not occur")
  v <- validate_domain_config(cfg2, sim, strict = FALSE)
  expect_true("not_in_data" %in%
                v$availability$status[v$availability$level == "ghost_level"])
})

test_that("min-k suppression and structural fixing are classified, not dropped silently", {
  # make 'other' comparator rare: only 2 studies -> below min_studies = 5
  dat <- sim
  other_studies <- unique(dat$study[dat$comparator == "other"])
  keep <- dat$comparator != "other" |
    dat$study %in% other_studies[1:2]
  dat <- dat[keep, ]

  rare_fork <- fork("comparator", "data", "changes_question", "clinical",
                    warrant(), column = "comparator",
                    levels = list(wl = "wl", cau = "cau", other = "other"))
  cfg <- make_config(list(rare_fork, method_fork, dependency_fork))
  v <- validate_domain_config(cfg, dat)
  av <- v$availability
  expect_equal(av$status[av$level == "other"], "too_few_studies")
  expect_equal(
    v$fork_status$status[v$fork_status$fork == "comparator"], "varies")

  # only one comparator level exists in the data -> structurally fixed
  # (absent levels are informative errors under strict, statuses otherwise)
  wl_only <- dat[dat$comparator == "wl", ]
  cfg3 <- make_config(list(rare_fork, method_fork, dependency_fork))
  expect_error(validate_domain_config(cfg3, wl_only), "do not occur")
  v3 <- validate_domain_config(cfg3, wl_only, strict = FALSE)
  expect_equal(
    v3$fork_status$status[v3$fork_status$fork == "comparator"],
    "cannot_vary")
})

test_that("levels with identical realized subsets are marked as duplicates", {
  dat <- sim[sim$comparator %in% c("wl", "cau"), ]
  cfg <- make_config(list(
    fork("comparator", "data", "changes_question", "clinical", warrant(),
         column = "comparator",
         levels = list(wl = "wl", cau = "cau",
                       all_controls = c("wl", "cau", "other"))),
    method_fork, dependency_fork))
  v <- validate_domain_config(cfg, dat)
  av <- v$availability
  # with no 'other' rows, all_controls duplicates the wl+cau union -- but not
  # either single level; here it must remain distinct
  expect_false(any(startsWith(av$status[av$level == "all_controls"],
                              "same_studies")))
  # now remove cau too: all_controls collapses onto wl exactly
  dat_wl <- sim[sim$comparator == "wl", ]
  cfg2 <- make_config(list(
    fork("comparator", "data", "changes_question", "clinical", warrant(),
         column = "comparator",
         levels = list(wl = "wl", all_controls = c("wl", "cau", "other"))),
    method_fork, dependency_fork))
  v2 <- validate_domain_config(cfg2, dat_wl, strict = FALSE)
  av2 <- v2$availability
  expect_equal(av2$status[av2$level == "all_controls"], "same_studies_as_wl")
  expect_equal(
    v2$fork_status$status[v2$fork_status$fork == "comparator"],
    "cannot_vary")
})

test_that("adaptive fork levels materialize at validation time", {
  adaptive_fork <- fork(
    "rob_profile", type = "data", decision = "uncertain", family = "analytic",
    column = "rob",
    adaptive = function(data) {
      lv <- list(keep_all = unique(data$rob))
      if (any(data$rob == 0)) lv$low_rob_only <- 0
      lv
    },
    justification = warrant("data-dependent levels; hardcoded menu")
  )
  cfg <- make_config(list(comparator_fork, adaptive_fork,
                          method_fork, dependency_fork))
  v <- validate_domain_config(cfg, sim)
  expect_true(all(c("keep_all", "low_rob_only") %in%
                    names(v$levels$rob_profile)))
})

# --- build_specification_grid ------------------------------------------------

full_config <- make_config(list(comparator_fork, format_fork, method_fork,
                                dependency_fork, fe_fork))

grid <- build_specification_grid(full_config, sim)

test_that("N forks split into estimand cells; E/U vary within", {
  inference <- grid[grid$layer == "inference", ]
  cells <- attr(grid, "estimand_cells")
  expect_equal(sort(unique(inference$cell_id)),
               sort(names(cells)))
  expect_equal(length(cells), 3) # wl, cau, all_controls
  # each cell contains the full within-cell how/E/U cross
  per_cell <- table(inference$cell_id)
  expect_true(length(unique(per_cell)) == 1)
  # estimand declaration carries static fields + comparator field
  expect_equal(cells[["comparator=wl"]]$comparator, "wl")
  expect_equal(cells[["comparator=wl"]]$population, "simulated adults")
})

test_that("U forks are grid columns keyed by name -- no positional wf_ slots", {
  expect_true(all(c("comparator", "delivery_format", "ma_method",
                    "dependency") %in% names(grid)))
  expect_false(any(grepl("^wf_[0-9]+$", names(grid))))
  dm <- attr(grid, "decision_map")
  expect_equal(unname(dm[["delivery_format"]]), "uncertain")
  expect_equal(unname(dm[["comparator"]]), "changes_question")
})

test_that("X forks appear only in the bias layer, others at reference", {
  inference <- grid[grid$layer == "inference", ]
  bias <- grid[grid$layer == "bias", ]
  expect_true(all(is.na(inference$fixed_effect_model)))
  expect_true(all(!is.na(bias$fixed_effect_model)))
  # in bias rows, non-N non-X forks sit at their reference levels
  expect_true(all(bias$delivery_format == "any_format"))
  expect_true(all(bias$ma_method == "reml"))
  expect_true(all(bias$dependency == "aggregate"))
  # bias layer exists per estimand cell
  expect_equal(sort(unique(bias$cell_id)), sort(unique(inference$cell_id)))
})

test_that("method x dependency compatibility filters the grid", {
  puni_method <- fork("ma_method", "analysis", "uncertain", "analytic", warrant(),
                      levels = c("reml", "p-uniform"), reference = "reml")
  cfg <- make_config(list(comparator_fork, puni_method, dependency_fork))
  g <- build_specification_grid(cfg, sim)
  inf <- g[g$layer == "inference", ]
  # p-uniform supports neither aggregate nor modeled -> only reml rows remain
  expect_true(all(inf$ma_method == "reml"))
  expect_true(attr(g, "attrition")$incompatible_method_dependency > 0)
})

test_that("spec ids are unique, stable, and self-describing", {
  expect_false(anyDuplicated(grid$spec_id) > 0)
  expect_true(all(grepl("^simulated::", grid$spec_id)))
  g2 <- build_specification_grid(full_config, sim)
  expect_identical(grid$spec_id, g2$spec_id) # deterministic rebuild
})

test_that("attrition attribute reports planned vs defined vs bias", {
  at <- attr(grid, "attrition")
  expect_true(at$planned >= at$defined)
  expect_equal(at$bias_layer, sum(grid$layer == "bias"))
})

test_that("grid errors informatively when nothing varies", {
  tiny <- sim[sim$comparator == "wl", ][1:3, ]
  cfg <- make_config(list(comparator_fork, method_fork, dependency_fork))
  expect_error(suppressWarnings(build_specification_grid(cfg, tiny)))
})
