#' Digital Depression Intervention Meta-Analysis Dataset
#'
#' A dataset containing standardized mean differences (SMD) from
#' depression digital intervention studies for use in demonstrating multiverse meta-analysis.
#'
#' @format A data frame with rows representing effect sizes and 12 variables:
#' \describe{
#'   \item{study}{Character. Study identifier (e.g., "Addington, 2019").}
#'   \item{es_id}{Numeric. Unique effect size identifier.}
#'   \item{yi}{Numeric. Effect size (standardized mean difference).}
#'   \item{vi}{Numeric. Variance of the effect size.}
#'   \item{sei}{Numeric. Standard error of the effect size.}
#'   \item{wf_1}{Character. Technology type (website, mobile, etc.).}
#'   \item{wf_2}{Character. Guidance level (guided, minimal to no support, etc.).}
#'   \item{wf_3}{Character. Population type (adult, etc.).}
#'   \item{wf_4}{Character. Therapy type (cbt-based, not-cbt-based, etc.).}
#'   \item{wf_5}{Character. Control group type (wl = waitlist, other ctr = other control).}
#'   \item{wf_6}{Character. Diagnostic status (cut = cutoff for diagnosis).}
#'   \item{wf_7}{Character. Risk of bias (some concerns, high risk, etc.).}
#'   \item{wf_8}{Character. Time point (post, follow-up).}
#'   \item{condition_arm1}{Character. Description of intervention arm (cbt, other psy).}
#'   \item{condition_arm2}{Character. Description of control arm (wl, other ctr).}
#' }
#'
#' @details
#' This dataset contains effect size information for digital depression interventions.
#' Each row represents one effect size from a study, with multiple effect sizes
#' possible per study. The "wf_" columns represent "which factors" that can be used for
#' analytical choices in multiverse meta-analysis.
#'
#' Abbreviations:
#' \itemize{
#'   \item wl: waitlist control
#'   \item cbt: cognitive behavioral therapy
#'   \item other psy: other psychological intervention
#'   \item other ctr: other control condition
#' }
#'
#' @source Depression digital intervention studies compiled for multiverse meta-analysis.
#'
#' @examples
#' data(data_digDep)
#' head(data_digDep)
#' check_data_multiverse(data_digDep)
#'
"data_digDep"
