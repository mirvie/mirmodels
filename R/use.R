#' Set up a GBM.
#'
#' Copy boilerplate for fitting a GBM to the clipboard.
#'
#' @param mode A string. `"classification"` or `"regression"`.
#' @param outcome A string. The name of the outcome variable in your data.
#'
#' @return The character vector copied to clipboard, invisibly.
#'
#' @export
use_mirvie_gbm <- function(mode, outcome) {
  checkmate::assert_string(outcome, min.chars = 1)
  mode <- strex::str_match_arg(mode, c("classification", "regression"),
    ignore_case = TRUE
  )
  out <- readr::read_lines(
    system.file("scripts", "gbm-template.R", package = "mirmodels")
  )
  out <- stringr::str_replace_all(out, "INSERT_MODE_HERE", mode)
  out <- stringr::str_replace_all(out, "INSERT_OUTCOME_HERE", outcome)
  metrics <- dplyr::if_else(
    mode == "classification",
    "mn_log_loss, roc_auc",
    "rmse, auc"
  )
  out <- stringr::str_replace_all(out, "INSERT_METRICS_HERE", metrics)
  clipr::write_clip(out)
  usethis::ui_done("GBM template copied to clipboard.")
  invisible(out)
}

#' Set up a GLM.
#'
#' Copy boilerplate for fitting a GLM to the clipboard.
#'
#' @param outcome A string. The name of the outcome variable in your data.
#'
#' @return The character vector copied to clipboard, invisibly.
#'
#' @export
use_mirvie_glm <- function(outcome) {
  checkmate::assert_string(outcome, min.chars = 1)
  out <- readr::read_lines(
    system.file("scripts", "glm-template.R", package = "mirmodels")
  )
  out <- stringr::str_replace_all(out, "INSERT_OUTCOME_HERE", outcome)
  metrics <- "mn_log_loss, roc_auc"
  out <- stringr::str_replace_all(out, "INSERT_METRICS_HERE", metrics)
  clipr::write_clip(out)
  usethis::ui_done("GLM template copied to clipboard.")
  invisible(out)
}
