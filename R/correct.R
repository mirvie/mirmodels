#' Correct data for the effects of selected covariates.
#'
#' This function uses linear models to regress away the effects of selected
#' covariates on selected columns of a data frame. One may optionally specify
#' variables whose effects are considered _real_ or _of interest_ and their
#' effects will not be regressed away (only effects orthogonal to those will be
#' regressed away).
#'
#' If `keep_effect_cols` is `NULL`, this function is just a wrapper around
#' [multi_lm()] and [multi_resids()] with `reset_mean_med = TRUE`. That is, for
#' each variable in `correct_cols`, a linear model is fit with the variables
#' `correct_for_cols` as explanatory variables. Then the residuals from this
#' model (reset about their original mean or median) are kept as the _corrected_
#' values of those variables.
#'
#' If `keep_effect_cols` is not `NULL`, then first, for each variable in
#' `correct_cols`, a linear model is fit with the variables `keep_effect_cols`
#' as explanatory variables. The fitted variables from these models are
#' remembered as the effects of these `keep_effect_cols` on `correct_cols`. The
#' residuals from these models are then components of `correct_cols` which can't
#' be explained by `keep_effect_cols`. With these residuals, the effects of
#' `correct_for_cols` are regressed away, and what remains is added onto the
#' fitted values from the modelling of `correct_cols` with `keep_effect_cols`.
#'
#' Columns in `training_data` that are not specified in `correct_cols`,
#' `correct_for_cols`, or `keep_effect_cols` will be returned unchanged.
#'
#' @param training_data A data frame containing one sample per row. The
#'   correction is learned and applied on this data.
#' @param testing_data A data frame. The testing counterpart to `training_data`.
#'   The correction that is learned on `training_data` is applied here. It's
#'   fine to pass this argument as `NULL`, in which case no testing data
#'   correction takes place.
#' @param correct_cols A character vector. The names of the columns that are to
#'   be altered (corrected). These columns must all be numeric.
#' @param correct_for_cols A character vector. The names of the columns that are
#'   to be corrected _for_. These columns must all be numeric, factor or
#'   logical.
#' @param keep_effect_cols A character vector. The names of the column
#'   specifying variables whose effects should _not_ be regressed away. These
#'   columns must all be numeric, factor or logical. If there are no columns
#'   whose effects should _not_ be regressed away, pass this argument as `NULL`.
#' @inheritParams multi_lm
#'
#' @return A list with elements named `training_data` and `testing_data`. The
#'   corrected data.
#'
#' @examples
#' if (rlang::is_installed("mirmisc")) {
#'   data <- get_combined_cohort_data(
#'     c("bw", "ga", "io", "kl", "pm", "pt", "rs"),
#'     cpm = FALSE, log2 = TRUE, tot_counts = TRUE,
#'     gene_predicate = ~ median(.) > 0
#'   )
#'   res <- linear_correct(
#'     data,
#'     correct_cols = mirmisc::get_df_gene_names(data),
#'     correct_for_cols  = c("log2_tot_counts"),
#'     keep_effect_cols = "meta_collectionga"
#'   )
#' }
#' @export
linear_correct <- function(training_data, testing_data = NULL,
                           correct_cols, correct_for_cols,
                           keep_effect_cols = NULL, robust = TRUE) {
  checked_args <- argchk_linear_correct(
    training_data = training_data,
    testing_data = testing_data,
    correct_cols = correct_cols,
    correct_for_cols = correct_for_cols,
    keep_effect_cols = keep_effect_cols,
    robust = robust
  )
  c(training_data, testing_data, keep_effect_cols) %<-%
    checked_args[c("training_data", "testing_data", "keep_effect_cols")]
  if (length(keep_effect_cols)) {
    training_keep_lms <- multi_lm(training_data,
      xs = keep_effect_cols,
      ys = correct_cols,
      robust = robust
    )
    training_keep_resids <- multi_resids(training_keep_lms,
      lms_data = training_data,
      reset_mean_med = TRUE
    )
    training_correct_lms <- multi_lm(training_keep_resids,
                                     xs = correct_for_cols,
                                     ys = correct_cols,
                                     robust = robust)
  } else {
    training_correct_lms <- multi_lm(training_data,
                                     xs = correct_for_cols,
                                     ys = correct_cols,
                                     robust = robust)
  }
  out <- list()
  out$training_data <- multi_resids(training_correct_lms,
                                    lms_data = training_data,
                                    reset_mean_med = TRUE,
                                    new_data = training_data)
  
  if (is.null(testing_data)) {
    out["testing_data"] <- list(NULL)
  } else {
    out$testing_data <- multi_resids(training_correct_lms,
                                     lms_data = training_data,
                                     reset_mean_med = TRUE,
                                     new_data = testing_data)
  }
  out
}
