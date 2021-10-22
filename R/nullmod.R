#' Evaluate the null model on a dataset.
#'
#' @inheritParams train_gbm
#' @param mod_mode A string. `"classification"` or `"regression"`.
#' @param cv_resamp A cross-validation resampling of `training_data` created by
#'   [rsample::vfold_cv()].
#' @param metrics A [yardstick::metric_set]. Only the first metric in the set is
#'   used.
#'
#' @return The output of a call to [tune::collect_metrics] for the performance
#'   of the null model in the cross-validation.
#'
#' @noRd
evaluate_nullmod <- function(training_data, mod_mode, outcome,
                             cv_resamp, metrics) {
  # Argument checking ----------------------------------------------------------
  checkmate::assert_data_frame(training_data, min.rows = 2, min.cols = 1)
  checkmate::assert_string(mod_mode)
  checkmate::assert_names(mod_mode,
    subset.of = c("classification", "regression")
  )
  checkmate::assert_string(outcome)
  checkmate::assert_names(outcome, subset.of = names(training_data))
  checkmate::assert_class(cv_resamp, "vfold_cv")
  checkmate::assert_class(metrics, "metric_set")
  checkmate::assert_true(length(attr(metrics, "metrics")) > 0)

  # Parallel setup (turns parallel stuff off; somehow faster for this) ---------
  old_plan <- future::plan(future::multisession, workers = 1)
  on.exit(future::plan(old_plan), add = TRUE)
  doFuture::registerDoFuture()

  # Main body ------------------------------------------------------------------
  null_mod <- parsnip::null_model(mode = mod_mode) %>%
    parsnip::set_engine("parsnip")
  null_rec <- recipes::recipe(
    training_data
  ) %>%
    recipes::update_role(dplyr::everything(), new_role = "predictor") %>%
    recipes::update_role(dplyr::one_of(outcome), new_role = "outcome")
  out <- null_mod %>%
    tune::fit_resamples(null_rec, cv_resamp, metrics = metrics) %>%
    tune::collect_metrics()
  null_wf <- workflows::workflow() %>%
    workflows::add_recipe(null_rec) %>%
    workflows::add_model(null_mod) %>%
    generics::fit(data = training_data)
  structure(out, null_wf = null_wf)
}

#' Check a model's performance against the null model.
#'
#' There are optionally errors/warnings if the null model is (nearly) as good or
#' better than the competing model.
#'
#' @param mod_performance The output of [tune::select_best()] for the model to
#'   compare the null model to.
#' @param training_data A data frame. Data for evaluating the models via
#'   cross-validation.
#' @inheritParams evaluate_nullmod
#' @param warn_if_nullmod,err_if_nullmod Flags. Warn/error if the null model is
#'   (nearly) as good or better than the competing model.
#'
#' @return `"fail"` if the null model is (nearly) as good or better than the
#'   competing model. Otherwise `"pass"`.
#'
#' @noRd
check_against_nullmod <- function(mod_performance, training_data, mod_mode,
                                  outcome, cv_resamp, metrics,
                                  warn_if_nullmod, err_if_nullmod) {
  # Argument checking ----------------------------------------------------------
  checkmate::assert_data_frame(mod_performance, nrows = 1)
  checkmate::assert_names(
    c(".metric", ".estimator", "mean", "n", "std_err"),
    subset.of = names(mod_performance)
  )
  checkmate::assert_data_frame(training_data, min.cols = 1, min.rows = 2)
  checkmate::assert_string(mod_mode)
  mod_mode <- strex::match_arg(mod_mode, c("classification", "regression"))
  checkmate::assert_string(outcome)
  checkmate::assert_names(outcome, subset.of = names(training_data))
  checkmate::assert_class(cv_resamp, "vfold_cv")
  checkmate::assert_class(metrics, "metric_set")
  checkmate::assert_flag(warn_if_nullmod)
  checkmate::assert_flag(err_if_nullmod)

  # Main body
  nullmod_perf <- evaluate_nullmod(
    training_data = training_data,
    mod_mode = mod_mode,
    outcome = outcome,
    cv_resamp = cv_resamp,
    metrics = metrics
  )
  out <- structure("fail",
    null_wf = attr(nullmod_perf, "null_wf"),
    cv_performance = nullmod_perf
  )
  if (get_metric_direction(metrics) == "maximize") {
    mod_performance$mean[1] <- -mod_performance$mean[1]
    nullmod_perf$mean[1] <- -nullmod_perf$mean[1]
  }
  msg <- paste(
    "The null model {sub_msg} the fitted model.",
    "Returning the null model."
  )
  if (mod_performance$mean[1] >= nullmod_perf$mean[1]) {
    sub_msg <- "performed better than"
    if (err_if_nullmod) {
      custom_stop(msg, .envir = list(sub_msg = sub_msg))
    } else if (warn_if_nullmod) {
      warning(stringr::str_glue(msg))
    }
  } else if (mod_performance$mean[1] >=
    nullmod_perf$mean[1] - nullmod_perf$std_err[1]) {
    sub_msg <- "was within 1 standard error of"
    if (err_if_nullmod) {
      custom_stop(msg, .envir = list(sub_msg = sub_msg))
    } else if (warn_if_nullmod) {
      warning(stringr::str_glue(msg))
    }
  } else {
    out <- "pass"
  }
  out
}
