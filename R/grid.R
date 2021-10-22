#' Train several models with different hyperparameters and select the best one.
#'
#' Tune a model based on a provided hyperparameter grid by cross-validation.
#' Select the best one, optionally including the null model in the comparison.
#'
#' @param mod_spec A parsnip model specification. It must include the model mode
#'   and engine. See [parsnip::set_mode()] and [parsnip::set_engine()].
#' @param hyper_param_grid A data frame with one row per hyperparameter
#'   combination. The column names give the hyper parameter names. Can
#'   optionally be passed as a list which is made into a tibble by
#'   [tidyr::expand_grid()].
#' @param mod_rec The recipe for preparing the data for this model. See
#'   [recipes::recipe()].
#' @param training_data A data frame. The data used to train the model.
#' @param outcome A string. The name of the outcome variable. This must be a
#'   column in `training_data`.
#' @param cv_nfolds A positive integer. The number of folds for
#'   cross-validation.
#' @param cv_nreps A positive integer. The number of repeated rounds in the
#'   cross-validation.
#' @param strata A string. Variable to stratify on when splitting for
#'   cross-validation.
#' @param id_col A string. If there is a sample identifier column, specify it
#'   here to tell the model not to use it as a predictor.
#' @param metric A string. The metric to use to evaluate the models and select
#'   the best one. Common choices are `"rmse"`, `"mae"`, `"roc_auc"`,
#'   `"accuracy"`, `"mn_log_loss"`. This should be a metric that is available in
#'   the `yardstick` package, but use e.g. `"mae"` and not `"yardstick::mae"` in
#'   this argument. `r roxy_metric_char_vec()`
#' @param selection_method A string. How to select the best model. There are two
#'   options: "Breiman" and "absolute". "absolute" selects the best model by
#'   selecting the model with the best mean performance according to the chosen
#'   metric. "Breiman" selects the simplest model that comes within one standard
#'   deviation of the best score. The idea being that simple models generalize
#'   better, so it's better to select a simple model that had near-best
#'   performance.
#' @param simplicity_params A character vector. For `selection_method =
#'   "Breiman"`. These are passed directly to [tune::select_by_one_std_err()]
#'   and used to sort `hyper_param_grid` by simplicity. To sort descending, put
#'   a minus in front of the parameter. For example, to sort ascending on "x"
#'   and then descending on "y", use `simplicity_params = c("x", "-y")`. See
#'   [tune::select_by_one_std_err()] for details.
#' @param include_nullmod A bool. Include the null model (predicts mean or most
#'   common class every time) in the model comparison? This is recommended. If
#'   the null model comes within a standard deviation of the otherwise best
#'   model, the null model is chosen instead.
#' @param err_if_nullmod A bool. If the null model is chosen, throw an error
#'   rather than returning the null model.
#' @param warn_if_nullmod A bool. Warn if returning the null model?
#' @param n_cores A positive integer. The cross-validation can optionally be
#'   done in parallel. Specify the number of cores for parallel processing here.
#'
#' @return A `parsnip` `model_fit` object with a `predict()` method and `recipe`
#'   and `cv_performance` attributes.
#'
#' @export
train_on_grid <- function(mod_spec, hyper_param_grid, mod_rec,
                          training_data, outcome,
                          cv_nfolds, cv_nreps = 1, strata = NULL, id_col = NULL,
                          metric, selection_method = "Breiman",
                          simplicity_params = NULL, include_nullmod = TRUE,
                          err_if_nullmod = FALSE, warn_if_nullmod = TRUE,
                          n_cores = 1) {

  # Argument checking ----------------------------------------------------------
  argchk_train_on_grid(
    mod_spec = mod_spec, hyper_param_grid = hyper_param_grid, mod_rec = mod_rec,
    training_data = training_data, outcome = outcome,
    cv_nfolds = cv_nfolds, cv_nreps = cv_nreps, strata = strata,
    id_col = id_col, metric = metric, selection_method = selection_method,
    simplicity_params = simplicity_params, include_nullmod = include_nullmod,
    err_if_nullmod = err_if_nullmod, warn_if_nullmod = warn_if_nullmod,
    n_cores = n_cores
  )
  selection_method <- strex::match_arg(selection_method,
    c("Breiman", "absolute"),
    ignore_case = TRUE
  )
  if ((is.list(hyper_param_grid)) && (!is.data.frame(hyper_param_grid))) {
    hyper_param_grid <- do.call(tidyr::expand_grid, hyper_param_grid)
  }
  hyper_param_grid <- unique(hyper_param_grid)

  # Parallel setup -------------------------------------------------------------
  old_plan <- future::plan(future::multisession,
    workers = n_cores
  )
  on.exit(future::plan(old_plan), add = TRUE)
  doFuture::registerDoFuture()
  withr::local_options(list(future.rng.onMisuse = "ignore"))

  # Setup resampling and evaluate hyperparameter grid
  resamp <- suppressMessages(
    rsample::vfold_cv(training_data,
      v = cv_nfolds, repeats = cv_nreps,
      strata = strata
    )
  )
  metrics <- chr_vec_to_metric_set(metric)
  metric_names <- names(attr(metrics, "metrics"))
  tuned_grid <- suppressMessages(
    tune::tune_grid(mod_spec,
      mod_rec,
      resamp,
      grid = hyper_param_grid,
      metrics = metrics
    )
  )
  collected_metrics <- tune::collect_metrics(tuned_grid)

  # Select best model, including by default the null model for consideration
  if (selection_method == "Breiman") {
    dots_exprs <- rlang::parse_exprs(simplicity_params)
    best_params <- tune::select_by_one_std_err(tuned_grid,
      metric = metric[[1]],
      !!!dots_exprs
    )
  } else if (selection_method == "absolute") {
    best_params <- tune::select_best(tuned_grid, metric = metric[[1]]) %>%
      dplyr::inner_join(collected_metrics, by = names(.))
  }
  param_names <- utils::head(
    names(best_params),
    match(".metric", names(best_params)) - 1
  )
  cv_performance <- best_params[rep(1, length(metric_names)), param_names] %>%
    dplyr::mutate(.metric = metric_names) %>%
    dplyr::left_join(collected_metrics, by = names(.))
  final_mod <- tune::finalize_model(mod_spec, best_params)
  null_check <- "none"
  if (include_nullmod) {
    null_check <- check_against_nullmod(
      mod_performance = cv_performance[1, ],
      training_data = training_data,
      mod_mode = mod_spec$mode,
      outcome = outcome,
      cv_resamp = resamp,
      metrics = metrics,
      warn_if_nullmod = warn_if_nullmod,
      err_if_nullmod = err_if_nullmod
    )
  }
  if (null_check == "fail") {
    out <- structure(attr(null_check, "null_wf"),
      cv_performance = attr(null_check, "cv_performance")
    )
  } else {
    wf <- workflows::workflow() %>%
      workflows::add_recipe(mod_rec) %>%
      workflows::add_model(final_mod)
    out <- structure(generics::fit(wf, data = training_data),
      cv_performance = cv_performance
    )
  }
  out
}
