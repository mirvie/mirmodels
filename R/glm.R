#' `r roxy_mod_heading("Generalized Lasso linear")`
#'
#' Train a generalized Lasso linear model. The training routine automatically
#' selects the best lambda parameter using [glmnet::cv.glmnet()].
#'
#' The final model will be evaluated with the metric of your choice, but the
#' hyperparameter tuning will be done using deviance. This is necessary to use
#' [glmnet::cv.glmnet()].
#'
#' @param metric A string. The probability metric to choose the best model.
#'   and select the best one. Common choices are `"mn_log_loss"`, `"roc_auc"`
#'   and `"accuracy"`. This should be a metric that is available in the
#'   `yardstick` package, but use e.g. "mae" and not "yardstick::mae" in this
#'   argument.
#'   `r roxy_metric_char_vec()`
#' @inheritParams train_lm
#'
#' @return `r roxy_tidymodel_return()`
#'
#' @family model trainers
#'
#' @examples
#' iris_data <- janitor::clean_names(datasets::iris)
#' iris_data_split <- rsample::initial_split(iris_data, strata = species)
#' mod <- train_glm(
#'   training_data = rsample::training(iris_data_split),
#'   outcome = "species",
#'   metric = c("mn_log_loss", "roc_auc"),
#'   n_cores = 5
#' )
#' preds <- predict(mod, new_data = rsample::testing(iris_data_split)) %>%
#'   magrittr::set_names("pred") %>%
#'   dplyr::mutate(truth = rsample::testing(iris_data_split)$species)
#' yardstick::accuracy(preds, truth, pred)
#' preds_prob <- predict(mod,
#'   new_data = rsample::testing(iris_data_split),
#'   type = "prob"
#' )
#' dplyr::bind_cols(preds_prob,
#'   truth = rsample::testing(iris_data_split)$species
#' )
#' yardstick::mn_log_loss_vec(
#'   truth = rsample::testing(iris_data_split)$species,
#'   estimate = as.matrix(preds_prob)
#' )
#' @export
train_glm <- function(training_data, outcome, metric = "mn_log_loss",
                      na_action = c("medianimpute", "knnimpute"),
                      lambda = NULL,
                      cv_nfolds = 10, id_col = NULL, strata = NULL,
                      selection_method = "Breiman",
                      include_nullmod = TRUE, err_if_nullmod = FALSE,
                      warn_if_nullmod = TRUE, n_cores = 1) {
  checked_args <- argchk_train_glm(
    training_data = training_data,
    id_col = id_col,
    outcome = outcome,
    metric = metric,
    selection_method = selection_method,
    cv_nfolds = cv_nfolds,
    n_cores = n_cores,
    na_action = na_action
  )
  c(selection_method, na_action) %<-% checked_args[
    c("selection_method", "na_action")
  ]
  metrics <- chr_vec_to_metric_set(metric)
  if (get_metric_type(metrics) != "probability") {
    custom_stop(
      "You have selected a numeric metric.",
      "Please select a probability metric.",
      "'mn_log_loss' is a good probability metric."
    )
  }

  # Parallel setup -------------------------------------------------------------
  old_plan <- future::plan(future::multisession, workers = n_cores)
  on.exit(future::plan(old_plan), add = TRUE)
  doFuture::registerDoFuture()
  withr::local_options(list(future.globals.maxSize = 4e9))

  # Main body ------------------------------------------------------------------
  mod_rec <- create_glm_recipe(
    training_data = training_data,
    outcome = outcome,
    id_col = id_col,
    na_action = na_action
  )
  mod_rec_prepped <- recipes::prep(mod_rec, strings_as_factors = FALSE)
  current_seed <- get_seed()
  set.seed(current_seed)
  cv_resamp <- rsample::vfold_cv(training_data,
    v = cv_nfolds,
    strata = !!strata
  )
  cvg_help <- cvg_lambda_help(
    prepped_rec = mod_rec_prepped, outcome = outcome,
    metric = metric, foldid = get_fold_ids(cv_resamp),
    selection_method = selection_method,
    lambda_user = lambda, n_cores = n_cores
  )
  penalty <- dplyr::case_when(
    !is.null(lambda) ~ cvg_help$lambda_user,
    selection_method == "absolute" ~ cvg_help$best_lambda,
    selection_method == "Breiman" ~ cvg_help$lambda_1se
  )
  if (cvg_help$family == "multinomial") {
    mod_spec <- parsnip::multinom_reg(
      penalty = tune::tune(),
      mixture = tune::tune()
    )
  } else {
    mod_spec <- parsnip::logistic_reg(
      penalty = tune::tune(),
      mixture = tune::tune()
    )
  }
  set_engine_args <- list(
    object = mod_spec,
    engine = "glmnet",
    nlambda = cvg_help$n_lambda,
    lambda.min.ratio = cvg_help$lambda_min_ratio
  )
  mod_spec <- do.call(parsnip::set_engine, set_engine_args)
  set.seed(current_seed)
  train_on_grid(
    mod_spec = mod_spec,
    hyper_param_grid = list(penalty = penalty, mixture = 1),
    mod_rec = mod_rec,
    training_data = training_data,
    outcome = outcome,
    cv_nfolds = cv_nfolds,
    cv_nreps = 1,
    strata = strata,
    metric = metric,
    selection_method = selection_method,
    simplicity_params = "-penalty",
    include_nullmod = include_nullmod,
    err_if_nullmod = err_if_nullmod,
    warn_if_nullmod = warn_if_nullmod,
    n_cores = n_cores
  ) %>%
    structure(lambda = penalty)
}
