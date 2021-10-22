#' `r roxy_mod_heading("Lasso linear")`
#'
#' Train a Lasso linear model. The training routine automatically selects the
#' best lambda parameter using [glmnet::cv.glmnet()].
#'
#' @param metric A string. `"rmse"` or `"mae"`.
#' @param na_action A string. How to impute missing data in explanatory
#'   variables`"medianimpute"` or `"knnimpute"`. See
#'   [recipes::step_medianimpute()] and [recipes::step_knnimpute()]. Default is
#'   `"medianimpute"`.
#' @param lambda A numeric vector. Optional. A grid of lambdas for tuning the
#'   Lasso. If you leave this as `NULL`, recommended, a sensible grid is chosen
#'   for you.
#' @inheritParams train_gbm
#' @inheritParams train_on_grid
#'
#' @return `r roxy_tidymodel_return()`
#'
#' @family model trainers
#'
#' @examples
#' iris_data <- janitor::clean_names(datasets::iris)
#' iris_data_split <- rsample::initial_split(iris_data, strata = species)
#' mod <- train_lm(
#'   training_data = rsample::training(iris_data_split),
#'   outcome = "petal_length",
#'   metric = "mae",
#'   n_cores = 5
#' )
#' preds <- predict(mod, new_data = rsample::testing(iris_data_split))
#' dplyr::bind_cols(preds,
#'   truth = rsample::testing(iris_data_split)$petal_length
#' )
#' yardstick::mae_vec(
#'   truth = rsample::testing(iris_data_split)$petal_length,
#'   estimate = preds[[1]]
#' )
#' @export
train_lm <- function(training_data, outcome, metric = c("rmse", "mae"),
                     na_action = c("medianimpute", "knnimpute"),
                     lambda = NULL,
                     cv_nfolds = 10, id_col = NULL,
                     strata = NULL,
                     selection_method = "Breiman",
                     include_nullmod = TRUE, err_if_nullmod = FALSE,
                     warn_if_nullmod = TRUE, n_cores = 1) {
  checked_args <- argchk_train_lm(
    training_data = training_data,
    id_col = id_col,
    outcome = outcome,
    metric = metric,
    selection_method = selection_method,
    cv_nfolds = cv_nfolds,
    n_cores = n_cores,
    na_action = na_action
  )

  # Main body ------------------------------------------------------------------
  c(metric, selection_method, na_action) %<-% checked_args[
    c("metric", "selection_method", "na_action")
  ]
  metrics <- glue::glue("yardstick::metric_set(yardstick::{metric})") %>%
    parse(text = .) %>%
    eval()
  mod_rec <- create_lm_recipe(
    training_data = training_data,
    outcome = outcome,
    id_col = id_col,
    na_action = na_action
  )
  mod_rec_prepped <- recipes::prep(mod_rec, strings_as_factors = FALSE)
  cv_resamp <- rsample::vfold_cv(training_data,
    v = cv_nfolds,
    strata = !!strata
  )
  cvg_help <- cvg_lambda_help(mod_rec_prepped,
    outcome = outcome,
    metric = metric, foldid = get_fold_ids(cv_resamp),
    selection_method = selection_method,
    lambda_user = lambda, n_cores = n_cores
  )
  mod_performance <- dplyr::tibble(
    .metric = cvg_help$type_measure,
    .estimator = "standard",
    mean = dplyr::case_when(
      !is.null(lambda) ~ cvg_help$lambda_user_metric_mn,
      selection_method == "absolute" ~ cvg_help$best_metric_mn,
      selection_method == "Breiman" ~ cvg_help$metric_mn_1se
    ),
    n = as.integer(cv_nfolds),
    std_err = dplyr::case_when(
      !is.null(lambda) ~ cvg_help$lambda_user_metric_se,
      selection_method == "absolute" ~ cvg_help$best_metric_se,
      selection_method == "Breiman" ~ cvg_help$metric_se_1se
    )
  )
  if (metric == "rmse") {
    mod_performance <- dplyr::mutate(mod_performance,
      .metric = "rmse",
      mean = sqrt(mean),
      std_err = sqrt(std_err)
    )
  }
  if (include_nullmod) {
    nullmod_test_result <- check_against_nullmod(
      mod_performance = mod_performance,
      training_data = training_data,
      mod_mode = "regression",
      outcome = outcome,
      cv_resamp = cv_resamp,
      metrics = metrics,
      warn_if_nullmod = warn_if_nullmod,
      err_if_nullmod = err_if_nullmod
    )
    if (nullmod_test_result == "fail") {
      return(
        structure(attr(nullmod_test_result, "null_wf"),
          cv_performance = attr(nullmod_test_result, "cv_performance")
        )
      )
    }
  }
  penalty <- dplyr::case_when(
    !is.null(lambda) ~ cvg_help$lambda_user,
    selection_method == "absolute" ~ cvg_help$best_lambda,
    selection_method == "Breiman" ~ cvg_help$lambda_1se
  )
  linear_mod <- parsnip::linear_reg(penalty = penalty, mixture = 1)
  set_engine_call_txt <- glue::glue(
    "parsnip::set_engine(
      linear_mod, \"glmnet\",
      nlambda = {cvg_help$n_lambda},
      lambda.min.ratio = {cvg_help$lambda_min_ratio}
    )"
  )
  linear_mod <- eval(parse(text = set_engine_call_txt))
  wf <- workflows::workflow() %>%
    workflows::add_recipe(mod_rec) %>%
    workflows::add_model(linear_mod)
  linear_fit <- generics::fit(wf, data = training_data)
  structure(linear_fit, cv_performance = mod_performance)
}
