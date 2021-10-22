#' `r roxy_mod_heading("gradient boosted")`
#'
#' `r roxy_mod_desc("gradient boosted", c("trees", "tree_depth", "learn_rate"))`
#'
#' @inheritParams train_glm
#' @inheritParams train_on_grid
#'
#' @return `r roxy_tidymodel_return()`
#'
#' @family model trainers
#'
#' @examples
#' iris_data <- janitor::clean_names(datasets::iris)
#' iris_data_split <- rsample::initial_split(iris_data, strata = species)
#' iris_training_data <- rsample::training(iris_data_split)
#' iris_testing_data <- rsample::testing(iris_data_split)
#' mod <- train_gbm(
#'   training_data = iris_training_data, outcome = "species",
#'   metric = "mn_log_loss",
#'   hyper_param_grid = list(
#'     trees = c(20, 50),
#'     tree_depth = c(1, 2),
#'     learn_rate = c(0.01, 0.1)
#'   ),
#'   simplicity_params = c("trees", "learn_rate"),
#'   strata = c("species"),
#'   n_cores = 5
#' )
#' preds <- predict(mod, new_data = iris_testing_data, type = "prob")
#' dplyr::bind_cols(preds, truth = iris_testing_data$species)
#' yardstick::mn_log_loss_vec(
#'   truth = iris_testing_data$species,
#'   estimate = as.matrix(preds)
#' )
#' @export
train_gbm <- function(training_data, outcome, metric,
                      hyper_param_grid = list(
                        trees = c(5, 10, 20, 40),
                        tree_depth = c(1, 2),
                        learn_rate = c(0.01, 0.1, 0.2, 0.5)
                      ),
                      cv_nfolds = 5, cv_nreps = 1, id_col = NULL, strata = NULL,
                      selection_method = "Breiman", simplicity_params = NULL,
                      include_nullmod = TRUE, err_if_nullmod = FALSE,
                      warn_if_nullmod = TRUE, n_cores = 1) {
  argchk_train_gbm(
    training_data = training_data,
    id_col = id_col,
    selection_method = selection_method,
    outcome = outcome,
    n_cores = n_cores
  )

  mod_mode <- dplyr::if_else(
    is.numeric(training_data[[outcome]]),
    "regression", "classification"
  )
  boost_mod <- parsnip::boost_tree(
    mode = mod_mode,
    trees = tune::tune(),
    tree_depth = tune::tune(),
    learn_rate = tune::tune()
  ) %>%
    parsnip::set_engine("xgboost")

  mod_rec <- create_xgb_recipe(
    training_data = training_data,
    outcome = outcome,
    id_col = id_col
  )

  train_on_grid(
    mod_spec = boost_mod,
    hyper_param_grid = hyper_param_grid,
    mod_rec = mod_rec,
    training_data = training_data,
    outcome = outcome,
    cv_nfolds = cv_nfolds,
    cv_nreps = cv_nreps,
    strata = strata,
    metric = metric,
    selection_method = selection_method,
    simplicity_params = simplicity_params,
    include_nullmod = include_nullmod,
    err_if_nullmod = err_if_nullmod,
    warn_if_nullmod = warn_if_nullmod,
    n_cores = n_cores
  )
}
