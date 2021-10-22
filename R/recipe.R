#' Create the recipe appropriate for tidymodels XGBoost models.
#'
#' For XGBoost, everything needs to be numeric. The recipe should process
#' numbers, factors, characters and logicals.
#'
#' @inheritParams train_gbm
#'
#' @return A [recipes::recipe()].
#'
#' @noRd
create_xgb_recipe <- function(training_data, outcome, id_col = NULL) {
  checkmate::assert_data_frame(training_data, min.rows = 2, min.cols = 2)
  checkmate::assert_string(outcome)
  checkmate::assert_string(id_col, null.ok = TRUE)
  checkmate::assert_names(names(training_data), must.include = outcome)
  if (!is.null(id_col)) {
    checkmate::assert_names(names(training_data), must.include = id_col)
  }
  mod_rec <- recipes::recipe(training_data) %>%
    recipes::update_role(dplyr::everything(), new_role = "predictor") %>%
    recipes::update_role(dplyr::one_of(outcome),
      new_role = "outcome"
    )
  if (!is.null(id_col)) {
    mod_rec <- recipes::update_role(mod_rec,
      dplyr::one_of(id_col),
      new_role = "ID"
    )
  }
  mod_rec_summary <- summary(mod_rec)
  n_logical_predictors <- mod_rec_summary %>%
    dplyr::filter(type == "logical", role == "predictor") %>%
    nrow()
  if (n_logical_predictors) {
    mod_rec <- recipes::step_mutate_at(
      mod_rec,
      recipes::has_type("logical"), -recipes::all_outcomes(),
      fn = as.numeric
    )
  }
  n_nominal_predictors <- mod_rec_summary %>%
    dplyr::filter(type == "nominal", role == "predictor") %>%
    nrow()
  if (n_nominal_predictors) {
    mod_rec <- recipes::step_factor2string(
      mod_rec,
      recipes::all_predictors(), -recipes::all_numeric()
    ) %>%
      recipes::step_mutate_at(
        recipes::all_nominal(), -recipes::all_outcomes(),
        -recipes::has_role("ID"),
        fn = ~ dplyr::if_else(
          stringr::str_to_lower(stringr::str_trim(.)) == "unknown",
          NA_character_, .
        )
      ) %>%
      recipes::step_string2factor(
        recipes::all_nominal(),
        -recipes::all_outcomes(), -recipes::has_role("ID")
      ) %>%
      recipes::step_unknown(
        recipes::all_nominal(),
        -recipes::all_outcomes(), -recipes::has_role("ID")
      ) %>%
      recipes::step_other(recipes::all_predictors(), -recipes::all_numeric(),
        threshold = 0.1, other = "infrequent_other"
      ) %>%
      recipes::step_dummy(recipes::all_predictors(), -recipes::all_numeric())
  }
  mod_rec <- recipes::step_zv(mod_rec, recipes::all_predictors())
  mod_rec
}

#' Create the recipe appropriate for tidymodels multinomial regression models.
#'
#' Everything needs to be numeric. The recipe should process numbers, factors,
#' characters and logicals. This is the same as is needed for XGBoost, except
#' that predictors should be normalized.
#'
#' @inheritParams create_xgb_recipe
#'
#' @return A [recipes::recipe()].
#'
#' @noRd
create_glm_recipe <- function(training_data, outcome,
                              id_col = NULL, na_action = "medianimpute") {
  na_action <- strex::match_arg(
    na_action,
    c("medianimpute", "knnimpute")
  )
  out <- create_xgb_recipe(
    training_data = training_data,
    outcome = outcome,
    id_col = id_col
  ) %>%
    recipes::step_normalize(recipes::all_predictors())
  switch(na_action,
    medianimpute = recipes::step_medianimpute(
      out,
      recipes::all_predictors()
    ),
    knnimpute = recipes::step_knnimpute(
      out,
      recipes::all_predictors()
    )
  )
}

create_lm_recipe <- create_glm_recipe
