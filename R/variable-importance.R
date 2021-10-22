#' Get variable importances from a model or list of models.
#'
#' This is a wrapper around [vip::vi()]. It returns variables in descending
#' order of importance. For a list of models, it assumes that the models have
#' all modelled the same problem with the same variables and the variables are
#' summarized by median importance.
#'
#' @param mod A `workflow` or `model_fit` object, or a list thereof.
#'
#' @return A tibble.
#'
#' @examples
#' iris_data <- janitor::clean_names(datasets::iris)
#' iris_data_split <- rsample::initial_split(iris_data, strata = species)
#' mod <- train_glm(
#'   training_data = rsample::training(iris_data_split),
#'   outcome = "species",
#'   metric = "mn_log_loss",
#'   cv_nfolds = 5,
#'   n_cores = 5
#' )
#' vimp(mod)
#' vimp(list(mod, mod))
#' @export
vimp <- function(mod) {
  if (methods::is(mod, "workflow")) {
    mod <- workflows::pull_workflow_fit(mod)
  }
  if (methods::is(mod, "_nullmod")) {
    return(
      dplyr::tibble(
        rank = integer(),
        variable = character(),
        importance = double()
      )
    )
  }
  has_predict <- FALSE
  if (length(class(mod))) {
    all_methods <- purrr::map(class(mod), ~ utils::methods(class = .)) %>%
      unlist()
    has_predict <- any(stringr::str_detect(all_methods, "^predict\\..+"))
  }
  if (has_predict) {
    out <- vip::vi(mod) %>%
      dplyr::transmute(variable = Variable, importance = abs(Importance))
  } else if (is.list(mod)) {
    outs <- mod %>%
      purrr::map(vimp) %>%
      purrr::map(~ select(., -rank))
    vars <- outs %>%
      dplyr::bind_rows() %>%
      dplyr::pull(variable) %>%
      unique()
    zeroes <- dplyr::tibble(variable = vars, importance = 0)
    out <- outs %>%
      purrr::keep(~ as.logical(nrow(.))) %>%
      purrr::map(dplyr::bind_rows, zeroes) %>%
      purrr::map(~ dplyr::group_by(., variable)) %>%
      purrr::map(
        ~ dplyr::summarise(., importance = max(importance), .groups = "drop")
      ) %>%
      dplyr::bind_rows() %>%
      dplyr::group_by(variable) %>%
      dplyr::summarise(
        importance = stats::median(importance),
        .groups = "drop"
      )
  } else {
    custom_stop("Argument `mod` does not appear to be a model or list of
                 models.")
  }
  out <- out %>%
    dplyr::arrange(dplyr::desc(importance), variable) %>%
    dplyr::bind_cols(rank = seq_len(nrow(.)), .)
  if (methods::is(out, "vi")) {
    class(out) <- dplyr::setdiff(class(out), "vi")
    att_names <- names(attributes(out))
    attributes(out) <- attributes(out)[dplyr::setdiff(att_names, "type")]
  }
  out
}
