#' Is a string a valid yardstick metric name?
#'
#' @param x A string.
#'
#' @return A flag with an attribute called `cleaned_metric_name` with the
#'   cleaned metric name (removes `_vec` suffix).
#'
#' @noRd
is_yardstick_metric_name <- function(x) {
  checkmate::assert_string(x, min.chars = 1)
  if (stringr::str_detect(x, stringr::coll("("))) {
    x <- strex::str_before_first(x, stringr::coll("("))
  }
  x <- stringr::str_replace(x, "_vec$", "")
  tryx <- try(eval(parse(text = glue::glue("yardstick::{x}"))),
    silent = TRUE
  )
  structure(methods::is(tryx, "metric"),
    cleaned_metric_name = x
  )
}

#' Assert that elements of a character vector are valid yardstick metric names.
#'
#' @param x A character vector of possible metric names.
#'
#' @return `TRUE` (invisibly) if all elements are valid yardstick metric names.
#'   Otherwise, an informative error is thrown. If `TRUE`, there is an attribute
#'   `cleaned_metric_names` with the cleaned metric name (removes `_vec`
#'   suffix).
#'
#' @noRd
assert_yardstick_metric_names <- function(x) {
  checkmate::assert_character(x,
    min.len = 1, min.chars = 1,
    any.missing = FALSE
  )
  good <- purrr::map(x, is_yardstick_metric_name)
  if (!all(unlist(good))) {
    custom_stop("'{x[which.min(good)]}' is not a valid yardstick metric name.")
  }
  invisible(
    structure(
      TRUE,
      cleaned_metric_names = purrr::map_chr(good, attr, "cleaned_metric_name")
    )
  )
}

#' Convert a character vector of yardstick metric names to a metric set.
#'
#' @param x A character vector of yardstick metric names.
#'
#' @return A [yardstick::metric_set].
#'
#' @noRd
chr_vec_to_metric_set <- function(x) {
  valid_names <- assert_yardstick_metric_names(x)
  glue::glue("yardstick::{attr(valid_names, 'cleaned_metric_names')}",
    .envir = list(valid_names = valid_names)
  ) %>%
    stringr::str_c(collapse = ", ") %>%
    stringr::str_c("yardstick::metric_set(", ., ")") %>%
    parse(text = .) %>%
    eval()
}

#' Get the direction of a metric.
#'
#' The metric must be a metric in the `yardstick` package. Examples are `rmse`,
#' `mae`, `accuracy`, `mn_log_loss`.
#'
#' @param x A string or a [yardstick::metric_set].
#'
#' @return A string. `"maximize"` or `"minimize"`. For example, accuracy is a
#'   maximize metric but log loss is a minimize metric. If the input has more
#'   than one metric, the direction of the first metric is returned.
#'
#' @noRd
get_metric_direction <- function(x) {
  if (is.character(x)) {
    checkmate::assert_character(x, min.chars = 1)
    x <- "yardstick::metric_set(yardstick::{x[1]})" %>%
      stringr::str_glue() %>%
      parse(text = .) %>%
      eval()
  }
  x %>%
    attr("metrics") %>%
    .[[1]] %>%
    attr("direction")
}

#' Get the type of a metric.
#'
#' The metric must be a metric in the `yardstick` package. Examples are `rmse`,
#' `mae`, `accuracy`, `mn_log_loss`.
#'
#' @param x A string or a [yardstick::metric_set].
#'
#' @return A string. `"numeric"` or `"probability"`. For example, RMSE is a
#'   numeric metric but log loss is a probability metric.
#'
#' @noRd
get_metric_type <- function(x) {
  if (is.character(x)) {
    checkmate::assert_scalar(x)
    x <- "yardstick::metric_set(yardstick::{x})" %>%
      stringr::str_glue() %>%
      parse(text = .) %>%
      eval()
  }
  if (methods::is(x, "numeric_metric_set")) {
    return("numeric")
  }
  if (methods::is(x, "class_prob_metric_set")) {
    return("probability")
  }
  custom_stop("Could not determine metric type.") # should never happen
}

#' Get the names of the metrics in a metric set.
#'
#' @param ms A [yardstick::metric_set].
#'
#' @return A character vector.
#'
#' @noRd
get_metric_names_from_set <- function(ms) {
  checkmate::assert_class(ms, "metric_set")
  names(attr(ms, "metrics"))
}

#' Get the fold ids from a [rsample::vfold_cv()] for [glmnet::cv.glmnet()].
#'
#' The result will be the argument `foldid` in [glmnet::cv.glmnet()].
#'
#' @param cv_resamp An object of class `vfold_cv`. The output of a call to
#'   [rsample::vfold_cv].
#'
#' @return A numeric vector.
#'
#' @noRd
get_fold_ids <- function(cv_resamp) {
  checkmate::assert_class(cv_resamp, c("vfold_cv"))
  checkmate::assert_data_frame(cv_resamp, min.rows = 1, min.cols = 1)
  checkmate::assert_true(attr(cv_resamp, "repeats") == 1)
  checkmate::assert_names("splits", subset.of = names(cv_resamp))
  folds_which <- purrr::map(cv_resamp$splits, rsample::complement)
  data_nrow <- dplyr::n_distinct(unlist(folds_which))
  purrr::imap(
    folds_which,
    function(x, y) {
      out <- rep(0, data_nrow)
      out[x] <- y
      out
    }
  ) %>%
    purrr::reduce(`+`)
}

#' Get the current seed.
#'
#' This function doesn't actually retrieve the current seed but it behaves
#' exactly like you'd want such a function to. See
#' http://rorynolan.rbind.io/2018/09/30/rcsetseed/.
#'
#' @return An int.
#'
#' @noRd
get_seed <- function() {
  sample.int(.Machine$integer.max, 1)
}

#' Construct an informative error bullet for a data frame containing NAs.
#'
#' This function assumes that a data frame contains an `NA` and tells you where
#' the first `NA` is.
#'
#' @param df
#'
#' @return A string.
#'
#' @noRd
construct_df_na_err_msg <- function(df) {
  checkmate::assert_data_frame(df, col.names = "named")
  if (!anyNA(df)) rlang::abort("`df` must contain an `NA`.")
  offending_column <- character(1)
  for (n in names(df)) {
    if (anyNA(df[[n]])) offending_column <- n
  }
  row_num <- which.max(is.na(df[[offending_column]]))
  stringr::str_glue("The column named '{offending_column}' has an ",
    "{usethis::ui_code('NA')} in row number {row_num}.",
    .envir = list(
      offending_column = offending_column,
      row_num = row_num
    )
  )
}

#' Convert a list of `training_data` and `testing_data` to an `rsplit`.
#'
#' A lot of functions in this package return lists with two elements  that are
#' data frames called `training_data` and `testing_data`. You might prefer these
#' to be in the form of an `rsplit` object, such as the return of
#' [rsample::initial_split()] so that you can access the training and testing
#' parts with [rsample::training()] and [rsample::testing()].
#'
#' @param traintest_lst A list with two elements `training_data` and
#'   `testing_data` which are data frames with the same column names.
#'
#' @return An `rsplit` object. Something like the return of
#'   [rsample::initial_split()].
#'
#' @export
conv_traintest_lst_to_rsplit <- function(traintest_lst) {
  checkmate::assert_list(traintest_lst, len = 2, names = "named")
  checkmate::assert_names(names(traintest_lst),
    permutation.of = c("training_data", "testing_data")
  )
  checkmate::assert_data_frame(traintest_lst$training_data, min.rows = 1)
  checkmate::assert_data_frame(traintest_lst$testing_data, min.rows = 1)
  checkmate::assert_names(names(traintest_lst$training_data),
    identical.to = names(traintest_lst$testing_data)
  )
  data <- dplyr::bind_rows(
    traintest_lst$training_data,
    traintest_lst$testing_data
  )
  ind <- list(
    analysis = seq_len(nrow(traintest_lst$training_data)),
    assessment = nrow(traintest_lst$training_data) +
      seq_len(nrow(traintest_lst$testing_data))
  )
  rsample::make_splits(ind = ind, data = data)
}

ifelse_fun <- function(flag, fun1, fun2) {
  checkmate::assert_flag(flag)
  checkmate::assert_function(fun1)
  checkmate::assert_function(fun2)
  if (flag) {
    return(fun1)
  }
  fun2
}

df_fct_drop <- function(df) {
  checkmate::assert_data_frame(df)
  purrr::map_dfc(
    df,
    function(x) {
      if (is.factor(x)) {
        forcats::fct_drop(x)
      } else {
        x
      }
    }
  )
}

vecsum1 <- function(x) {
  checkmate::assert_numeric(x, min.len = 1)
  x / sum(x)
}

insist_mirmisc <- function() {
  if (!rlang::is_installed("mirmisc")) {
    custom_stop(
      "Package 'mirmisc' is required."
    )
  }
  invisible(TRUE)
}
