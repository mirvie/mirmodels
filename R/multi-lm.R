#' Fit multiple linear models.
#'
#' For a constant set of explanatory variables (`xs`) and several dependent
#' variables (`ys`), fit a linear model `y ~ xs`  for each `y` in `ys`.
#'
#' @param df A data frame containing explanatory variables `xs` and dependent
#'   variables `ys`.
#' @param xs A character vector. The explanatory variables. These must be the
#'   names of columns of `df` that are either numeric, factor or logical.
#' @param ys A character vector. The dependent variables. These must be the
#'   names of numeric columns of `df` that are either numeric or factors.
#' @param robust A flag. Use robust linear model [MASS::rlm()]? Can only be used
#'   with `type = 1`.
#'
#' @return `r roxy_lm_rlm_list()`
#'
#' @examples
#' if (rlang::is_installed("mirmisc")) {
#'   gars_data <- get_combined_cohort_data(c("ga", "rs"),
#'     gene_predicate = ~ median(.) > 0, log2 = TRUE
#'   ) %>%
#'     dplyr::mutate(
#'       cohort = factor(
#'         dplyr::if_else(startsWith(mirvie_id, "RS"), "RS", "GA")
#'       )
#'     ) %>%
#'     dplyr::filter(!is.na(meta_major_race), !is.na(meta_collectionga))
#'   xs <- c("cohort", "meta_major_race", "meta_collectionga")
#'   ys <- mirmisc::get_df_gene_names(gars_data)
#'   res <- multi_lm(gars_data, xs, ys)
#' }
#' @export
multi_lm <- function(df, xs, ys, robust = TRUE) {
  checked_args <- argchk_multi_lm(df = df, xs = xs, ys = ys, robust = robust)
  df <- checked_args$df[c(xs, ys)]
  lm_func <- ifelse_fun(robust, MASS::rlm, stats::lm)
  df_xs <- df[xs]
  model_dfs <- purrr::imap(
    df[ys], 
    ~dplyr::bind_cols(
      df_xs,
      tibble::enframe(.x, name = NULL, value = .y)
    )
  ) 
  xs_pasted_plus <- paste(paste0("`", xs, "`"), collapse = " + ")
  frmlas <- purrr::map(paste0("`", ys, "`", " ~ ", xs_pasted_plus),
                       stats::as.formula)
  purrr::map2(frmlas, model_dfs, lm_func)
}

#' Get the residuals from multiple linear models.
#'
#' There is the option to add the mean (or median for robust models) to the
#' residuals.
#'
#' @param lms `r roxy_lm_rlm_list()` Most likely, the output of a call to
#'   [multi_lm()].
#' @param lms_data The data frame that was passed as the `df` argument to
#'   [multi_lm()], if [multi_lm()] was used to create `lms`.
#' @param reset_mean_med A flag. If `TRUE`, for each variable for which the
#'   residuals are calculated the mean (or in the case of robust linear models,
#'   the median) of the original values of these variables is added to the
#'   residuals, such that the output is again centred on this value.
#' @param new_data Rather than calculating the residuals on the data where the
#'   model was fit, you can pass a new dataset and calculate the residuals
#'   there.
#'
#' @return A data frame of the residuals. The variables in this data frame will
#'   have their original names. If `lms_data` is given, other columns (ones for
#'   which residuals were not calculated) of that data frame will be returned as
#'   is.
#'
#' @examples
#' if (rlang::is_installed("mirmisc")) {
#'   gars_data <- get_combined_cohort_data(c("ga", "rs"), log2 = TRUE) %>%
#'     dplyr::mutate(
#'       cohort = dplyr::if_else(startsWith(mirvie_id, "RS"), "RS", "GA"),
#'       cohort = factor(cohort),
#'       meta_major_race = forcats::fct_drop(meta_major_race)
#'     ) %>%
#'     dplyr::filter(!is.na(meta_major_race), !is.na(meta_collectionga))
#'   xs <- c("cohort", "meta_major_race", "meta_collectionga")
#'   ys <- gars_data %>%
#'     dplyr::select(dplyr::any_of(mirmisc::get_gene_names())) %>%
#'     purrr::map_dbl(sum) %>%
#'     sort() %>%
#'     tail(100) %>%
#'     names() # ys are the highest expressed 100 genes
#'   lms <- multi_lm(gars_data, xs, ys)
#'   resids <- multi_resids(lms, gars_data, reset_mean_med = TRUE)
#' }
#' @export
multi_resids <- function(lms, lms_data = NULL, reset_mean_med = FALSE,
                         new_data = NULL) {
  argchk_multi_resids(
    lms = lms, lms_data = lms_data,
    reset_mean_med = reset_mean_med, new_data = new_data
  )
  y_names <- lms %>%
    purrr::map_chr(~ as.character(stats::formula(.))[2]) %>%
    strex::str_trim_anything(stringr::coll("`"))
  if (is.null(new_data)) {
    resids <- purrr::map(lms, stats::residuals)
  } else {
    preds <- purrr::map(lms, stats::predict, new_data)
    resids <- purrr::map2(new_data[y_names], preds, `-`)
  }
  if (reset_mean_med) {
    summaryfun <- ifelse_fun(methods::is(lms[[1]], "rlm"), stats::median, mean)
    lms_data_y_summaries <- purrr::map(lms_data[y_names], summaryfun)
    resids <- purrr::map2(resids, lms_data_y_summaries, `+`)
  }
  out <- resids %>%
    magrittr::set_names(y_names) %>%
    dplyr::bind_cols()
  if ((!is.null(lms_data)) || (!is.null(new_data))) {
    if (is.null(new_data)) {
      data <- lms_data
    } else {
      data <- new_data
    }
    out <- dplyr::bind_cols(
      out,
      dplyr::select(data, -dplyr::all_of(names(out)))
    ) %>%
      .[names(data)]
  }
  out
}

#' Get the fitted values from multiple linear models.
#'
#' @inheritParams multi_resids
#'
#' @return A data frame of the fitted values The variables in this data frame
#'   will have their original names. If `lms_data` is given, other columns (ones
#'   for which residuals were not calculated) of that data frame will be
#'   returned as is.
#'
#' @examples
#' if (rlang::is_installed("mirmisc")) {
#'   gars_data <- get_combined_cohort_data(c("ga", "rs"), log2 = TRUE) %>%
#'     dplyr::mutate(
#'       cohort = factor(
#'         dplyr::if_else(startsWith(mirvie_id, "RS"), "RS", "GA")
#'       ),
#'       meta_major_race = forcats::fct_drop(meta_major_race)
#'     ) %>%
#'     dplyr::filter(!is.na(meta_major_race), !is.na(meta_collectionga))
#'   xs <- c("cohort", "meta_major_race", "meta_collectionga")
#'   ys <- gars_data %>%
#'     dplyr::select(dplyr::any_of(mirmisc::get_gene_names())) %>%
#'     purrr::map_dbl(sum) %>%
#'     sort() %>%
#'     tail(100) %>%
#'     names() # ys are the highest expressed 100 genes
#'   lms <- multi_lm(gars_data, xs, ys)
#'   fitteds <- multi_fitteds(lms, gars_data)
#' }
#' @export
multi_fitteds <- function(lms, lms_data = NULL, new_data = NULL) {
  argchk_multi_fitteds(lms = lms, lms_data = lms_data, new_data = new_data)
  y_names <- purrr::map_chr(lms, ~ as.character(stats::formula(.))[2]) %>%
    strex::str_trim_anything(stringr::coll("`"))
  if (is.null(new_data)) {
    fitteds <- purrr::map(lms, stats::fitted)
  } else {
    fitteds <- purrr::map(lms, stats::predict, new_data)
  }
  out <- fitteds %>%
    magrittr::set_names(y_names) %>%
    dplyr::bind_cols()
  if (!is.null(lms_data)) {
    if (is.null(new_data)) {
      data <- lms_data
    } else {
      data <- new_data
    }
    out <- dplyr::bind_cols(
      out,
      dplyr::select(data, -dplyr::all_of(names(out)))
    ) %>%
      .[names(data)]
  }
  out
}

#' Perform multiple analyses of variance on linear model objects.
#'
#' Given a list of linear models (the results of calls to [stats::lm()] or
#' [MASS::rlm()]), all of which used the same explanatory variables with the
#' same dataset (most likely the output of a call to [multi_lm()]), perform
#' analyses of variance on all of them and arrange the result into a single
#' data frame.
#'
#' `lms_data` is needed to ensure that the elements of the `y` column in the
#' return match the input. You should provide this argument if you can at all.
#'
#' @inheritParams multi_resids
#' @param type The type of sum of squares to use. Types I and II are currently
#'   supported. Type I can be used with either default or robust linear models,
#'   but type II cannot be used with robust linear models.
#'
#' @return An object of class `mirmodels_multi_aov_df`. A long data frame
#'   with columns:
#'   * `y`: The name of the response variable
#'   * `x`: The name of the explanatory variable.
#'   * `pctvarexp`: The percent of variance explained by `x`.
#'   * `pval`: The p-value for x explaining a non-zero amount of `y`'s variance.
#'
#' @family multiple linear modelling functions
#'
#' @examples
#' if (requireNamespace("mirmisc")) {
#'   gars_data <- get_combined_cohort_data(c("ga", "rs"), log2 = TRUE) %>%
#'     dplyr::mutate(
#'       cohort = factor(
#'         dplyr::if_else(startsWith(mirvie_id, "RS"), "RS", "GA")
#'       ),
#'       meta_major_race = forcats::fct_drop(meta_major_race)
#'     ) %>%
#'     dplyr::filter(!is.na(meta_collectionga), !is.na(meta_major_race))
#'   xs <- c("cohort", "meta_major_race", "meta_collectionga")
#'   ys <- gars_data %>%
#'     dplyr::select(dplyr::any_of(mirmisc::get_gene_names())) %>%
#'     purrr::map_dbl(sum) %>%
#'     sort() %>%
#'     tail(100) %>%
#'     names() # ys are the highest expressed 100 genes
#'   lms <- multi_lm(gars_data, xs, ys, robust = TRUE)
#'   aovs <- multi_aov(lms)
#'   summary(aovs)
#' }
#' @export
multi_aov <- function(lms, lms_data = NULL, type = 1) {
  checkmate::assert_list(lms, types = "lm")
  checkmate::assert_scalar(type)
  type <- ifelse(isTRUE(type == "I"), 1, type)
  type <- ifelse(isTRUE(type == "II"), 2, type)
  type <- checkmate::assert_int(as.numeric(type),
    lower = 1, upper = 2,
    na.ok = FALSE, coerce = TRUE
  )
  if (type == 2 && "rlm" %in% unlist(purrr::map(lms, class))) {
    custom_stop("Cannot perform type II ANOVA on a robust linear model.")
  }
  ys <- lms %>%
    purrr::map(stats::formula) %>%
    purrr::map(as.character) %>%
    purrr::map_chr(2)
  if (type == 1) {
    aovs <- purrr::map(lms, stats::aov)
  } else {
    aovs <- purrr::map(lms, car::Anova, type = type)
  }
  tidied_aov_dfs <- purrr::map(aovs, broom::tidy)
  tidied_aov_dfs <- purrr::map2(
    tidied_aov_dfs, ys,
    function(x, y) {
      x %>%
        dplyr::mutate(pctvarexp = round(100 * sumsq / sum(sumsq), 1)) %>%
        dplyr::bind_cols(y = y)
    }
  )
  out <- dplyr::bind_rows(tidied_aov_dfs) %>%
    dplyr::arrange(y, dplyr::desc(pctvarexp)) %>%
    dplyr::transmute(y = y, x = term, pctvarexp = pctvarexp, pval = p.value)
  class(out) <- c("mirmodels_multi_aov_df", class(out))
  out
}

#' Summarize the output of [multi_aov()].
#'
#' Group by `x` and get the mean and median percent variance explained for all
#' of the `y`s.
#'
#' @param object The output of [multi_aov()].
#' @param ... Optional. Positive integers. Quantiles of variance explained.
#'   See example in [multi_aov()].
#' @param na_rm A flag. Remove `NA`s in the summary calculations?
#'
#' @return A data frame.
#'
#' @export
summary.mirmodels_multi_aov_df <- function(object, ..., na_rm = TRUE) {
  checkmate::assert_class(object, "mirmodels_multi_aov_df")
  object_grouped <- dplyr::group_by(object, x)
  out <- object_grouped %>%
    dplyr::summarise(
      med_pctvarexp = stats::median(pctvarexp, na.rm = na_rm),
      mean_pctvarexp = mean(pctvarexp, na.rm = na_rm),
      max_pctvarexp = max(pctvarexp, na.rm = na_rm),
      .groups = "drop"
    )
  pctls <- unlist(list(...))
  if (length(pctls)) {
    if (!isTRUE(checkmate::check_numeric(pctls, lower = 0, upper = 1))) {
      custom_stop(
        "Arguments passed in ... must all be numbers between 0 and 1."
      )
    }
    out_more <- list()
    for (pctl in pctls) {
      out_more[[length(out_more) + 1]] <- object_grouped %>%
        dplyr::summarise(
          "pctl{round(100 * pctl, 1)}_pctvarexp" := quantile(
            pctvarexp, pctl,
            na.rm = na_rm
          ),
          .groups = "drop"
        ) %>%
        .[2]
    }
    out <- dplyr::bind_cols(out, out_more)
  }
  out %>%
    dplyr::mutate_if(is.numeric, round, 1) %>%
    dplyr::arrange(dplyr::desc(med_pctvarexp))
}
