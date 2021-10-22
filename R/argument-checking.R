argchk_learn_curve <- function(model_evaluate, training_data, outcome,
                               testing_data, testing_frac,
                               training_fracs, repeats,
                               strata, n_cores) {
  checkmate::assert_function(model_evaluate,
    nargs = 2,
    args = c("training_data", "testing_data")
  )
  checkmate::assert_data_frame(training_data, min.rows = 2, min.cols = 1)
  checkmate::assert_string(outcome)
  checkmate::assert_data_frame(testing_data,
    min.rows = 2, min.cols = 1,
    null.ok = TRUE
  )
  if (is.null(testing_data)) {
    if (is.null(testing_frac)) {
      custom_stop(
        "
        {code('testing_data')} and {code('testing_frac')}
        can't both be {code('NULL')}.
        "
      )
    }
  } else {
    checkmate::assert_subset(names(training_data), names(testing_data))
    if (!is.null(testing_frac)) {
      custom_stop(
        "
      If you specify {code('testing_data')}, then
      {code('testing_frac')} must be
      {code('NULL')}.
      "
      )
    }
    if (repeats > 1) {
      custom_stop(
        "
      If {code('repeats')} is greater than 1, then
      {code('testing_data')} must be
      {code('NULL')}.
      "
      )
    }
  }
  checkmate::assert_numeric(testing_frac,
    lower = 0, upper = 1,
    null.ok = TRUE, unique = TRUE, sorted = TRUE
  )
  if ((!is.null(testing_frac)) &&
    (!(all(testing_frac > 0) && all(testing_frac < 1)))) {
    custom_stop(
      "
      All of the elements in {code('testing_frac')} must be
      between 0 and 1 (not inclusive).
      "
    )
  }
  checkmate::assert_numeric(training_fracs,
    lower = 0, upper = 1,
    unique = TRUE, sorted = TRUE
  )
  checkmate::assert_count(repeats)
  checkmate::assert_string(strata, min.chars = 1, null.ok = TRUE)
  if (!is.null(strata)) {
    checkmate::assert_names(strata, subset.of = names(training_data))
  }
  checkmate::assert_count(n_cores)
  invisible(TRUE)
}

argchk_autoplot_mlc <- function(object, metric, smooth, meansd) {
  verify_mirvie_learning_curve(object)
  checkmate::assert_string(metric, null.ok = TRUE)
  checkmate::assert_flag(smooth)
  checkmate::assert_flag(meansd)
  n_repeats <- max(object$rep)
  if (n_repeats == 1 && meansd) {
    custom_stop(
      "
      You can only use {code('meansd = TRUE')} if the number of repeats in
      your learning curve is greater than 1.
      "
    )
  }
  invisible(TRUE)
}

argchk_train_on_grid <- function(mod_spec, hyper_param_grid, mod_rec,
                                 training_data, outcome, cv_nfolds, cv_nreps,
                                 strata, id_col, metric, selection_method,
                                 simplicity_params, include_nullmod,
                                 err_if_nullmod, warn_if_nullmod,
                                 n_cores) {
  checkmate::assert_class(mod_spec, "model_spec")
  checkmate::assert_string(mod_spec$mode)
  checkmate::assert_subset(mod_spec$mode, c("classification", "regression"))
  checkmate::assert(
    checkmate::check_list(hyper_param_grid),
    checkmate::check_data_frame(hyper_param_grid)
  )
  checkmate::assert_subset(names(hyper_param_grid), names(mod_spec$args))
  if ((is.list(hyper_param_grid)) && (!is.data.frame(hyper_param_grid))) {
    hyper_param_grid <- do.call(tidyr::expand_grid, hyper_param_grid)
  }
  checkmate::assert_data_frame(training_data, min.rows = 2)
  checkmate::assert_string(outcome)
  checkmate::assert_subset(outcome, names(training_data))
  checkmate::assert_int(cv_nfolds, lower = 2)
  checkmate::assert_int(cv_nreps, lower = 1)
  checkmate::assert_character(strata, min.len = 1, null.ok = TRUE)
  if (!is.null(strata)) checkmate::assert_subset(strata, names(training_data))
  checkmate::assert_string(id_col, null.ok = TRUE, min.chars = 1)
  assert_yardstick_metric_names(metric)
  checkmate::assert_string(selection_method)
  selection_method <- strex::match_arg(selection_method,
    c("Breiman", "absolute"),
    ignore_case = TRUE
  )
  checkmate::assert_character(simplicity_params, min.len = 1, null.ok = TRUE)
  if (selection_method == "Breiman") {
    if (is.null(simplicity_params)) {
      custom_stop(
        "
        When using {code('selection_method = \"Breiman\"')},
        you must specify at least one tuning parameter in
        {code('simplicity_params')} to sort the tuning results by
        simplicity.
        "
      )
    }
    simplicity_in_hyper <- purrr::map_lgl(
      simplicity_params,
      function(x) {
        if (stringr::str_starts(x, "-")) x <- stringr::str_sub(x, 2, -1)
        x %in% names(hyper_param_grid)
      }
    )
    if (!all(simplicity_in_hyper)) {
      custom_stop(
        "
        All variables referred to in {code('simplicity_params')}
        must be columns in {code('hyper_param_grid')}.
        ",
        "
        {code(simplicity_params[match(FALSE, simplicity_in_hyper)])}
        does not refer to a variable in {code('hyper_param_grid')}.
        "
      )
    }
  }
  checkmate::assert_flag(include_nullmod)
  checkmate::assert_flag(err_if_nullmod)
  checkmate::assert_flag(warn_if_nullmod)
  checkmate::assert_integerish(n_cores,
    lower = 1,
    upper = parallel::detectCores()
  )
  invisible(TRUE)
}

argchk_train_gbm <- function(training_data, id_col, outcome,
                             selection_method, n_cores) {
  checkmate::assert_string(id_col, null.ok = TRUE, min.chars = 1)
  checkmate::assert_data_frame(training_data, min.rows = 2, min.cols = 2)
  good_types <- purrr::map_lgl(
    training_data,
    ~ is.numeric(.) || is.factor(.) || is.character(.) || is.logical(.)
  )
  if (!all(good_types)) {
    custom_stop(
      "
      All elements of `training_data` must be of numeric, factor, character
      or logical type.
      ",
      "The column `{names(training_data)[match(FALSE, good_types)]}` is not."
    )
  }
  checkmate::assert_string(outcome)
  checkmate::assert_subset(outcome, names(training_data))
  outcome_vec <- training_data[[outcome]]
  if (is.logical(outcome_vec)) {
    custom_stop("Your outcome should not be of logical (boolean) type.")
  }
  n_distinct_outcomes <- length(unique(outcome_vec))
  if (n_distinct_outcomes < 2) {
    custom_stop("Your outcome must have at least two distinct values.")
  }
  if (anyNA(outcome_vec)) {
    custom_stop("Your outcome must not contain {code('NA')}s.")
  }
  selection_method <- strex::match_arg(selection_method,
    c("Breiman", "absolute"),
    ignore_case = TRUE
  )
  checkmate::assert_integerish(n_cores,
    lower = 1,
    upper = parallel::detectCores()
  )
  list(selection_method = selection_method)
}

argchk_train_glm <- function(training_data, id_col, outcome, metric,
                             selection_method, cv_nfolds,
                             n_cores = n_cores, na_action = na_action) {
  selection_method <- argchk_train_gbm(
    training_data = training_data,
    id_col = id_col,
    outcome = outcome,
    selection_method = selection_method,
    n_cores = n_cores
  )[["selection_method"]]
  assert_yardstick_metric_names(metric)
  checkmate::assert(
    checkmate::check_character(training_data[[outcome]]),
    checkmate::check_factor(training_data[[outcome]])
  )
  checkmate::assert_int(cv_nfolds, lower = 4)
  outcome_jantab <- janitor::tabyl(training_data[[outcome]])
  if (any(outcome_jantab$n < 5)) {
    first_bad <- dplyr::filter(outcome_jantab, n < 5)
    custom_stop(
      "Each multinomial class must have at least 5 instances.",
      "'{first_bad[[1, 1]]}' has {first_bad[[1, 'n']]}.",
      .envir = list(first_bad = first_bad)
    )
  }
  na_action <- strex::str_match_arg(na_action, c("medianimpute", "knnimpute"),
    ignore_case = TRUE
  )
  list(
    selection_method = selection_method,
    na_action = na_action
  )
}

argchk_train_lm <- function(training_data, id_col, outcome, metric, cv_nfolds,
                            selection_method, na_action, n_cores) {
  selection_method <- argchk_train_gbm(
    training_data = training_data,
    id_col = id_col,
    outcome = outcome,
    selection_method = selection_method,
    n_cores = n_cores
  )[["selection_method"]]
  assert_yardstick_metric_names(metric)
  metric <- strex::match_arg(metric, c("rmse", "mae"), ignore_case = TRUE)
  checkmate::assert_int(cv_nfolds, lower = 4)
  checkmate::assert_data_frame(training_data, min.cols = 3, min.rows = 5)
  na_action <- strex::match_arg(na_action, c("medianimpute", "knnimpute"),
    ignore_case = TRUE
  )
  checkmate::assert_numeric(training_data[[outcome]])
  list(
    metric = metric, selection_method = selection_method,
    na_action = na_action
  )
}

argchk_multi_lm <- function(df, xs, ys, robust = FALSE) {
  checkmate::assert_data_frame(df, min.rows = 2, min.cols = 2)
  checkmate::assert_character(xs, min.len = 1, max.len = 999)
  checkmate::assert_character(ys, min.len = 1)
  checkmate::assert_names(xs, subset.of = names(df))
  checkmate::assert_names(ys, subset.of = names(df))
  checkmate::assert_data_frame(df[xs],
    types = c("factor", "numeric", "logical")
  )
  checkmate::assert_data_frame(df[ys], types = c("numeric"))
  checkmate::assert_flag(robust)
  if (anyNA(df[xs])) {
    custom_stop(
      "{code('df[xs]')} should not contain {code('NA')}s.",
      construct_df_na_err_msg(df[xs])
    )
  } else if (anyNA(df[ys])) {
    custom_stop(
      "{code('df[ys]')} should not contain {code('NA')}s.",
      construct_df_na_err_msg(df[ys])
    )
  }
  df <- df_fct_drop(df)
  list(df = df, xs = xs, robust = robust)
}

argchk_multi_fitteds <- function(lms, lms_data, new_data) {
  checkmate::assert_list(lms, type = "lm", min.len = 1)
  checkmate::assert_data_frame(lms_data,
    min.rows = 2, min.cols = 2,
    null.ok = TRUE
  )
  if (!is.null(new_data)) {
    checkmate::assert_data_frame(new_data, min.rows = 2, min.cols = 2)
    checkmate::assert_names(names(new_data),
      identical.to = names(lms_data)
    )
    checkmate::assert_names(
      purrr::map_chr(new_data, typeof),
      identical.to = purrr::map_chr(lms_data, typeof)
    )
  }
  invisible(TRUE)
}

argchk_multi_resids <- function(lms, lms_data, reset_mean_med, new_data) {
  checkmate::assert_flag(reset_mean_med)
  if (is.null(lms_data) && reset_mean_med) {
    custom_stop("To use {code('reset_mean_med = TRUE')}, {code('lms_data')}
                 cannot be {code('NULL')}.")
  }
  argchk_multi_fitteds(lms = lms, lms_data = lms_data, new_data = new_data)
}

argchk_linear_correct <- function(training_data, testing_data,
                                  correct_cols, correct_for_cols,
                                  keep_effect_cols, robust) {
  checkmate::assert_flag(robust)
  if (methods::is(training_data, "rsplit")) {
    testing_data <- rsample::testing(training_data)
    training_data <- rsample::training(training_data)
  }
  checkmate::assert_data_frame(training_data, min.rows = 2, min.cols = 2)
  checkmate::assert_data_frame(testing_data,
    null.ok = TRUE,
    min.rows = 2, min.cols = 2
  )
  checkmate::assert_character(correct_for_cols, min.len = 1, min.chars = 1)
  checkmate::assert_names(correct_for_cols, subset.of = names(training_data))
  if (!is.null(testing_data)) {
    checkmate::assert_names(names(training_data),
      identical.to = names(testing_data)
    )
    checkmate::assert_names(
      purrr::map_chr(training_data, typeof),
      identical.to = purrr::map_chr(testing_data, typeof)
    )
  }
  checkmate::assert_names(correct_for_cols, subset.of = names(training_data))
  checkmate::assert_character(correct_cols,
    min.len = 1, min.chars = 1, unique = TRUE
  )
  checkmate::assert_disjunct(correct_cols, correct_for_cols)
  checkmate::assert_data_frame(training_data[correct_for_cols],
    types = c("numeric", "factor", "logical"),
    any.missing = FALSE
  )
  checkmate::assert_data_frame(testing_data[correct_for_cols],
    types = c("numeric", "factor", "logical"),
    any.missing = FALSE, null.ok = TRUE
  )
  if (!is.null(keep_effect_cols)) {
    checkmate::assert_character(keep_effect_cols, min.chars = 1, min.len = 1)
    checkmate::assert_names(keep_effect_cols, subset.of = names(training_data))
    checkmate::assert_disjunct(keep_effect_cols, correct_cols)
    checkmate::assert_disjunct(keep_effect_cols, correct_for_cols)
    checkmate::assert_data_frame(training_data[keep_effect_cols],
      types = c("numeric", "factor", "logical"),
      any.missing = FALSE
    )
    checkmate::assert_data_frame(testing_data[keep_effect_cols],
      types = c("numeric", "factor", "logical"),
      any.missing = FALSE, null.ok = TRUE
    )
    keep_effect_keep <- training_data[keep_effect_cols] %>%
      purrr::map_lgl(~ dplyr::n_distinct(.) > 1)
    keep_effect_cols <- keep_effect_cols[keep_effect_keep]
  }
  checkmate::assert_data_frame(training_data[correct_cols],
    type = "numeric",
    any.missing = FALSE
  )
  list(
    training_data = training_data,
    testing_data = testing_data,
    correct_cols = correct_cols,
    correct_for_cols = correct_for_cols,
    keep_effect_cols = keep_effect_cols,
    robust = robust
  )
}

argchk_cvg_lambda_help <- function(prepped_rec, outcome, metric,
                                   selection_method,
                                   foldid, lambda_user, n_cores) {
  checkmate::assert_class(prepped_rec, "recipe")
  assert_yardstick_metric_names(metric)
  checkmate::assert_string(selection_method)
  checkmate::assert_names(selection_method,
    subset.of = c("Breiman", "absolute")
  )
  checkmate::assert_numeric(lambda_user, lower = 0, min.len = 1, null.ok = TRUE)
  juiced_rec <- recipes::juice(prepped_rec)
  y <- juiced_rec[[outcome]]
  fam <- dplyr::case_when(
    is.numeric(y) ~ "gaussian",
    dplyr::n_distinct(y) == 2 ~ "binomial",
    dplyr::n_distinct(y) > 2 ~ "multinomial"
  )
  glmnet_x_mat_cols <- summary(prepped_rec) %>%
    dplyr::filter(role == "predictor") %>%
    dplyr::pull(variable)
  x <- as.matrix(juiced_rec[glmnet_x_mat_cols])
  list(juiced_rec = juiced_rec, x = x, y = y, fam = fam, metric = metric)
}

argchk_plot_roc <- function(rocdf, conf_level, aes_opts, quick, parallel) {
  checkmate::assert_data_frame(rocdf, min.cols = 2, min.rows = 1)
  checkmate::assert_subset(c("response", "predictor"), names(rocdf))
  checkmate::assert_integerish(rocdf$response,
    lower = 0, upper = 1, any.missing = FALSE
  )
  checkmate::assert_numeric(rocdf$predictor,
    lower = 0, upper = 1, any.missing = FALSE
  )
  if ("model" %in% names(rocdf)) {
    checkmate::assert(
      checkmate::check_character(rocdf$model),
      checkmate::check_factor(rocdf$model)
    )
  }
  checkmate::assert_list(aes_opts)
  checkmate::assert_names(
    names(aes_opts),
    subset.of = c(
      "conf_alpha", "roc_line_size", "roc_line_color",
      "null_line_size", "null_line_color", "null_line_alpha"
    )
  )
  checkmate::assert_number(aes_opts$conf_alpha, lower = 0, upper = 1)
  checkmate::assert_number(aes_opts$roc_line_size, lower = 0)
  checkmate::assert_string(aes_opts$roc_line_color)
  checkmate::assert_number(aes_opts$null_line_size, lower = 0)
  checkmate::assert_string(aes_opts$null_line_color)
  checkmate::assert_number(aes_opts$null_line_alpha, lower = 0, upper = 1)
  checkmate::assert_flag(quick)
  checkmate::assert_flag(parallel)
  invisible(TRUE)
}

argchk_compute_pcas <- function(df, num_comp, subset, normalize,
                                robust, crit_pca_distances) {
  checkmate::assert_data_frame(df, min.rows = 2, min.cols = 2)
  num_comp <- checkmate::assert_int(num_comp, coerce = TRUE)
  checkmate::assert_character(subset, min.chars = 1, null.ok = TRUE)
  df_subset <- df
  if (!is.null(subset)) {
    checkmate::assert_names(subset, subset.of = names(df))
    df_subset <- dplyr::select(df, dplyr::all_of(subset))
  }
  max_allowed_comp <- dplyr::if_else(
    is.null(subset), min(dim(df)),
    min(nrow(df), length(subset))
  ) - 1
  if (num_comp > max_allowed_comp) {
    custom_stop("With this data, the maximum number of components you can
                 have is {max_allowed_comp}.")
  }
  checkmate::assert_flag(normalize)
  checkmate::assert_flag(robust)
  checkmate::assert_number(crit_pca_distances, lower = 0, upper = 1)
  if (!all(purrr::map_lgl(df_subset, is.numeric))) {
    custom_stop(
      "The columns for PCA calculation must all be numeric.",
      "Select a numeric {code('subset')}."
    )
  }
  list(
    df = df, num_comp = num_comp, subset = subset, normalize = normalize,
    robust = robust, crit_pca_distances = crit_pca_distances,
    df_subset = df_subset
  )
}

argchk_deseq <- function(data, condition, genes, batch,
                         shrink, padj_method, quick,
                         n_cores, quiet) {
  insist_mirmisc()
  checkmate::assert_data_frame(data,
    min.cols = 2, min.rows = 2,
    col.names = "named"
  )
  checkmate::assert_string(condition)
  checkmate::assert_names(condition, subset.of = names(data))
  checkmate::assert(
    checkmate::check_factor(data[[condition]]),
    checkmate::check_character(data[[condition]]),
    checkmate::check_logical(data[[condition]]),
  )
  checkmate::assert_string(batch, null.ok = TRUE)
  if (!is.null(batch)) {
    checkmate::assert_names(batch, subset.of = names(data))
    if (isTRUE(checkmate::check_integerish(data[[batch]]))) {
      data[[batch]] <- factor(data[[batch]])
    } else if (isTRUE(checkmate::check_character(data[[batch]]))) {
      data[[batch]] <- factor(data[[batch]])
    }
    checkmate::assert_factor(data[[batch]], null.ok = TRUE)
  }
  checkmate::assert_character(genes, min.len = 1, null.ok = TRUE)
  if (is.null(genes)) {
    genes <- intersect(names(data), mirmisc::get_gene_names())
    if (!length(genes)) {
      custom_stop("No gene names found in {code('data')}.")
    }
  }
  checkmate::assert_names(genes, subset.of = names(data))
  if (is.logical(data[[condition]]) || is.character(data[[condition]])) {
    data[[condition]] <- as.factor(data[[condition]])
  }
  cond_tab <- table(data[[condition]])
  if (length(cond_tab) != 2) {
    custom_stop(
      "{code('data[[condition]]')} should be a factor with 2 levels.",
      "Your {code('data[[condition]]')} has {length(cond_tab)}
                level{ifelse(length(cond_tab) == 1, '', 's')}."
    )
  }
  if (min(cond_tab) == 0) {
    absent_level <- names(cond_tab)[!cond_tab][1]
    custom_stop("All levels of {code('data[[condition]]')} should be present.",
      "The level '{absent_level}' is absent.",
      .envir = list(
        data = data, condition = condition,
        absent_level = absent_level
      )
    )
  }
  data <- data[!is.na(data[[condition]]), ]
  checkmate::assert_string(padj_method, min.chars = 1)
  padj_method <- strex::match_arg(padj_method, stats::p.adjust.methods,
    ignore_case = TRUE
  )
  checkmate::assert_flag(quick)
  checkmate::assert_count(n_cores)
  n_cores <- min(n_cores, parallel::detectCores())
  n_cores <- dplyr::if_else(
    n_cores > length(genes),
    max(length(genes), 1), n_cores
  )
  checkmate::assert_flag(quiet)
  list(
    data = data,
    condition = condition,
    genes = genes,
    shrink = shrink,
    padj_method = padj_method,
    n_cores = n_cores,
    quiet = quiet
  )
}

argchk_cor_de <- function(data, condition, genes, padj_method) {
  checkmate::assert_data_frame(data)
  checkmate::assert_string(condition)
  numeric_cond <- FALSE
  if (condition %in% names(data) && is.numeric(data[[condition]])) {
    data <- data[!is.na(data[[condition]]), ]
    cond <- data[[condition]]
    data[[condition]] <- rep(c(TRUE, FALSE), length.out = nrow(data))
    numeric_cond <- TRUE
  }
  out <- argchk_deseq(
    data = data, condition = condition, batch = NULL, genes = genes,
    shrink = FALSE, padj_method = padj_method, quick = FALSE,
    n_cores = 1, quiet = FALSE
  )
  if (numeric_cond) out$data[[condition]] <- cond
  out[c("data", "condition", "genes", "padj_method")]
}
