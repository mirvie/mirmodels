#' Get the learning curve of a model as training data quantity increases.
#'
#' Given a training and test set, fit a model on increasing fractions of the
#' training set, up to the full set, with a constant test set per repeat (each
#' repeat will have a different test set). The default is to use 10%, 20%, 30%,
#' . . ., 90%, 100%. Care is taken to make sure each fraction is a subset of the
#' last e.g. all samples present in the 10% will be present in the 20% to
#' simulate the addition of more data, as opposed to a random sample of more
#' data. Optionally, you can pass all of your data in as the `training_data` and
#' then get the function to do the splitting for you.
#'
#' @param model_evaluate A function with exactly two arguments: `training_data`
#'   and `testing_data` that trains the model of choice on `training_data` and
#'   then produces predictions on `testing_data`, finally evaluating those
#'   predictions and outputting a length two numeric vector with names "cv" and
#'   "test" giving the cross-validation and test scores from the evaluation.
#' @param training_data A data frame. Subsets of this will be used for training.
#'   If `testing_data` is `NULL` and `testing_frac` is not, this will be split
#'   into training and testing sets, with `testing_frac` used for testing.
#' @inheritParams train_on_grid
#' @param testing_data A data frame. The trained models will all be tested
#'   against this constant test set.
#' @param testing_frac A numeric vector with values between 0 and 1/3.The
#'   fraction of `training_data` to use for the test set. This can only be used
#'   if `testing_data` is `NULL`. To try many different fractions, specify all
#'   of them as a numeric vector.
#' @param training_fracs A numeric vector. Fractions of the training data to
#'   use. This must be a positive, increasing vector of real numbers ending in
#'   1.
#' @param repeats A positive integer. The number of times to repeat the sampling
#'   for each proportion in `testing_frac`. This can be greater than 1 only if
#'   `testing_data` is `NULL` and `testing_frac` is not `NULL`. For each repeat,
#'   a different subsetting of `testing_data` remains takes place.
#' @param strata A string. Variable to stratify on when splitting data.
#'
#' @return A data frame with the following columns.
#'   * `rep`: The repeat number.
#'   * `testing_frac`: The fraction of `training_data` that is set aside for
#'     testing. If the `testing_data` argument is specified, `testing_frac` will
#'     be 0, because none of `training_data` is set aside for testing.
#'   * `training_frac`: The fraction of the (post train/test split)training data
#'     used for learning.
#'   * `testing_indices`: The row indices of the `training_data` argument that
#'     were set aside for testing. If `testing_data` is specified (and hence
#'     none of `training_data` needs to be set aside for testing, this will be a
#'     vector of `NA`s with length equal to the number of rows in
#'     `testing_data`.
#'   * `training_indices`: The row indices of the `training_data` that were used
#'     for learning.
#'   * `cv`: The cross-validation score.
#'   * `test`: The test score.
#'
#' @seealso [autoplot.mirvie_learning_curve()]
#'
#' @examples
#' data("BostonHousing", package = "mlbench")
#' bh <- dplyr::select_if(BostonHousing, is.numeric)
#' model_evaluate <- function(training_data, testing_data) {
#'   trained_mod <- lm(medv ~ ., training_data)
#'   training_preds <- predict(trained_mod, newdata = training_data)
#'   preds <- predict(trained_mod, newdata = testing_data)
#'   c(
#'     train = yardstick::mae_vec(training_data$medv, training_preds),
#'     test = yardstick::mae_vec(testing_data$medv, preds)
#'   )
#' }
#' mlc <- mlc0 <- suppressWarnings(
#'   learn_curve(model_evaluate, bh, "medv",
#'     training_fracs = c(seq(0.1, 0.7, 0.2), 0.85),
#'     testing_frac = c(0.25, 0.5), repeats = 8,
#'     strata = "medv", n_cores = 4
#'   )
#' )
#' @export
learn_curve <- function(model_evaluate, training_data, outcome,
                        testing_data = NULL, testing_frac = NULL,
                        training_fracs = seq(0.1, 1, by = 0.1), repeats = 1,
                        strata = NULL, n_cores = 1) {

  # Argument checking ----------------------------------------------------------
  argchk_learn_curve(
    model_evaluate = model_evaluate,
    training_data = training_data,
    outcome = outcome,
    testing_data = testing_data,
    testing_frac = testing_frac,
    training_fracs = training_fracs,
    repeats = repeats,
    strata = strata,
    n_cores = n_cores
  )
  checked_model_evaluate <- function(training_data, testing_data) {
    out <- model_evaluate(
      training_data = training_data,
      testing_data = testing_data
    )
    named_numeric_len2 <- isTRUE(
      checkmate::check_numeric(out, len = 2, names = "named")
    )
    if (!named_numeric_len2) {
      custom_stop("`model_evaluate` must return a
                   named length 2 numeric vector.")
    }
    good_names <- ("test" %in% names(out)) &&
      any(c("train", "cv") %in% names(out))
    if (!good_names) {
      custom_stop("`model_evaluate()` must return a length 2 numeric vector
                   with names 'cv' and 'test' or 'train' and 'test'.")
    }
    if (names(out)[1] == "test") out <- rev(out)
    out
  }
  if (0 %in% training_fracs) {
    training_fracs <- training_fracs[training_fracs != 0]
  }
  if (!(1 %in% training_fracs)) training_fracs <- c(training_fracs, 1)
  checkmate::assert_true(min(training_fracs) < 1)

  # Parallel setup -------------------------------------------------------------
  if (n_cores > 1) {
    old_plan <- future::plan(future::multisession, workers = n_cores)
    on.exit(future::plan(old_plan), add = TRUE)
    map_fun <- purrr::partial(furrr::future_map,
      .options = furrr::furrr_options(globals = FALSE)
    )
  } else {
    map_fun <- purrr::map
  }

  # Main body ------------------------------------------------------------------
  if (is.null(testing_data)) {
    indices <- replicate(
      repeats,
      get_single_rep_indices(
        data = training_data,
        testing_frac = testing_frac,
        training_fracs = training_fracs,
        strata = strata
      ),
      simplify = FALSE
    ) %>%
      purrr::imap(
        ~ dplyr::bind_cols(
          dplyr::tibble(rep = as.integer(.y)),
          .x
        )
      ) %>%
      dplyr::bind_rows()
    scores <- indices %>%
      dplyr::rowwise() %>%
      dplyr::group_split() %>%
      map_fun(
        ~ checked_model_evaluate(
          training_data[.[["training_indices"]][[1]], ],
          training_data[.[["testing_indices"]][[1]], ]
        )
      )
  } else {
    indices <- get_single_rep_indices(
      data = training_data,
      testing_frac = 0,
      training_fracs = training_fracs,
      strata = strata
    ) %>%
      dplyr::bind_cols(dplyr::tibble(rep = 1L), .) %>%
      dplyr::mutate(
        testing_indices = list(rep(NA_integer_, nrow(testing_data)))
      )
    scores <- indices %>%
      dplyr::rowwise() %>%
      dplyr::group_split() %>%
      map_fun(
        ~ checked_model_evaluate(
          training_data[.[["training_indices"]][[1]], ],
          testing_data
        )
      )
  }
  out <- indices %>%
    dplyr::mutate(scores = scores) %>%
    tidyr::unnest_wider(scores)
  new_mirvie_learning_curve(out)
}


#' Get indices for a single learning curve repeat.
#'
#' For a single repeat, given the training and testing fractions, get
#' appropriate training and testing indices with which to subset the data.
#'
#' This is primarily a helper for [learn_curve()].
#'
#' @param data A data frame. The data that will be subset.
#' @inheritParams learn_curve
#'
#' @return A data frame. For each combination of `testing_frac` and
#'   `training_fracs`, the appropriate train and test indices to use to
#'   calculate the learning curve.
#'
#' @noRd
get_single_rep_indices <- function(data, testing_frac, training_fracs,
                                   strata = NULL) {
  checkmate::assert_data_frame(data, min.rows = 2, min.cols = 1)
  checkmate::assert_numeric(testing_frac,
    lower = 0, upper = 1, min.len = 1,
    unique = TRUE, sorted = TRUE
  )
  checkmate::assert_numeric(training_fracs,
    lower = 0, upper = 1, min.len = 2,
    unique = TRUE, sorted = TRUE
  )
  checkmate::assert_true(min(training_fracs) < 1)
  checkmate::assert_true(max(training_fracs) == 1)
  checkmate::assert_true(all(testing_frac < 1))
  if (is.null(strata)) {
    out <- tidyr::expand_grid(
      testing_frac = testing_frac,
      training_frac = training_fracs
    ) %>%
      dplyr::mutate(
        possible_testing_indices_randomized = list(
          sample.int(nrow(data))
        )
      ) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        testing_indices = list(
          possible_testing_indices_randomized[
            seq_len(
              round(
                testing_frac * length(possible_testing_indices_randomized)
              )
            )
          ]
        ),
        possible_training_indices_randomized = list(
          dplyr::setdiff(
            possible_testing_indices_randomized,
            testing_indices
          )
        ),
        training_indices = list(
          possible_training_indices_randomized[
            seq_len(
              round(
                training_frac * length(possible_training_indices_randomized)
              )
            )
          ]
        )
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(-dplyr::starts_with("possible"))
  } else {
    if (length(testing_frac) == 1) {
      strata_df <- dplyr::tibble(
        strat = data[[strata]],
        row_num = seq_along(strat)
      )
      if (testing_frac == 0) {
        out <- get_strat_training_indices(strata_df, training_fracs) %>%
          dplyr::transmute(
            testing_frac = testing_frac,
            training_frac = training_frac,
            testing_indices = list(integer(0)),
            training_indices = training_indices
          )
      } else {
        init_split <- rsample::initial_split(strata_df,
          prop = 1 - testing_frac,
          strata = strat
        )
        testing_indices <- list(rsample::testing(init_split)$row_num)
        out <- get_strat_training_indices(
          rsample::training(init_split),
          training_fracs
        ) %>%
          dplyr::transmute(
            testing_frac = testing_frac,
            training_frac = training_frac,
            testing_indices = testing_indices,
            training_indices = training_indices
          )
      }
    } else {
      out <- purrr::map_dfr(
        testing_frac,
        ~ get_single_rep_indices(
          data = data, testing_frac = .,
          training_fracs = training_fracs,
          strata = strata
        )
      )
    }
  }
  out
}

#' Get training row indices from a data frame with stratifying info.
#'
#' Given a data frame `strata_df` with two rows: `row_num`, a subset of the row
#' indexes in the original data and `strat`, the stratifying variable
#' corresponding to those row indices, get appropriate training indices for
#' various `training_fracs`.
#'
#' @param `strata_df` A data frame with columns `strat` and `row_num`.
#' @param `training_fracs` A numeric vector whose values are in (0, 1].
#'
#' @return A tibble with columns `training_frac` (the input `training_fracs`)
#'   and `training_indices` (a list column of integer vectors).
#'
#' @noRd
get_strat_training_indices <- function(strata_df, training_fracs) {
  checkmate::assert_data_frame(strata_df,
    col.names = "named",
    min.rows = 2, ncols = 2
  )
  checkmate::assert_names(names(strata_df),
    permutation.of = c("row_num", "strat")
  )
  checkmate::assert_numeric(training_fracs,
    lower = 0, upper = 1, min.len = 2,
    unique = TRUE, sorted = TRUE
  )
  checkmate::assert_true(min(training_fracs) < 1)
  checkmate::assert_true(max(training_fracs) == 1)

  training_fracs <- rev(training_fracs)
  props <- training_fracs
  for (i in seq(2, length(props))) {
    props[[i]] <- props[[i]] / prod(utils::head(props, i - 1))
  }
  training_indices <- list(strata_df$row_num)
  for (i in seq(2, length(props))) {
    strata_df <- rsample::training(
      rsample::initial_split(strata_df, prop = props[[i]], strata = strat)
    )
    training_indices <- c(training_indices, list(strata_df$row_num))
  }
  dplyr::tibble(
    training_frac = rev(training_fracs),
    training_indices = rev(training_indices)
  )
}

#' Produce a learning curve plot from a
#' [mirvie_learning_curve][new_mirvie_learning_curve] object.
#'
#' This is a method for [ggplot2::autoplot()].
#'
#' @param object A [mirvie_learning_curve][new_mirvie_learning_curve] object
#'   (i.e. the output of a call to [learn_curve()]).
#' @param ... Arguments passed to [ggplot2::autoplot()]. Safe to ignore.
#' @param metric A string. The metric used to evaluate the performance.
#' @param smooth A flag. Use a loess smoothed line instead of joining the dots?
#' @param meansd A flag. If there are multiple repeats, rather than plotting all
#'   of them, plot means with standard deviation error bars?
#'
#' @return A [ggplot2::ggplot()].
#'
#' @examples
#' data("BostonHousing", package = "mlbench")
#' bh <- dplyr::select_if(BostonHousing, is.numeric)
#' model_evaluate <- function(training_data, testing_data) {
#'   trained_mod <- lm(medv ~ ., training_data)
#'   training_preds <- predict(trained_mod, newdata = training_data)
#'   preds <- predict(trained_mod, newdata = testing_data)
#'   c(
#'     train = yardstick::mae_vec(training_data$medv, training_preds),
#'     test = yardstick::mae_vec(testing_data$medv, preds)
#'   )
#' }
#' mlc <- mlc0 <- suppressWarnings(
#'   learn_curve(model_evaluate, bh, "medv",
#'     training_fracs = c(seq(0.1, 0.7, 0.2), 0.85),
#'     testing_frac = c(0.25, 0.5), repeats = 8,
#'     strata = "medv"
#'   )
#' )
#' suppressWarnings(print(autoplot(mlc, metric = "mae")))
#' suppressWarnings(print(autoplot(mlc, metric = "mae", smooth = TRUE)))
#' suppressWarnings(print(autoplot(mlc, metric = "mae", meansd = TRUE)))
#' suppressWarnings(
#'   print(autoplot(mlc, metric = "mae", smooth = TRUE, meansd = TRUE))
#' )
#' mlc <- dplyr::filter(mlc0, testing_frac == 0.25)
#' suppressWarnings(print(autoplot(mlc, metric = "mae")))
#' suppressWarnings(print(autoplot(mlc, metric = "mae", smooth = TRUE)))
#' suppressWarnings(print(autoplot(mlc, metric = "mae", meansd = TRUE)))
#' suppressWarnings(
#'   print(autoplot(mlc, metric = "mae", smooth = TRUE, meansd = TRUE))
#' )
#' mlc <- dplyr::filter(mlc0, rep == 1)
#' suppressWarnings(print(autoplot(mlc, metric = "mae")))
#' suppressWarnings(print(autoplot(mlc, metric = "mae", smooth = TRUE)))
#' mlc <- dplyr::filter(mlc0, rep == 1, testing_frac == 0.25)
#' suppressWarnings(print(autoplot(mlc, metric = "mae")))
#' suppressWarnings(print(autoplot(mlc, metric = "mae", smooth = TRUE)))
#' bh_split <- rsample::initial_split(bh, strata = medv)
#' bh_training <- rsample::training(bh_split)
#' bh_testing <- rsample::testing(bh_split)
#' mlc <- learn_curve(model_evaluate,
#'   training_data = bh_training,
#'   outcome = "medv", testing_data = bh_testing,
#'   strata = "medv"
#' )
#' suppressWarnings(print(autoplot(mlc)))
#' suppressWarnings(print(autoplot(mlc, smooth = TRUE)))
#' @export
autoplot.mirvie_learning_curve <- function(object, metric = NULL,
                                           smooth = FALSE, meansd = FALSE,
                                           ...) {
  argchk_autoplot_mlc(
    object = object,
    metric = metric,
    smooth = smooth,
    meansd = meansd
  )
  plot_tbl <- object %>%
    tidyr::pivot_longer(dplyr::any_of(c("train", "cv", "test")),
      names_to = "type", values_to = "score"
    ) %>%
    dplyr::mutate(rep = factor(rep),
                  type = factor(type, levels = c("train", "cv", "test")))
  plot_tbl$rep <- factor(plot_tbl$rep)
  if (dplyr::n_distinct(plot_tbl$rep) == 1 &&
    dplyr::n_distinct(plot_tbl$testing_frac) == 1) {
    n_training_samples <- max(lengths(plot_tbl$training_indices)) %>%
      format(big.mark = ",")
    n_testing_samples <- max(lengths(plot_tbl$testing_indices)) %>%
      format(big.mark = ",")
    plot_tbl$type <- forcats::fct_rev(factor(plot_tbl$type))
    out <- ggplot2::ggplot(plot_tbl) +
      ggplot2::aes(
        x = training_frac, y = score,
        colour = type
      ) +
      ggplot2::geom_point() +
      geom_autoplot_mlc_line(
        plot_tbl = plot_tbl,
        meansd = meansd, smooth = smooth
      ) +
      ggthemes::scale_color_colorblind(
        guide = ggplot2::guide_legend(reverse = TRUE)
      ) +
      ggplot2::ggtitle(
        "Learning curve",
        glue::glue(
          "Full training set: {n_training_samples} samples. ",
          "Test set: {n_testing_samples} samples."
        )
      )
  } else if (dplyr::n_distinct(plot_tbl$rep) > 1 &&
    dplyr::n_distinct(plot_tbl$testing_frac) == 1) {
    n_training_samples <- max(lengths(plot_tbl$training_indices)) %>%
      format(big.mark = ",")
    n_testing_samples <- max(lengths(plot_tbl$testing_indices)) %>%
      format(big.mark = ",")
    if (meansd) {
      plot_tbl <- plot_tbl %>%
        dplyr::group_by(testing_frac, training_frac, type) %>%
        dplyr::summarise(
          scoresd = stats::sd(score), score = mean(score),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          upper = score + scoresd,
          lower = score - scoresd,
          type = forcats::fct_rev(factor(type))
        )
      dodge_width <- min(diff(sort(unique(plot_tbl$training_frac)))) * 0.4
      out <- ggplot2::ggplot(plot_tbl) +
        ggplot2::aes(
          x = training_frac, y = score,
          colour = type
        ) +
        ggplot2::geom_point(
          position = ggplot2::position_dodge(width = dodge_width)
        ) +
        geom_autoplot_mlc_line(
          plot_tbl = plot_tbl,
          meansd = meansd, smooth = smooth
        ) +
        ggplot2::geom_errorbar(
          ggplot2::aes(ymin = lower, ymax = upper),
          position = ggplot2::position_dodge(width = dodge_width)
        ) +
        ggthemes::scale_color_colorblind(
          guide = ggplot2::guide_legend(reverse = TRUE)
        ) +
        ggplot2::ggtitle(
          "Learning curve",
          glue::glue(
            "Full training set: {n_training_samples} samples. ",
            "Test set: {n_testing_samples} samples."
          )
        )
    } else {
      out <- ggplot2::ggplot(plot_tbl) +
        ggplot2::aes(
          x = training_frac, y = score,
          colour = rep, linetype = type, shape = type
        ) +
        ggplot2::geom_point() +
        geom_autoplot_mlc_line(
          plot_tbl = plot_tbl,
          meansd = meansd, smooth = smooth
        ) +
        ggplot2::scale_linetype_manual(values = c("dashed", "solid")) +
        ggthemes::scale_color_colorblind() +
        ggplot2::ggtitle(
          "Learning curve",
          glue::glue(
            "Full training set: {n_training_samples} samples. ",
            "Test set: {n_testing_samples} samples."
          )
        )
    }
  } else if (dplyr::n_distinct(plot_tbl$rep) == 1 &&
    dplyr::n_distinct(plot_tbl$testing_frac) > 1) {
    plot_tbl <- plot_tbl %>%
      dplyr::mutate(
        testing_frac = glue::glue(
          "Test proportion: {scales::percent(testing_frac)}"
        ),
        type = forcats::fct_rev(factor(type))
      )
    ntot_samples <- plot_tbl %>%
      dplyr::filter(training_frac == 1) %>%
      dplyr::select(training_indices, testing_indices) %>%
      dplyr::slice(1) %>%
      purrr::map_int(lengths) %>%
      sum() %>%
      format(big.mark = ",")
    out <- ggplot2::ggplot(plot_tbl) +
      ggplot2::aes(x = training_frac, y = score, colour = type) +
      ggplot2::geom_point() +
      geom_autoplot_mlc_line(
        plot_tbl = plot_tbl,
        meansd = meansd, smooth = smooth
      ) +
      ggthemes::scale_color_colorblind(
        guide = ggplot2::guide_legend(reverse = TRUE)
      ) +
      ggplot2::facet_wrap(
        "testing_frac",
        nrow = floor(sqrt(dplyr::n_distinct(plot_tbl$testing_frac)))
      ) +
      ggplot2::ggtitle(
        "Learning curve",
        glue::glue(
          "Full set: {ntot_samples} samples. ",
          "Various train/test splits."
        )
      )
  } else if (dplyr::n_distinct(plot_tbl$rep) > 1 &&
    dplyr::n_distinct(plot_tbl$testing_frac) > 1) {
    plot_tbl <- plot_tbl %>%
      dplyr::mutate(
        testing_frac = glue::glue(
          "Test proportion: {scales::percent(testing_frac)}"
        )
      )
    ntot_samples <- plot_tbl %>%
      dplyr::filter(training_frac == 1) %>%
      dplyr::select(training_indices, testing_indices) %>%
      dplyr::slice(1) %>%
      purrr::map_int(lengths) %>%
      sum() %>%
      format(big.mark = ",")
    if (meansd) {
      plot_tbl <- plot_tbl %>%
        dplyr::group_by(testing_frac, training_frac, type) %>%
        dplyr::summarise(
          scoresd = stats::sd(score), score = mean(score),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          upper = score + scoresd,
          lower = score - scoresd,
          type = forcats::fct_rev(factor(type))
        )
      dodge_width <- min(diff(sort(unique(plot_tbl$training_frac)))) * 0.4
      out <- ggplot2::ggplot(plot_tbl) +
        ggplot2::aes(
          x = training_frac, y = score,
          colour = type
        ) +
        ggplot2::geom_point(
          position = ggplot2::position_dodge(width = dodge_width)
        ) +
        geom_autoplot_mlc_line(
          plot_tbl = plot_tbl,
          meansd = meansd, smooth = smooth
        ) +
        ggplot2::geom_errorbar(
          ggplot2::aes(ymin = lower, ymax = upper),
          position = ggplot2::position_dodge(width = dodge_width)
        ) +
        ggthemes::scale_color_colorblind(
          guide = ggplot2::guide_legend(reverse = TRUE)
        ) +
        ggplot2::facet_wrap(
          "testing_frac",
          nrow = floor(sqrt(dplyr::n_distinct(plot_tbl$testing_frac)))
        ) +
        ggplot2::ggtitle(
          "Learning curve",
          glue::glue(
            "Full set: {ntot_samples} samples. ",
            "Various train/test splits."
          )
        )
    } else {
      out <- ggplot2::ggplot(plot_tbl) +
        ggplot2::aes(
          x = training_frac, y = score,
          colour = rep, linetype = type, shape = type
        ) +
        ggplot2::geom_point() +
        geom_autoplot_mlc_line(
          plot_tbl = plot_tbl,
          meansd = meansd, smooth = smooth
        ) +
        ggplot2::scale_linetype_manual(values = c("dashed", "solid")) +
        ggthemes::scale_color_colorblind() +
        ggplot2::facet_wrap(
          "testing_frac",
          nrow = floor(sqrt(dplyr::n_distinct(plot_tbl$testing_frac)))
        ) +
        ggplot2::ggtitle(
          "Learning curve",
          glue::glue(
            "Full set: {ntot_samples} samples. ",
            "Various train/test splits."
          )
        )
    }
  }
  out <- out +
    ggplot2::scale_x_continuous(
      breaks = unique(c(0, sort(plot_tbl$training_frac))),
      labels = function(x) scales::percent(x, accuracy = 1)
    ) +
    ggplot2::xlab("proportion of full training set used") +
    ggplot2::ylab(metric %||% "score")
  out
}

#' Get the appropriate line geom for [autoplot.mirvie_learning_curve()].
#'
#' This is only for use inside of [autoplot.mirvie_learning_curve()].
#'
#' @return A [ggplot2::geom_line()] or [ggplot2::geom_smooth()] with
#'   appropriate parameters.
#'
#' @noRd
geom_autoplot_mlc_line <- function(plot_tbl, meansd, smooth) {
  if (meansd) {
    dodge_width <- min(diff(sort(unique(plot_tbl$training_frac)))) * 0.4
    ggline_pos <- ggplot2::position_dodge(width = dodge_width)
  } else {
    ggline_pos <- "identity"
  }
  if (smooth) {
    ggline <- ggplot2::geom_smooth(
      formula = y ~ x,
      method = "loess",
      se = FALSE,
      position = ggline_pos
    )
  } else {
    ggline <- ggplot2::geom_line(position = ggline_pos)
  }
  ggline
}


#' Construct a `mirvie_learning_curve` object.
#'
#' A `mirvie_learning_curve` object is what is output by [learn_curve()].
#'
#' This just tacks `"mirvie_learning_curve"` onto the front of the `class`
#' attribute of an appropriate object. This should only be used inside of
#' [learn_curve()].
#'
#' @param tib A tibble with columns `rep` (int), `testing_frac` (dbl),
#'   `training_frac` (dbl), `testing_indices` (list), `training_indices` (list),
#'   `cv` (dbl) and `test` (dbl).
#'
#' @return A `mirvie_learning_curve` object.
#'
#' @export
new_mirvie_learning_curve <- function(tib) {
  class(tib) <- c("mirvie_learning_curve", class(tib))
  verify_mirvie_learning_curve(tib)
  tib
}

#' Verify that an object is a bona-fide `mirvie_learning_curve`.
#'
#' This is needed for [autoplot.mirvie_learning_curve()].
#'
#' @inheritParams new_mirvie_learning_curve
#'
#' @noRd
verify_mirvie_learning_curve <- function(tib) {
  checkmate::assert_tibble(tib)
  checkmate::assert_class(tib, "mirvie_learning_curve")
  checkmate::assert(
    checkmate::check_names(
      names(tib),
      identical.to = c(
        "rep", "testing_frac", "training_frac",
        "testing_indices", "training_indices",
        "cv", "test"
      )
    ),
    checkmate::check_names(
      names(tib),
      identical.to = c(
        "rep", "testing_frac", "training_frac",
        "testing_indices", "training_indices",
        "train", "test"
      )
    )
  )
  col_types <- purrr::map_chr(tib, typeof)
  correct_types <- c(
    "integer", rep("double", 2),
    rep("list", 2), rep("double", 2)
  ) %>%
    magrittr::set_names(names(col_types))
  if (!isTRUE(all.equal(col_types, correct_types))) {
    custom_stop(
      "
      The column {code(names(tib)[first_bad])} should be of
      type '{correct_types[first_bad]}'.
      ",
      "Currently, it is of type '{col_types[first_bad]}'.",
      .envir = list(
        first_bad = match(FALSE, col_types == correct_types),
        tib = tib,
        correct_types = correct_types,
        col_types = col_types
      )
    )
  }
  invisible(TRUE)
}

#' Create a cross-validation learning curve.
#'
#' This function needs a [workflows::workflow] ready for [tune::fit_resamples].
#' It does different fold cross-validation to vary the training set sizes and
#' then collects the predictions and scores them.
#'
#' @param data A data frame. The data to be used for the modelling.
#' @param wf A [workflows::workflow()]. Should have been constructed using
#'   `data`.
#' @param folds An integer vector. Different `v` to use in
#'   [rsample::vfold_cv()].
#' @param repeats The number of times to repeat each cross-validation.
#' @param metric_calculator A function which takes a single data frame argument
#'   and returns a double. The data frame that will be passed to this function
#'   is the output of [tune::collect_predictions()] which will be run on the
#'   output of `tune::fit_resamples(save_preds = TRUE)`. See the example below.
#' @inheritParams train_on_grid
#' @param pkgs A character vector. Passed to [tune::control_resamples()].
#'
#' @return A tibble with 2 columns:
#'   * `training_samples`: The number of samples used in training.
#'   * `score`: The score computed by `metric_calculator()`.
#'
#' @examples
#' data("BostonHousing", package = "mlbench")
#' bh <- dplyr::select_if(BostonHousing, is.numeric)
#' mod <- parsnip::linear_reg(penalty = 0, mixture = 0) %>%
#'   parsnip::set_engine("lm")
#' wf <- workflows::workflow() %>%
#'   workflows::add_formula(medv ~ .) %>%
#'   workflows::add_model(mod)
#' metric_calculator <- ~ yardstick::mae(., medv, .pred)$.estimate
#' lccv <- suppressWarnings(
#'   learn_curve_cv(bh, wf, 2:9, 3, metric_calculator, n_cores = 4)
#' )
#' @export
learn_curve_cv <- function(data, wf, folds, repeats, metric_calculator,
                           strata = NULL, pkgs = c("mirmodels"), n_cores = 1) {

  # Argument checking ----------------------------------------------------------
  checkmate::assert_data_frame(data, min.rows = 2, min.cols = 2)
  checkmate::assert_class(wf, "workflow")
  checkmate::assert_integerish(folds, lower = 2, unique = TRUE)
  checkmate::assert_count(repeats)
  if (!isTRUE(checkmate::check_function(metric_calculator))) {
    if (rlang::is_formula(metric_calculator)) {
      metric_calculator <- rlang::as_function(metric_calculator)
    }
  }
  checkmate::assert_function(metric_calculator)
  checked_metric_calculator <- function(df) {
    cmc_out <- metric_calculator(df)
    if (!isTRUE(checkmate::check_number(cmc_out))) {
      custom_stop(
        "{code('metric_calculator()')} must output a single number.",
        "This time, the output was of class {class(cmc_out)[1]}.",
        "The length of the {class(cmc_out)[1]} was
         {tryCatch(length(cmc_out), error = function(cnd) 0)}."
      )
    }
    cmc_out
  }
  checkmate::assert_string(strata, min.chars = 1, null.ok = TRUE)
  if (!is.null(strata)) {
    checkmate::assert_names(strata, subset.of = names(data))
  }
  checkmate::assert_character(pkgs, min.chars = 1, null.ok = TRUE)
  if (length(pkgs) == 0) pkgs <- NULL
  checkmate::assert_count(n_cores)

  # Parallel setup -------------------------------------------------------------
  old_plan <- future::plan(future::multisession,
    workers = n_cores
  )
  on.exit(future::plan(old_plan), add = TRUE)
  doFuture::registerDoFuture()

  # Main body ------------------------------------------------------------------
  folds <- rep(as.integer(folds), repeats)
  collected_prediction_list <- furrr::future_map(
    folds,
    function(n_folds) {
      if (is.null(strata)) {
        rs <- rsample::vfold_cv(data, v = n_folds)
      } else {
        rs <- rsample::vfold_cv(data, v = n_folds, strata = !!strata)
      }
      tune::fit_resamples(
        wf, rs,
        control = tune::control_resamples(save_pred = TRUE, pkgs = pkgs)
      ) %>%
        tune::collect_predictions(summarize = TRUE)
    }
  )
  dplyr::tibble(
    training_samples = as.integer(round(nrow(data) - nrow(data) / folds)),
    score = purrr::map_dbl(collected_prediction_list, checked_metric_calculator)
  ) %>%
    new_mirvie_learning_curve_cv()
}

#' Verify that an object is a bona-fide `mirvie_learning_curve_cv`.
#'
#' This is needed for [autoplot.mirvie_learning_curve_cv()].
#'
#' @inheritParams new_mirvie_learning_curve_cv
#'
#' @noRd
verify_mirvie_learning_curve_cv <- function(tib) {
  checkmate::assert_class(tib, "mirvie_learning_curve_cv")
  checkmate::assert_tibble(tib, ncols = 2)
  checkmate::assert_names(names(tib),
    permutation.of = c("training_samples", "score")
  )
  checkmate::assert_integer(tib$training_samples, lower = 2)
  checkmate::assert_double(tib$score)
  invisible(TRUE)
}

#' Construct a `mirvie_learning_curve` object.
#'
#' A `mirvie_learning_curve_cv` object is what is output by [learn_curve_cv()].
#'
#' This just tacks `"mirvie_learning_curve_cv"` onto the front of the `class`
#' attribute of an appropriate object. This should only be used inside of
#' [learn_curve_cv()].
#'
#' @param tib A tibble with columns `training_samples` (integer, the number of
#'   training samples on that iteration) and `score` (double, a model metric
#'   score).
#'
#' @return A `mirvie_learning_curve_cv` object.
#'
#' @export
new_mirvie_learning_curve_cv <- function(tib) {
  class(tib) <- unique(c("mirvie_learning_curve_cv", class(tib)))
  verify_mirvie_learning_curve_cv(tib)
  tib
}

#' Produce a learning curve plot from a
#' [mirvie_learning_curve][new_mirvie_learning_curve_cv] object.
#'
#' This is a method for [ggplot2::autoplot()].
#'
#' @param object A [mirvie_learning_curve_cv][new_mirvie_learning_curve_cv]
#'   object (i.e. the output of a call to [learn_curve_cv()]).
#' @inheritParams autoplot.mirvie_learning_curve
#'
#' @return A [ggplot2::ggplot()].
#'
#' @examples
#' data("BostonHousing", package = "mlbench")
#' bh <- dplyr::select_if(BostonHousing, is.numeric)
#' mod <- parsnip::linear_reg(penalty = 0, mixture = 0) %>%
#'   parsnip::set_engine("lm")
#' wf <- workflows::workflow() %>%
#'   workflows::add_formula(medv ~ .) %>%
#'   workflows::add_model(mod)
#' metric_calculator <- ~ yardstick::mae(., medv, .pred)$.estimate
#' lccv <- suppressWarnings(
#'   learn_curve_cv(bh, wf, 2:9, 8, metric_calculator, n_cores = 4)
#' )
#' autoplot(lccv, metric = "mae")
#' autoplot(lccv, metric = "mae", smooth = TRUE)
#' autoplot(lccv, metric = "mae", meansd = TRUE)
#' autoplot(lccv, metric = "mae", smooth = TRUE, meansd = TRUE)
#' @export
autoplot.mirvie_learning_curve_cv <- function(object, metric = NULL,
                                              smooth = FALSE, meansd = FALSE,
                                              ...) {

  # Argument checking ----------------------------------------------------------
  verify_mirvie_learning_curve_cv(object)
  checkmate::assert_string(metric, null.ok = TRUE)
  checkmate::assert_flag(smooth)
  checkmate::assert_flag(meansd)

  # Main body ------------------------------------------------------------------
  plot_tbl <- object %>%
    dplyr::group_by(training_samples) %>%
    dplyr::mutate(repeat_num = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(repeat_num, training_samples)
  if (smooth) {
    line_geom <- ggplot2::geom_smooth(
      se = FALSE, formula = y ~ x,
      method = "loess"
    )
  } else {
    line_geom <- ggplot2::geom_line()
  }
  if (meansd) {
    plot_tbl <- plot_tbl %>%
      dplyr::group_by(training_samples) %>%
      dplyr::summarise(
        score_sd = stats::sd(score), score = mean(score),
        .groups = "drop"
      )
    out <- ggplot2::ggplot(
      plot_tbl,
      ggplot2::aes(training_samples, score)
    ) +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = score - score_sd, ymax = score + score_sd)
      ) +
      line_geom +
      ggplot2::geom_point()
  } else {
    plot_tbl$repeat_num <- strex::str_alphord_nums(plot_tbl$repeat_num)
    out <- ggplot2::ggplot(
      plot_tbl,
      ggplot2::aes(training_samples, score, color = repeat_num)
    ) +
      line_geom +
      ggplot2::geom_point() +
      ggplot2::scale_color_viridis_d()
  }
  out <- out +
    ggplot2::xlab("number of training samples") +
    ggplot2::ylab(metric %||% "score")
  out
}
