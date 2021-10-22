#' Plot ROC curves with confidence intervals.
#'
#' Plot ROC curves for several different models (or just one) with confidence
#' intervals calculated by [pROC::ci.se()] and [pROC::ci.sp()].
#'
#' Inside this function, [pROC::roc()] is called with `direction = "<"`. The
#' easiest thing is to have `rocdf$response` be a vector of 0s and 1s and have
#' `rocdf$predictor` be a vector of probabilities where each probability is the
#' probability of a response of 1. If `rocdf$predictor` is logical, then these
#' `rocdf$predictor` is the probability of a response of `TRUE`.
#'
#' @param rocdf A data frame with columns `response` and `predictor` (see
#'   [pROC::roc()]. To plot several models, provide a `model` column with the
#'   model names; one ROC will be calculated and plot per model name.
#' @param conf_level A number in `[0, 1]`. This is passed as the `conf.level`
#'   argument to [pROC::ci.se()] and [pROC::ci.sp()]. Use `conf_level = NA` to
#'   disable plotting of confidence intervals.
#' @param aes_opts A named list of aesthetic options for the plot.
#'   * `conf_alpha`: Transparency of the confidence interval lines.
#'   * `roc_line_size`: Thickness of the ROC line.
#'   * `roc_line_color`: Color of the ROC line.
#'   * `null_line_size`: Thickness of the null line.
#'   * `null_line_color`: Color of the null line.
#'   * `null_line_alpha`: Transparency of the null line
#' @param quick A flag. With `quick = FALSE`, ROC confidence intervals are
#'   calculated with a 2,000-round bootstrap. `quick = TRUE` will use 100 rounds
#'   instead. `quick = TRUE` is fine for exploration.
#' @param parallel A flag. Calculate the sensitivity and specificity confidence
#'   intervals simultaneously? This is a small speedup (<2x).
#'
#' @return A [ggplot2::ggplot()].
#'
#' @examples
#' rocdf1 <- dplyr::tibble( # decent model
#'   predictor = seq(0.01, 0.99, length.out = 1000),
#'   response = purrr::rbernoulli(length(predictor), predictor)
#' )
#' rocdf2 <- dplyr::tibble( # bad model
#'   predictor = seq(0.01, 0.99, length.out = 88),
#'   response = purrr::rbernoulli(length(predictor), 0.5)
#' )
#' rocdf12 <- dplyr::bind_rows(
#'   dplyr::bind_cols(rocdf1, model = "decent"),
#'   dplyr::bind_cols(rocdf2, model = "bad")
#' )
#' plot_roc(rocdf1,
#'   quick = TRUE, parallel = FALSE,
#'   aes_opts = list(conf_alpha = 0.3)
#' )
#' plot_roc(rocdf2,
#'   conf_level = NA, quick = TRUE,
#'   aes_opts = list(roc_line_size = 2, roc_line_color = "red")
#' )
#' plot_roc(rocdf12,
#'   conf_level = NA, quick = TRUE,
#'   aes_opts = list(null_line_size = 2, null_line_alpha = 1)
#' )
#' plot_roc(rocdf12, conf_level = 0.99, quick = TRUE)
#' @export
plot_roc <- function(rocdf, conf_level = 0.95,
                     aes_opts = list(
                       conf_alpha = 0.8,
                       roc_line_size = 1,
                       roc_line_color = "black",
                       null_line_size = 1,
                       null_line_color = "blue",
                       null_line_alpha = 0.5
                     ),
                     quick = TRUE, parallel = TRUE) {
  if (is.list(rocdf) && ("response" %in% names(rocdf)) &&
    (is.logical(rocdf$response))) {
    rocdf$response <- as.integer(rocdf$response)
  }
  checkmate::assert_list(aes_opts)
  aes_opts$conf_alpha <- aes_opts$conf_alpha %||% 0.8
  aes_opts$roc_line_size <- aes_opts$roc_line_size %||% 1
  aes_opts$roc_line_color <- aes_opts$roc_line_color %||% "black"
  aes_opts$null_line_size <- aes_opts$null_line_size %||% 1
  aes_opts$null_line_color <- aes_opts$null_line_color %||% "blue"
  aes_opts$null_line_alpha <- aes_opts$null_line_alpha %||% 0.5
  argchk_plot_roc(
    rocdf = rocdf, conf_level = conf_level,
    aes_opts = aes_opts,
    quick = quick, parallel = parallel
  )
  if ("model" %in% names(rocdf)) {
    auc_tbl <- rocdf %>%
      dplyr::group_by(model) %>%
      dplyr::summarise(
        get_single_auc_tbl(
          response = response, predictor = predictor,
          conf_level = conf_level
        )
      ) %>%
      dplyr::arrange(dplyr::desc(AUC)) %>%
      list() %>%
      dplyr::tibble(x = 0, y = 0, tbl = .)
    plot_df <- rocdf %>%
      dplyr::group_by(model) %>%
      dplyr::summarise(
        get_single_roc_plot_tbl(
          response = response, predictor = predictor,
          conf_level = conf_level, quick = quick, parallel = parallel
        )
      ) %>%
      dplyr::mutate(model = factor(model, levels = auc_tbl$tbl[[1]]$model))
    if (isTRUE(as.logical(conf_level))) {
      out <- ggplot2::ggplot(
        plot_df,
        ggplot2::aes(x = spec, y = sens, color = model)
      ) +
        ggplot2::geom_path(ggplot2::aes(y = sens_lower),
          linetype = "dashed", alpha = aes_opts$conf_alpha
        ) +
        ggplot2::geom_path(ggplot2::aes(y = sens_upper),
          linetype = "dashed", alpha = aes_opts$conf_alpha
        ) +
        ggplot2::geom_path(
          size = aes_opts$roc_line_size,
          color = aes_opts$roc_line_color
        ) +
        ggpmisc::geom_table(ggplot2::aes(x = x, y = y, label = tbl),
          data = auc_tbl
        ) +
        ggthemes::scale_color_colorblind()
    } else {
      out <- ggplot2::ggplot(
        plot_df,
        ggplot2::aes(x = spec, y = sens, color = model)
      ) +
        ggplot2::geom_path(
          size = aes_opts$roc_line_size,
          color = aes_opts$roc_line_color
        ) +
        ggpmisc::geom_table(ggplot2::aes(x = x, y = y, label = tbl),
          data = auc_tbl
        ) +
        ggthemes::scale_color_colorblind()
    }
  } else {
    plot_df <- get_single_roc_plot_tbl(
      response = rocdf$response,
      predictor = rocdf$predictor,
      conf_level = conf_level,
      quick = quick,
      parallel = parallel
    )
    auc_tbl <- get_single_auc_tbl(
      response = rocdf$response,
      predictor = rocdf$predictor,
      conf_level = conf_level
    ) %>%
      dplyr::transmute(x = 0, y = 0, tbl = list(.))
    if (isTRUE(as.logical(conf_level))) {
      out <- ggplot2::ggplot(plot_df, ggplot2::aes(x = spec, y = sens)) +
        ggplot2::geom_path(ggplot2::aes(y = sens_lower),
          linetype = "dashed", alpha = aes_opts$conf_alpha
        ) +
        ggplot2::geom_path(ggplot2::aes(y = sens_upper),
          linetype = "dashed", alpha = aes_opts$conf_alpha
        ) +
        ggplot2::geom_path(
          size = aes_opts$roc_line_size,
          color = aes_opts$roc_line_color
        ) +
        ggpmisc::geom_table(ggplot2::aes(x = x, y = y, label = tbl),
          data = auc_tbl
        )
    } else {
      out <- ggplot2::ggplot(plot_df, ggplot2::aes(x = spec, y = sens)) +
        ggplot2::geom_path(
          size = aes_opts$roc_line_size,
          color = aes_opts$roc_line_color
        ) +
        ggpmisc::geom_table(ggplot2::aes(x = x, y = y, label = tbl),
          data = auc_tbl
        )
    }
  }
  out +
    ggplot2::annotate("segment",
      x = 1, y = 0, xend = 0, yend = 1,
      linetype = "dotted",
      size = aes_opts$null_line_size, alpha = aes_opts$null_line_alpha,
      color = aes_opts$null_line_color
    ) +
    ggplot2::scale_x_continuous(
      name = "specificity", trans = "reverse",
      labels = scales::percent
    ) +
    ggplot2::scale_y_continuous(
      name = "sensitivity",
      labels = scales::percent
    ) +
    ggplot2::coord_fixed()
}

#' Get the tibble for plotting a single ROC curve.
#'
#' Construct [dplyr::tibble()] with columns for sensitivity (`sens`) levels for
#' each specificity (`spec`) level between 0 and 1, with graduations of 0.004.
#' If `conf_level` is specified, these will be accompanied by `ci_lower` and
#' `ci_upper` values appropriate for plotting with [ggplot2::geom_errorbar()].
#'
#' @inheritParams pROC::roc
#' @inheritParams plot_roc
#'
#' @return A [dplyr::tibble()].
#'
#' @noRd
get_single_roc_plot_tbl <- function(response, predictor, conf_level, quick,
                                    parallel) {
  checkmate::assert_integerish(response,
    lower = 0, upper = 1, any.missing = FALSE
  )
  checkmate::assert_numeric(predictor,
    lower = 0, upper = 1, any.missing = FALSE
  )
  checkmate::assert_number(conf_level, na.ok = TRUE, lower = 0, upper = 1)
  checkmate::assert_flag(quick)
  roc <- suppressMessages(
    pROC::roc(
      response = response, predictor = predictor,
      direction = "<"
    )
  )
  grads <- seq(0, 1, 0.004)
  out <- suppressWarnings(pROC::coords(roc)) %>%
    dplyr::transmute(spec = specificity, sens = sensitivity) %>%
    dplyr::bind_rows(dplyr::tibble(spec = grads, sens = NA)) %>%
    dplyr::arrange(dplyr::desc(spec), sens) %>%
    dplyr::mutate(sens = zoo::na.locf(sens)) %>%
    dplyr::filter(spec %in% grads)
  if (isTRUE(as.logical(conf_level))) {
    boot_n <- dplyr::if_else(quick, 100, 2000)
    ci_sp_se <- get_se_sp_confs(
      roc = roc,
      grads = grads, conf_level = conf_level,
      boot_n = boot_n, parallel = parallel
    )
    sens_bounds <- dplyr::bind_rows(
      dplyr::tibble(spec = ci_sp_se$sp$sp_lower, sens = grads),
      dplyr::tibble(spec = ci_sp_se$sp$sp_upper, sens = grads),
      data.frame(spec = grads, sens = unlist(ci_sp_se$se)) # uses recycling
    ) %>%
      dplyr::group_by(spec) %>%
      dplyr::summarise(sens_lower = min(sens), sens_upper = max(sens)) %>%
      dplyr::arrange(dplyr::desc(spec)) %>%
      dplyr::mutate(
        sens_lower = stats::smooth.spline(sens_lower, nknots = 25)$y,
        sens_lower = c(sens_lower[seq_len(length(sens_lower) - 1)], 1),
        sens_lower = pmin(pmax(sens_lower, 0), 1), # truncate to [0, 1]
        sens_upper = stats::smooth.spline(sens_upper, nknots = 25)$y,
        sens_upper = pmin(pmax(sens_upper, 0), 1)
      )
    out <- dplyr::inner_join(out, sens_bounds,
      by = c("spec" = "spec")
    )
  }
  dplyr::as_tibble(out)
}

#' Get sensitivity and specificity confidence intervals
#'
#' @param roc The output of a call to [pROC::roc()].
#' @param grads An increasing numeric vector between 0 and 1.
#' @param conf_level A number. The confidence level.
#' @param boot_n A count. The number of bootstrap reps.
#' @param parallel A flag. If `TRUE`, [pROC::ci.se()] and [pROC::ci.sp()] will
#'   be computed in parallel.
#'
#' @return A list with elements named "se" and "sp", the outputs of
#'   [pROC::ci.se()] and [pROC::ci.sp()]
#'
#' @noRd
get_se_sp_confs <- function(roc, grads, conf_level, boot_n, parallel) {
  checkmate::assert_numeric(grads,
    lower = 0, upper = 1,
    unique = TRUE, sorted = TRUE
  )
  checkmate::assert_number(conf_level, lower = 0, upper = 1)
  checkmate::assert_count(boot_n)
  checkmate::assert_flag(parallel)
  if (parallel) {
    old_plan <- future::plan(future::multisession, workers = 2)
  } else {
    old_plan <- future::plan(future::sequential)
  }
  on.exit(future::plan(old_plan), add = TRUE)
  ci_se <- future::future(
    pROC::ci.se(roc,
      specificities = grads,
      conf.level = conf_level,
      boot.n = boot_n,
      progress = "none"
    ),
    seed = get_seed()
  )
  ci_sp <- future::future(
    pROC::ci.sp(roc,
      sensitivities = grads,
      conf.level = conf_level,
      boot.n = boot_n,
      progress = "none"
    ),
    seed = get_seed()
  )
  ci_se <- as.data.frame(future::value(ci_se))
  ci_sp <- future::value(ci_sp) %>%
    as.data.frame() %T>% {
      .[["sp_lower"]] <- purrr::map_dbl(
        .[[1]],
        ~ min(DescTools::Closest(grads, .x))
      )
      .[["sp_upper"]] <- purrr::map_dbl(
        .[[3]],
        ~ max(DescTools::Closest(grads, .x))
      )
    }
  list(se = ci_se, sp = ci_sp)
}

#' Get AUC info (with confidence interval) for inclusion in ROC plots.
#'
#' Construct [dplyr::tibble()] with column `AUC` and optionally columns `lower`
#' and `upper` for the confidence intervals on the AUC.
#'
#' @inheritParams pROC::roc
#' @inheritParams plot_roc
#'
#' @return A [dplyr::tibble()].
#'
#' @noRd
get_single_auc_tbl <- function(response, predictor, conf_level) {
  checkmate::assert_integerish(response,
    lower = 0, upper = 1, any.missing = FALSE
  )
  checkmate::assert_numeric(predictor,
    lower = 0, upper = 1, any.missing = FALSE
  )
  checkmate::assert_number(conf_level, na.ok = TRUE, lower = 0, upper = 1)
  roc <- suppressMessages(
    pROC::roc(
      response = response, predictor = predictor,
      direction = "<"
    )
  )
  if (isTRUE(as.logical(conf_level))) {
    ci_auroc <- pROC::ci.auc(roc, conf.level = conf_level)
    out <- dplyr::tibble(
      AUC = ci_auroc[[2]],
      lower = ci_auroc[[1]], upper = ci_auroc[[3]]
    )
  } else {
    auroc <- pROC::auc(roc)
    out <- dplyr::tibble(AUC = round(auroc, 2))
  }
  out %>%
    dplyr::mutate_if(is.double, ~ round(.x, 2)) %>%
    dplyr::bind_cols(n = length(response), .)
}
