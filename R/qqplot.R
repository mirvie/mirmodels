#' QQ-plot a vector of p-values.
#'
#' @param x A numeric vector with elements in (0, 1).
#' @param labels A character vector of length at most `length(x)`. Labels for
#'   the smallest p-values.
#' @param controls Number of controls in the tests.
#' @param cases Number of cases in the tests.
#' @param lambda_correct A flag. Apply a correction to make the slope of the
#'   fitted line equal to 1.
#' @param aes_opts A list. Aesthetic options for the plot. The list has two 
#'   named elements.
#'   * `null_alpha`: Transparency of the `x = y` line.
#'   * `fit_alpha`: Transparency of the fit line.
#'
#' @return A [ggplot2::ggplot()].
#'
#' @examples
#' set.seed(1)
#' pvals <- runif(999) ^ (4 / 3)
#' qqplot_pvals(pvals,
#'   controls = 500, cases = 499,
#'   aes_opts = list(null_alpha = 0.5, fit_alpha = 0.5)
#' )
#' qqplot_pvals(pvals,
#'   labels = c("a", "b", "c"),
#'   controls = 500, cases = 499,
#'   lambda_correct = TRUE
#' )
#' @export
qqplot_pvals <- function(x, labels = NULL, controls = NULL, cases = NULL,
                         lambda_correct = FALSE,
                         aes_opts = list(null_alpha = 1, fit_alpha = 1)) {
  
  # Argument checking ----------------------------------------------------------
  checkmate::assert_numeric(x, lower = 0, upper = 1, min.len = 9)
  checkmate::assert_character(labels,
    min.chars = 1, min.len = 1,
    null.ok = TRUE
  )
  checkmate::assert_int(controls, null.ok = TRUE)
  checkmate::assert_int(cases, null.ok = TRUE)
  checkmate::assert_flag(lambda_correct)
  checkmate::assert_list(aes_opts, names = "named")
  aes_opts$null_alpha <- aes_opts$null_alpha %||% 1
  aes_opts$fit_alpha <- aes_opts$fit_alpha %||% 1
  
  # Main body ------------------------------------------------------------------
  x <- sort(x)
  expected <- RcppRoll::roll_mean(seq(0, 1, length.out = length(x) + 1), n = 2)
  df <- dplyr::tibble(pval = x, expected = expected) %>%
    dplyr::mutate_if(is.numeric, ~ log10(.))
  if (lambda_correct) {
    slope <- MASS::rlm(pval ~ 0 + expected, data = df) %>% 
      stats::coef() %>% 
      getElement("expected")
    x <- x ^ (1 / slope)
    return(
      qqplot_pvals(x = x, labels = labels, controls = controls, cases = cases,
                   lambda_correct = FALSE, aes_opts = aes_opts)
    )
  }
  df <- dplyr::mutate_if(df, is.numeric, ~ -.x)
  if (is.null(labels)) {
    base_gg <- ggplot2::ggplot(df, ggplot2::aes(expected, pval))
  } else {
    if (is.character(labels) && length(labels) < length(x)) {
      labels <- c(labels, character(length(x) - length(labels)))
    }
    checkmate::assert_character(labels, len = length(x), any.missing = FALSE)
    df$label <- labels
    base_gg <- ggplot2::ggplot(df, ggplot2::aes(expected, pval, label = label))
  }
  out <- base_gg +
    ggplot2::stat_smooth(ggplot2::aes(color = "fit"),
                         geom = "line",
                         method = MASS::rlm, formula = y ~ 0 + x,
                         alpha = aes_opts$fit_alpha
    ) +
    ggplot2::geom_segment(
      data = df[1, ],
      ggplot2::aes(
        x = 0, y = 0, xend = max(expected), yend = max(expected),
        color = "expected"
      ),
      linetype = "dotted", alpha = aes_opts$null_alpha
    ) +
    ggplot2::geom_point()
  if (!is.null(labels)) out <- out + ggrepel::geom_label_repel()
  out <- out +
    ggplot2::scale_color_manual(
      name = "line",
      values = c(expected = "blue", fit = "red")
    ) +
    ggplot2::xlab("expected p-values (-log10)") +
    ggplot2::ylab("observed p-values (-log10)") +
    ggplot2::coord_fixed()
  if (length(cases + controls)) {
    tbl <- dplyr::tibble(
      x = max(df$expected), y = 0,
      tbl = list(
        dplyr::tibble(n = controls + cases, controls = controls, cases = cases)
      )
    )
    out <- out + ggpmisc::geom_table(ggplot2::aes(x = x, y = y, label = tbl),
      data = tbl, hjust = 1, vjust = 0
    )
  }
  out
}
