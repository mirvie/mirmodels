#' Compute principal components.
#'
#' Computation can be done using the [recipes][recipes::recipes] package
#' (`robust = FALSE`, uses regular PCA) or with the [rrcov][rrcov::PcaGrid]
#' package which uses [rrcov::PcaGrid()], a robust PCA computation with built-in
#'  outlier identification.
#'
#' @param df A data frame.
#' @param num_comp A positive integer. The number of PCA components to compute.
#' @param subset A character vector. The subset of columns to use for PCA
#'   calculation. Default is all columns. Columns in `df` outside of this subset
#'   will be kept in the result.
#' @param normalize A flag. Center and scale before PCA calculation?
#' @param robust A flag. Use robust PCA (from [rrcov::PcaGrid()])?
#' @param crit_pca_distances A number between 0 and 1. Outlier identification
#'  parameter. See [rrcov::PcaGrid()]. The larger this number, the fewer
#'  outliers identified.
#'
#' @return A data frame with the principal components with attributes
#' * `var_exp`: A numeric vector. The variance explained by each component.
#' * `loadings`: A data frame. The contribution of each variable to each
#'   principal component.
#' * `outlier`: A logical vector with length `nrow(df)`. `TRUE` for outlier,
#'   `FALSE` otherwise. This is present with `robust = TRUE` only.
#'
#' @examples
#' if (rlang::is_installed("mirmisc")) {
#'   ga_data <- get_ga_data(
#'     log2 = TRUE,
#'     gene_predicate = ~ median(.) > 0
#'   )
#'   pca_reg <- compute_pcas(ga_data,
#'     subset = intersect(
#'       mirmisc::get_gene_names(),
#'       names(ga_data)
#'     ),
#'     robust = FALSE
#'   )
#'   pca_rob <- compute_pcas(ga_data,
#'     subset = intersect(
#'       mirmisc::get_gene_names(),
#'       names(ga_data)
#'     ),
#'     robust = TRUE
#'   )
#'   ggplot2::ggplot(
#'     pca_reg,
#'     ggplot2::aes(PC1, PC2, color = meta_collectionga)
#'   ) +
#'     ggplot2::geom_point() +
#'     ggplot2::scale_color_viridis_c() +
#'     ggplot2::ggtitle("GAPPS regular")
#'   ggplot2::ggplot(
#'     pca_rob,
#'     ggplot2::aes(PC1, PC2, color = meta_collectionga)
#'   ) +
#'     ggplot2::geom_point() +
#'     ggplot2::scale_color_viridis_c() +
#'     ggplot2::ggtitle("GAPPS robust")
#' }
#' @export
compute_pcas <- function(df, num_comp = 5, subset = NULL, normalize = TRUE,
                         robust = FALSE, crit_pca_distances = 0.999) {
  c(num_comp, df_subset) %<-% argchk_compute_pcas(
    df = df,
    num_comp = num_comp,
    subset = subset,
    normalize = normalize,
    robust = robust,
    crit_pca_distances = crit_pca_distances
  )[c("num_comp", "df_subset")]
  if (normalize) {
    df_subset <- purrr::map_dfc(janitor::remove_constant(df_subset), scale)
  }
  if (robust) {
    pca_grid <- rrcov::PcaGrid(df_subset,
      k = min(dim(df_subset) - 1),
      crit.pca.distances = crit_pca_distances
    )
    out <- structure(
      dplyr::bind_cols(
        dplyr::as_tibble(pca_grid@scores)[seq_len(num_comp)],
        dplyr::select(df, -dplyr::all_of(subset))
      ),
      var_exp = vecsum1(pca_grid@eigenvalues)[seq_len(num_comp)],
      loadings = dplyr::bind_cols(
        dplyr::tibble(var_name = names(df_subset)),
        dplyr::as_tibble(pca_grid@loadings[, seq_len(num_comp)])
      ),
      outlier = !pca_grid@flag
    )
  } else {
    rec <- recipes::recipe(df_subset) %>%
      recipes::update_role(dplyr::everything(), new_role = "predictor") %>%
      recipes::step_pca(recipes::all_predictors(), num_comp = num_comp)
    prepped_rec <- recipes::prep(rec)
    sdev <- prepped_rec$steps[[1]]$res$sdev
    rotation <- prepped_rec$steps[[1]]$res$rotation
    out <- structure(
      dplyr::bind_cols(
        recipes::juice(prepped_rec),
        dplyr::select(df, -dplyr::all_of(subset))
      ),
      var_exp = vecsum1(sdev^2)[seq_len(num_comp)],
      loadings = dplyr::as_tibble(rotation[, seq_len(num_comp)],
        rownames = "var_name"
      )
    )
  }
  out
}
