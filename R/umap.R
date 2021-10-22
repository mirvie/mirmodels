#' Compute UMAPs.
#'
#' Computation is be done using the `embed` package.
#'
#' @param df A data frame.
#' @param num_comp A positive integer. The number of UMAP components to compute.
#' @param subset A character vector. The subset of columns to use for UMAP
#'   calculation. Default is all columns. Columns in `df` outside of this subset
#'   will be kept in the result.
#'
#' @return A data frame with the UMAP components.
#'
#' @examples
#' if (rlang::is_installed("mirmisc")) {
#'   ga_data <- get_ga_data(
#'     log2 = TRUE,
#'     gene_predicate = ~ median(.) > 0
#'   )
#'   genes <- dplyr::intersect(names(ga_data), mirmisc::get_gene_names())
#'   umap <- compute_umap(ga_data, subset = genes)
#'   ggplot2::ggplot(
#'     umap,
#'     ggplot2::aes(umap_1, umap_2, color = meta_collectionga)
#'   ) +
#'     ggplot2::geom_point() +
#'     ggplot2::scale_color_viridis_c()
#' }
#' @export
compute_umap <- function(df, num_comp = 2, subset = NULL) {
  c(num_comp, df_subset) %<-% argchk_compute_pcas(
    df = df,
    num_comp = num_comp,
    subset = subset,
    normalize = FALSE,
    robust = FALSE,
    crit_pca_distances = 0.999
  )[c("num_comp", "df_subset")]
  rec <- recipes::recipe(df_subset) %>%
    recipes::update_role(dplyr::everything(), new_role = "predictor") %>%
    embed::step_umap(recipes::all_predictors(), num_comp = num_comp)
  prepped_rec <- recipes::prep(rec)
  dplyr::bind_cols(
    recipes::juice(prepped_rec),
    dplyr::select(df, -dplyr::all_of(subset))
  )
}
