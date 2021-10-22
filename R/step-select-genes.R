#' Gene selection by differential expression analysis.
#'
#' `step_select_genes()` creates a _specification_ of a recipe step that will
#' select genes by differential expression analysis, discarding those that don't
#' pass a certain p-value threshold. Currently, [cor_de()] is used.
#'
#' @inheritParams recipes::step_pca
#' @param condition The condition for the differential expression. See
#'   [cor_de()].
#' @param padj_cutoff Genes with an adjusted p-value less than or equal to
#'   `padj_cutoff` in the differential expression analysis are kept. The rest
#'   are discarded.
#' @param max_n_genes A positive integer. The maximum number of genes selected
#'   by this step.
#' @param min_n_genes A positive integer. The minimum number of genes selected.
#'   This guarantees that even if no genes pass `padj_cutoff`, there will be
#'   this many returned.
#' @param genes_pass This should not be specified by the end user. Information
#'   about which genes do and don't pass the p-value threshold in the
#'   differential expression analysis is stored here.
#' @param options A list with two elements named `method` and `padj_method`.
#'   Both are passed to [cor_de()]. `padj_cutoff` is the threshold for keeping
#'   genes.
#'
#' @return An updated version of recipe with the new step added to the sequence
#'   of existing steps (if any).
#'
#' @export
step_select_genes <- function(recipe, ...,
                              role = NA, trained = FALSE,
                              condition = NULL, genes_pass = NULL,
                              padj_cutoff = 0.05,
                              max_n_genes = NULL,
                              min_n_genes = NULL,
                              options = list(
                                method = "spearman",
                                padj_method = stats::p.adjust.methods[1]
                              ),
                              skip = FALSE,
                              id = recipes::rand_id("select_genes")) {

  # Argument checking ----------------------------------------------------------
  checkmate::assert_class(recipe, "recipe")
  terms <- recipes::ellipse_check(...)
  # Further argument checking is done by `step_select_genes_new()` below.

  # Main body ------------------------------------------------------------------
  recipes::add_step(
    recipe,
    step_select_genes_new(
      terms = terms,
      trained = trained,
      role = role,
      condition = condition,
      padj_cutoff = padj_cutoff,
      max_n_genes = max_n_genes,
      min_n_genes = min_n_genes,
      genes_pass = genes_pass,
      options = options,
      skip = skip,
      id = id
    )
  )
}

step_select_genes_new <- function(terms, role, trained,
                                  condition, padj_cutoff,
                                  max_n_genes, min_n_genes,
                                  genes_pass,
                                  options, skip, id) {

  # Argument checking ----------------------------------------------------------
  checkmate::assert_string(role, na.ok = TRUE, min.chars = 1)
  checkmate::assert_flag(trained)
  if (is.null(condition)) {
    custom_stop("Please specify a {code('condition')} column.")
  }
  checkmate::assert_string(condition, min.chars = 1)
  if (!is.null(genes_pass)) {
    checkmate::assert_list(genes_pass, len = 2)
    checkmate::assert_names(names(genes_pass),
      permutation.of = c("good", "bad")
    )
    checkmate::assert_character(genes_pass$good, min.chars = 1, unique = TRUE)
    checkmate::assert_character(genes_pass$bad, min.chars = 1, unique = TRUE)
    if (!length(genes_pass$good)) {
      custom_stop("No good genes found.")
    }
  }
  if (!isTRUE(all.equal(padj_cutoff, tune::tune()))) {
    checkmate::assert_number(padj_cutoff, lower = 0, upper = 1)
  }
  checkmate::assert_count(max_n_genes, null.ok = TRUE)
  checkmate::assert_count(min_n_genes, null.ok = TRUE)
  checkmate::assert_list(options, names = "unique")
  checkmate::assert_names(
    names(options),
    permutation.of = c("method", "padj_method")
  )
  options$method <- strex::match_arg(options$method,
    c("pearson", "spearman", "kendall"),
    ignore_case = TRUE
  )
  checkmate::assert_flag(skip)
  checkmate::assert_string(id, min.chars = 2)

  # Main body ------------------------------------------------------------------
  recipes::step(
    subclass = "select_genes",
    terms = terms,
    role = role,
    trained = trained,
    condition = condition,
    padj_cutoff = padj_cutoff,
    max_n_genes = max_n_genes,
    min_n_genes = min_n_genes,
    genes_pass = genes_pass,
    options = options,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_select_genes <- function(x, training, info = NULL, ...) {
  prep_step_select_genes(x = x, training = training, info = info)
}

#' @export
bake.step_select_genes <- function(object, new_data, ...) {
  bake_step_select_genes(object = object, new_data = new_data)
}

#' @export
print.step_select_genes <- function(x, width = max(20, options()$width - 35),
                                    ...) {
  print_step_select_genes(x = x, width = width)
}

#' @export
tidy.step_select_genes <- function(x, ...) {
  tidy_step_select_genes(x = x)
}

#' @export
tunable.step_select_genes <- function(x, ...) {
  tunable_step_select_genes(x = x)
}

#' Adjusted p-value cutoff
#'
#' Used for specification of `padj_cutoff` values in [step_select_genes()].
#'
#' @inheritParams dials::neighbors
#'
#' @export
padj_cutoff <- function(range = c(0, 1), trans = NULL) {
  dials::new_quant_param(
    type = "double", range = range,
    inclusive = c(FALSE, FALSE), trans = trans,
    label = c(padj_cutoff = "Adjusted p-value cutoff")
  )
}
