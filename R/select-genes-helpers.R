prep_step_select_genes <- function(x, training, info) {
  # Argument checking ----------------------------------------------------------
  gene_cols <- recipes::terms_select(terms = x$terms, info = info)
  checkmate::assert_data_frame(training[gene_cols], types = "numeric")
  de <- cor_de(training,
    condition = x$condition, method = x$options$method,
    genes = gene_cols, padj_method = x$options$padj_method
  )
  good_gene_names <- de %>%
    dplyr::filter(padj <= x$padj_cutoff) %>%
    dplyr::pull(gene)
  if (!is.null(x$max_n_genes) && length(good_gene_names) > x$max_n_genes) {
    good_gene_names <- utils::head(good_gene_names, x$max_n_genes)
  }
  if (!is.null(x$min_n_genes) && length(good_gene_names) < x$min_n_genes) {
    good_gene_names <- de %>%
      utils::head(x$min_n_genes) %>%
      dplyr::pull(gene)
  }
  bad_gene_names <- dplyr::setdiff(de$gene, good_gene_names)
  step_select_genes_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    condition = x$condition,
    padj_cutoff = x$padj_cutoff,
    max_n_genes = x$max_n_genes,
    min_n_genes = x$min_n_genes,
    genes_pass = list(good = good_gene_names, bad = bad_gene_names),
    options = x$options,
    skip = x$skip,
    id = x$id
  )
}

bake_step_select_genes <- function(object, new_data) {
  checkmate::assert_data_frame(new_data)
  good_genes <- object$genes_pass$good
  if (!all(good_genes %in% names(new_data))) {
    custom_stop(
      "Not all genes found by the differential expression analysis
       were found in {code('new_data')}.",
      "An example is
       '{good_genes[which.min(good_genes %in% names(new_data))]}'."
    )
  }
  new_data %>%
    dplyr::select(-dplyr::any_of(object$genes_pass$bad)) %>%
    dplyr::as_tibble()
}

print_step_select_genes <- function(x, width = max(20, options()$width - 35)) {
  cat("Differential expression based gene selection ", sep = "")
  if (x$trained) {
    good_gene_names <- x$genes_pass$good
    sentence_end <- dplyr::if_else(
      as.logical(length(good_gene_names)),
      paste(
        dplyr::if_else(length(good_gene_names) == 1, ":", " including"),
        good_gene_names[1]
      ),
      ""
    )
    cat("selecting ", length(good_gene_names), " gene",
      dplyr::if_else(length(good_gene_names) == 1, "", "s"),
      sentence_end,
      sep = ""
    )
    cat("\n")
  } else {
    cat("on ", sep = "")
    recipes::printer(untr_obj = x$terms, trained = x$trained, width = width)
  }
  invisible(x)
}

tidy_step_select_genes <- function(x) {
  if (recipes::is_trained(x)) {
    out <- dplyr::bind_rows(
      dplyr::tibble(gene = x$genes_pass$good, pass = TRUE),
      dplyr::tibble(gene = x$genes_pass$bad, pass = FALSE)
    )
  } else {
    out <- dplyr::tibble(
      gene = recipes::sel2char(x$terms),
      pass = rlang::na_lgl
    )
  }
  out$id <- x$id
  out
}

tunable_step_select_genes <- function(x) {
  dplyr::tibble(
    name = c("padj_cutoff"),
    call_info = list(list(pkg = "mirmodels", fun = "padj_cutoff")),
    source = "recipe",
    component = "step_select_genes",
    component_id = x$id
  )
}
