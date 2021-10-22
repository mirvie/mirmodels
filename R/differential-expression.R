#' Get median levels of genes for baseline and cases.
#'
#' @param data A data frame.
#' @param condition A string. The name of the column holding the condition variable, which should be logical or a two-level factor.
#' @param genes The names of the columns holding the gene values.
#'
#' @return A tibble. Columns are `genes`, `base_med` and `case_med`.
#'
#' @noRd
get_gene_medlevels <- function(data, condition, genes) {
  checkmate::assert_data_frame(data, min.rows = 2, min.cols = 2)
  checkmate::assert_string(condition, min.chars = 1)
  checkmate::assert_names(condition, subset.of = names(data))
  checkmate::assert_factor(data[[condition]],
    any.missing = FALSE,
    empty.levels.ok = FALSE, n.levels = 2
  )
  checkmate::assert_character(genes,
    min.chars = 1, unique = TRUE,
    any.missing = FALSE
  )
  checkmate::assert_names(genes, subset.of = names(data))
  data_genes <- data[genes]
  checkmate::assert_data_frame(data_genes, types = "numeric")

  data_genes1 <- data_genes[data[[condition]] == levels(data[[condition]])[1], ]
  data_genes2 <- data_genes[data[[condition]] == levels(data[[condition]])[2], ]
  data_genes1_meds <- matrixStats::colMedians(data.matrix(data_genes1))
  data_genes2_meds <- matrixStats::colMedians(data.matrix(data_genes2))
  data_genes1_means <- matrixStats::colMeans2(data.matrix(data_genes1))
  data_genes2_means <- matrixStats::colMeans2(data.matrix(data_genes2))
  dplyr::tibble(
    gene = genes,
    base_med = data_genes1_meds,
    case_med = data_genes2_meds,
    base_mean = data_genes1_means,
    case_mean = data_genes2_means
  )
}

#' Conduct a differential expression analysis using [DESeq2::DESeq()].
#'
#' This function is designed to ingest data in the form output by functions like
#' [get_bw_data()].
#'
#' @param data A data frame where rows are samples and columns are genes and
#'   metadata.
#' @param condition A string. The name of the column in `data` which contains
#'   the condition on which the differential expression is premised. This column
#'   should be logical or a factor with two levels. The first level (or `FALSE`)
#'   is the baseline and the second level (or `TRUE`) is the disease/treatment.
#' @param batch A string. The name of the column in `data` specifying the
#'   batches that you'd like the model to account for. A common choice is a
#'   cohort column.
#' @param genes A character vector. The column names that contain the genes for
#'   the differential expression. If `NULL`, any genes in data found in
#'   [mirmisc::get_gene_names()] are used.
#' @param shrink A flag. If `FALSE` (the default), [DESeq2::results()] will be
#'   used. Otherwise, [DESeq2::lfcShrink()] will be used.
#' @param padj_method A string. The method of adjusting the p-values for
#'   multiple hypothesis testing. Must be one of [stats::p.adjust.methods()].
#' @param quick A flag. If `TRUE`, the `base_med`, `case_med`, `base_mean` and
#'   `case_mean` columns are omitted from the return and the lack of need to
#'   calculate these medians offers a small speedup.
#' @param n_cores The number of cores for parallel processing.
#' @param quiet A flag. Suppresses messages.
#'
#' @return `r roxy_de_return()`
#'
#' @family differential expression methods
#'
#' @examples
#' bwms_data <- get_combined_cohort_data(
#'   c("bw", "ms"),
#'   gene_predicate = ~ stats::median(.) > 0
#' ) %>%
#'   dplyr::filter(!is.na(meta_pre_eclampsia))
#' deseq(bwms_data, "meta_pre_eclampsia", batch = "cohort")
#' @export
deseq <- function(data, condition, batch = NULL, genes = NULL, shrink = FALSE,
                  padj_method = stats::p.adjust.methods[1],
                  quick = FALSE, n_cores = 1, quiet = FALSE) {
  # Argument checking
  c(data, genes, padj_method, n_cores) %<-% argchk_deseq(
    data = data, condition = condition, batch = batch, genes = genes,
    shrink = shrink, padj_method = padj_method, quick = quick,
    n_cores = n_cores, quiet = quiet
  )[c("data", "genes", "padj_method", "n_cores")]

  # Parallel setup -------------------------------------------------------------
  init_dopar_workers <- foreach::getDoParWorkers()
  on.exit(doParallel::registerDoParallel(init_dopar_workers), add = TRUE)
  doParallel::registerDoParallel(n_cores)

  # Main body
  sample_names <- seq_len(nrow(data)) %>%
    paste0("sample", .) %>%
    strex::str_alphord_nums()
  if ("mirvie_id" %in% names(data)) sample_names <- data$mirvie_id
  cts <- data[genes] %>%
    purrr::map_dfc(~ as.integer(round(.))) %>%
    as.matrix() %>%
    magrittr::set_rownames(sample_names) %>%
    t()
  if (is.null(batch)) {
    coldata <- janitor::clean_names(data[condition])
    design <- stats::as.formula(paste("~", names(coldata)))
  } else {
    coldata <- janitor::clean_names(data[c(batch, condition)])
    design <- stats::as.formula(
      paste("~", paste(names(coldata), collapse = "+"))
    )
  }
  dataset <- DESeq2::DESeqDataSetFromMatrix(
    countData = cts,
    colData = coldata,
    design = design
  )
  deseqed <- DESeq2::DESeq(dataset,
    quiet = quiet,
    parallel = n_cores > 1,
    BPPARAM = BiocParallel::DoparParam()
  )
  if (shrink) {
    out <- DESeq2::lfcShrink(
      deseqed,
      coef = dplyr::last(DESeq2::resultsNames(deseqed)),
      parallel = n_cores > 1,
      BPPARAM = BiocParallel::DoparParam(),
      quiet = quiet
    )
  } else {
    out <- DESeq2::results(deseqed,
      parallel = n_cores > 1,
      BPPARAM = BiocParallel::DoparParam()
    )
  }
  out <- dplyr::as_tibble(out, rownames = "gene") %>%
    dplyr::arrange(pvalue, gene) %>%
    dplyr::transmute(
      gene = gene,
      log2fc = log2FoldChange,
      pvalue = pvalue,
      padj = stats::p.adjust(pvalue, method = padj_method)
    )
  if (!quick) {
    out_more <- get_gene_medlevels(data, condition = condition, genes = genes)
    out <- dplyr::left_join(out, out_more, by = "gene")
  }
  out
}

#' Conduct a differential expression analysis using [DESeq2::DESeq()].
#'
#' This function is designed to ingest data in the form output by functions like
#' [get_bw_data()].
#'
#' @inheritParams deseq
#'
#' @return `r roxy_de_return()`
#'
#' @family differential expression methods
#'
#' @examples
#' bw_data <- get_bw_data()
#' edger(bw_data, "meta_pre_eclampsia")
#' @export
edger <- function(data, condition, batch = NULL, genes = NULL, quick = FALSE,
                  padj_method = stats::p.adjust.methods[1]) {
  # Argument checking
  c(data, genes, padj_method) %<-% argchk_deseq(
    data = data, condition = condition, batch = batch,
    genes = genes, quick = quick, shrink = FALSE,
    padj_method = padj_method, n_cores = 1, quiet = FALSE
  )[c("data", "genes", "padj_method")]

  # Main body
  sample_names <- seq_len(nrow(data)) %>%
    paste0("sample", .) %>%
    strex::str_alphord_nums()
  if ("mirvie_id" %in% names(data)) sample_names <- data$mirvie_id
  cts <- data[genes] %>%
    purrr::map_dfc(~ as.integer(round(.))) %>%
    as.matrix() %>%
    magrittr::set_rownames(sample_names) %>%
    t()
  group <- data[[condition]]
  if (is.null(batch)) {
    design <- stats::model.matrix(~group)
  } else {
    design <- stats::model.matrix(
      stats::as.formula(paste("~", batch, "+", condition)),
      data = data
    )
  }
  dge_list <- suppressMessages(
    edgeR::DGEList(counts = cts, group = group, remove.zeros = TRUE)
  )
  keep <- edgeR::filterByExpr(dge_list, design = design)
  dge_list <- dge_list[keep, , keep.lib.sizes = FALSE]
  dge_list <- edgeR::calcNormFactors(dge_list)
  dge_list <- edgeR::estimateDisp(dge_list, design = design)
  fit <- edgeR::glmQLFit(dge_list, design = design)
  qlf <- edgeR::glmQLFTest(fit)
  out <- suppressWarnings(
    edgeR::topTags(qlf, n = .Machine$integer.max) %>%
      .[["table"]] %>%
      dplyr::as_tibble(rownames = "gene")
  ) %>%
    dplyr::transmute(
      gene = gene,
      log2fc = log2(exp(logFC)),
      pvalue = PValue,
      padj = stats::p.adjust(pvalue, method = padj_method)
    )
  if (!quick) {
    out_more <- get_gene_medlevels(data, condition = condition, genes = genes)
    out <- dplyr::left_join(out, out_more, by = "gene")
  }
  out
}

#' Conduct a differential expression analysis using correlation tests.
#'
#' This function is designed to ingest data in the form output by functions like
#' [get_bw_data()].
#'
#' This function has no way of dealing with batch effects. To allow for these,
#' you should correct your data (with e.g. [linear_correct()])
#' beforehand.
#'
#' @param condition A string. The name of the column in `data` which contains
#'   the condition on which the differential expression is premised. This column
#'   should be logical, a factor with two levels or a numeric vector. The first
#'   level (or `FALSE`) is the baseline and the second level (or `TRUE`) is the
#'   disease/treatment. If the condition is numeric, then columns `log2fc`,
#'   `base_med`, `case_med`, `base_mean` and `case_mean` will be absent from the
#'   result.
#' @inheritParams deseq
#' @param method A string. The correlation test method. Must be `"spearman"`,
#'   `"kendall"` or `"pearson"`.
#'
#' @return `r roxy_de_return()` For `method = "pearson"` the `log2fc` is of the
#'   means of the two groups. Otherwise, it is of the medians.
#'
#' @family differential expression methods
#'
#' @examples
#' if (requireNamespace("mirmisc", quietly = TRUE)) {
#'   bwms_data <- get_combined_cohort_data(
#'     c("bw", "ms"),
#'     log2 = TRUE, tot_counts = TRUE,
#'     gene_predicate = ~ stats::median(.) > 0,
#'   ) %>%
#'     dplyr::filter(!is.na(meta_pre_eclampsia)) %>%
#'     dplyr::mutate(tot_counts = log2(tot_counts + 1))
#'   genes <- dplyr::intersect(mirmisc::get_gene_names(), names(bwms_data))
#'   bwms_corr <- linear_correct(
#'     bwms_data,
#'     correct_cols = genes,
#'     correct_for_cols = c("log2_tot_counts", "cohort",
#'                          "meta_q_pcr_actb_ct", "meta_q_pcr_ercc_ct"),
#'     keep_effect_cols = "meta_pre_eclampsia"
#'   )[[1]]
#'   cor_de(bwms_corr, "meta_pre_eclampsia")
#' }
#' @export
cor_de <- function(data, condition, genes = NULL,
                   method = c("spearman", "kendall", "pearson"),
                   padj_method = stats::p.adjust.methods[1]) {
  # Argument checking ----------------------------------------------------------
  c(data, genes, padj_method) %<-% argchk_cor_de(
    data = data, condition = condition, genes = genes,
    padj_method = padj_method
  )[c("data", "genes", "padj_method")]
  method <- strex::match_arg(method, ignore_case = TRUE)

  # Main body ------------------------------------------------------------------
  if (is.factor(data[[condition]])) {
    cond <- as.integer(data[[condition]])
    numeric_cond <- FALSE
  } else {
    cond <- data[[condition]]
    numeric_cond <- TRUE
  }
  out <- rlang::with_handlers( # muffle certain warnings
    purrr::map(
      data[genes],
      function(x, y, method) {
        pval <- stats::cor.test(x, y, method = method)$p.value
        list(pvalue = pval)
      },
      y = cond, method = method
    ),
    warning = rlang::calling(
      function(cnd) {
        cnd_msg <- conditionMessage(cnd)
        muffle <- stringr::str_detect(
          cnd_msg,
          c(
            "standard deviation is zero", "NaNs produced",
            "Cannot compute exact p-value with ties"
          )
        ) %>%
          any()
        if (muffle) rlang::cnd_muffle(cnd)
      }
    )
  ) %>%
    purrr::map_dfr(tibble::new_tibble, nrow = 1) %>%
    dplyr::mutate(
      gene = genes,
      padj = stats::p.adjust(pvalue, method = padj_method)
    )
  if (numeric_cond) {
    out <- out %>%
      dplyr::select(gene, pvalue, padj)
  } else {
    out <- out %>%
      dplyr::left_join(get_gene_medlevels(data, condition, genes),
        by = "gene"
      ) %>%
      dplyr::mutate(log2fc = suppressWarnings(log2(case_med / base_med))) %>%
      dplyr::select(
        gene, log2fc, pvalue, padj,
        base_med, case_med, base_mean, case_mean
      )
  }
  dplyr::arrange(out, pvalue, gene)
}
