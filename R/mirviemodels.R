#' @importFrom utils globalVariables
#' @importFrom magrittr '%T>%'
#' @importFrom xgboost xgb.gblinear.history
#' @importFrom rlang '%||%' ':='
#' @importFrom zeallot '%<-%'
NULL


## quiets concerns of R CMD check due to tidy evaluation
if (getRversion() >= "2.15.1") {
  globalVariables(
    c(
      ".", ".x", ".y", "cv", "test", "training_frac", "testing_frac", "score",
      "scoresd", "type", "lower", "upper", "role", "pctvarexp", "p.value",
      "possible_testing_indices_randomized", "testing_indices", "quantile",
      "possible_training_indices_randomized", "training_frac", "med_pctvarexp",
      "training_indices", "trained_models", "n", "x", "y", "i", "sumsq", "term",
      "mirvie_id", "lm_i", "variable", "std_err", ".metric", ".estimator",
      ".data", "juiced_rec", "fam", "keep_effect_cols", "Importance",
      "importance", "Variable", "pvalue", "gene", "log2fc", "padj",
      "log2FoldChange", "gene", "logFC", "PValue", "specificity", "sensitivity",
      "spec", "sens", "sens_lower", "sens_upper", "model", "response",
      "predictor", "AUC", "tbl", "pval", "label", "base_med", "case_med",
      "strat", "training_samples", "repeat_num", "score_sd", "ACTB",
      "meta_collectionga", "meta_deliveryga", "base_mean", "case_mean",
      "df_subset", "train", "gene_id", "transcript_biotype"
    )
  )
}
