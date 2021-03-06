% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/correct.R
\name{linear_correct}
\alias{linear_correct}
\title{Correct data for the effects of selected covariates.}
\usage{
linear_correct(
  training_data,
  testing_data = NULL,
  correct_cols,
  correct_for_cols,
  keep_effect_cols = NULL,
  robust = TRUE
)
}
\arguments{
\item{training_data}{A data frame containing one sample per row. The
correction is learned and applied on this data.}

\item{testing_data}{A data frame. The testing counterpart to \code{training_data}.
The correction that is learned on \code{training_data} is applied here. It's
fine to pass this argument as \code{NULL}, in which case no testing data
correction takes place.}

\item{correct_cols}{A character vector. The names of the columns that are to
be altered (corrected). These columns must all be numeric.}

\item{correct_for_cols}{A character vector. The names of the columns that are
to be corrected \emph{for}. These columns must all be numeric, factor or
logical.}

\item{keep_effect_cols}{A character vector. The names of the column
specifying variables whose effects should \emph{not} be regressed away. These
columns must all be numeric, factor or logical. If there are no columns
whose effects should \emph{not} be regressed away, pass this argument as \code{NULL}.}

\item{robust}{A flag. Use robust linear model \code{\link[MASS:rlm]{MASS::rlm()}}? Can only be used
with \code{type = 1}.}
}
\value{
A list with elements named \code{training_data} and \code{testing_data}. The
corrected data.
}
\description{
This function uses linear models to regress away the effects of selected
covariates on selected columns of a data frame. One may optionally specify
variables whose effects are considered \emph{real} or \emph{of interest} and their
effects will not be regressed away (only effects orthogonal to those will be
regressed away).
}
\details{
If \code{keep_effect_cols} is \code{NULL}, this function is just a wrapper around
\code{\link[=multi_lm]{multi_lm()}} and \code{\link[=multi_resids]{multi_resids()}} with \code{reset_mean_med = TRUE}. That is, for
each variable in \code{correct_cols}, a linear model is fit with the variables
\code{correct_for_cols} as explanatory variables. Then the residuals from this
model (reset about their original mean or median) are kept as the \emph{corrected}
values of those variables.

If \code{keep_effect_cols} is not \code{NULL}, then first, for each variable in
\code{correct_cols}, a linear model is fit with the variables \code{keep_effect_cols}
as explanatory variables. The fitted variables from these models are
remembered as the effects of these \code{keep_effect_cols} on \code{correct_cols}. The
residuals from these models are then components of \code{correct_cols} which can't
be explained by \code{keep_effect_cols}. With these residuals, the effects of
\code{correct_for_cols} are regressed away, and what remains is added onto the
fitted values from the modelling of \code{correct_cols} with \code{keep_effect_cols}.

Columns in \code{training_data} that are not specified in \code{correct_cols},
\code{correct_for_cols}, or \code{keep_effect_cols} will be returned unchanged.
}
\examples{
if (rlang::is_installed("mirmisc")) {
  data <- get_combined_cohort_data(
    c("bw", "ga", "io", "kl", "pm", "pt", "rs"),
    cpm = FALSE, log2 = TRUE, tot_counts = TRUE,
    gene_predicate = ~ median(.) > 0
  )
  res <- linear_correct(
    data,
    correct_cols = mirmisc::get_df_gene_names(data),
    correct_for_cols  = c("log2_tot_counts"),
    keep_effect_cols = "meta_collectionga"
  )
}
}
