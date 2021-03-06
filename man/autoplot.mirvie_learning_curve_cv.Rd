% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/learning-curve.R
\name{autoplot.mirvie_learning_curve_cv}
\alias{autoplot.mirvie_learning_curve_cv}
\title{Produce a learning curve plot from a
\link[=new_mirvie_learning_curve_cv]{mirvie_learning_curve} object.}
\usage{
\method{autoplot}{mirvie_learning_curve_cv}(object, metric = NULL, smooth = FALSE, meansd = FALSE, ...)
}
\arguments{
\item{object}{A \link[=new_mirvie_learning_curve_cv]{mirvie_learning_curve_cv}
object (i.e. the output of a call to \code{\link[=learn_curve_cv]{learn_curve_cv()}}).}

\item{metric}{A string. The metric used to evaluate the performance.}

\item{smooth}{A flag. Use a loess smoothed line instead of joining the dots?}

\item{meansd}{A flag. If there are multiple repeats, rather than plotting all
of them, plot means with standard deviation error bars?}

\item{...}{Arguments passed to \code{\link[ggplot2:autoplot]{ggplot2::autoplot()}}. Safe to ignore.}
}
\value{
A \code{\link[ggplot2:ggplot]{ggplot2::ggplot()}}.
}
\description{
This is a method for \code{\link[ggplot2:autoplot]{ggplot2::autoplot()}}.
}
\examples{
data("BostonHousing", package = "mlbench")
bh <- dplyr::select_if(BostonHousing, is.numeric)
mod <- parsnip::linear_reg(penalty = 0, mixture = 0) \%>\%
  parsnip::set_engine("lm")
wf <- workflows::workflow() \%>\%
  workflows::add_formula(medv ~ .) \%>\%
  workflows::add_model(mod)
metric_calculator <- ~ yardstick::mae(., medv, .pred)$.estimate
lccv <- suppressWarnings(
  learn_curve_cv(bh, wf, 2:9, 8, metric_calculator, n_cores = 4)
)
autoplot(lccv, metric = "mae")
autoplot(lccv, metric = "mae", smooth = TRUE)
autoplot(lccv, metric = "mae", meansd = TRUE)
autoplot(lccv, metric = "mae", smooth = TRUE, meansd = TRUE)
}
