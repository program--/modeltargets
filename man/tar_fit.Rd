% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tar_fit.R
\name{tar_fit}
\alias{tar_fit}
\title{Fitting Target}
\usage{
tar_fit(
  name,
  object,
  formula,
  data,
  control = parsnip::control_parsnip(),
  ...,
  tar_args = list()
)
}
\arguments{
\item{name}{Name of target}

\item{object}{An object of class \code{model_spec} that has a chosen engine
(via \code{\link[parsnip:set_engine]{set_engine()}}).}

\item{formula}{An object of class "formula" (or one that can
be coerced to that class): a symbolic description of the model
to be fitted.}

\item{data}{Optional, depending on the interface (see Details
below). A data frame containing all relevant variables (e.g.
outcome(s), predictors, case weights, etc). Note: when needed, a
\emph{named argument} should be used.}

\item{control}{A named list with elements \code{verbosity} and
\code{catch}. See \code{\link[parsnip:control_parsnip]{control_parsnip()}}.}

\item{...}{Not currently used; values passed here will be
ignored. Other options required to fit the model should be
passed using \code{set_engine()}.}

\item{tar_args}{Arguments passed to \code{tar_target}.}
}
\value{
A \code{tar_target} object.
}
\description{
Fitting Target
}
