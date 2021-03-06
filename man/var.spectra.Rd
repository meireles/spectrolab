% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stats_function_overloads.R
\name{var.spectra}
\alias{var.spectra}
\title{Variance spectrum}
\usage{
\method{var}{spectra}(x, y = NULL, na.rm = TRUE, use)
}
\arguments{
\item{x}{spectra}

\item{y}{nothing}

\item{na.rm}{boolean. remove NAs?}

\item{use}{nothing}
}
\value{
single spectrum
}
\description{
Forces keep_txt_meta = TRUE
}
\examples{
library(spectrolab)
spec = as_spectra(spec_matrix_example, name_idx = 1)
var(spec)
}
\author{
Jose Eduardo Meireles
}
