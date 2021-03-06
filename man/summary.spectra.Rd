% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/descrip_summary_overloads.R
\name{summary.spectra}
\alias{summary.spectra}
\title{Summarize spectra}
\usage{
\method{summary}{spectra}(object, ...)
}
\arguments{
\item{object}{spectra object}

\item{...}{additional params to summary. not used yet}
}
\value{
nothing yet (just prints to console)
}
\description{
Summarize spectra
}
\examples{
library(spectrolab)
spec = as_spectra(spec_matrix_example, name_idx = 1)
summary(spec)
}
\author{
Jose Eduardo Meireles
}
