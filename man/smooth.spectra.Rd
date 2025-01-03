% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/smooth.R
\name{smooth.spectra}
\alias{smooth.spectra}
\title{Smooth spectra}
\usage{
\method{smooth}{spectra}(x, method = "gaussian", ...)
}
\arguments{
\item{x}{spectra object. bands must be strictly increasing}

\item{method}{Choose smoothing method: "gaussian" (default), "spline", or "moving_average"}

\item{...}{additional parameters passed to methods \code{smooth_fwhm}, \code{smooth_spline}, \code{smooth_moving_avg}}
}
\value{
a spectra object of with smoothed spectra
}
\description{
\code{smooth} runs each spectrum by a smoothing and returns the spectra
}
\examples{
library(spectrolab)

spec = as_spectra(spec_matrix_example, name_idx = 1)
spec = smooth(spec)
}
\author{
Jose Eduardo Meireles
}
