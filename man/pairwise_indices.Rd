% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conversion.R
\name{pairwise_indices}
\alias{pairwise_indices}
\title{Pairwise indices}
\usage{
pairwise_indices(x, max_out_elements = 5e+08)
}
\arguments{
\item{x}{spectra}

\item{max_out_elements}{maximum number of elements in the output object}
}
\value{
list that includes the *indices* between bands a and b (column names a|b)
        and the pairwise *band_combinations*
}
\description{
\code{pairwise_indices} computes pairwise spectral indices.
Indices are computed as (a - b) / (a + b) where a is the lower band.
The column names of the resulting matrix are given as "a|b".
}
\examples{
library(spectrolab)
spec  = as_spectra(spec_matrix_example, name_idx = 1)

# Resampling spectra since a spectral dataset with 2,001 bands
# results in 2,001,000 unique spectral indices per sample
spec  = resample(spec, seq(400, 2400, 2))
p_idx = pairwise_indices(spec)

}
\author{
Jose Eduardo Meireles
}
