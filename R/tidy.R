################################################################################
# Tidy / long bridge
################################################################################

#' Convert spectra to long format
#'
#' \code{to_long} reshapes a spectra object into a long (tidy) data.frame with
#' one row per sample/band combination: \code{sample_name}, \code{band},
#' \code{value}, and (optionally) the metadata columns repeated per band.
#' Dependency-free (base R only); see \code{\link{as_tibble.spectra}} for a
#' tibble version.
#'
#' @param x spectra object
#' @param metadata boolean. Include spectral metadata? Defaults to TRUE
#' @param ... additional arguments (not currently used)
#' @return long-format data.frame
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' spec = as_spectra(spec_matrix_example, name_idx = 1)
#' long = to_long(spec)
to_long = function(x, metadata = TRUE, ...){
    UseMethod("to_long")
}


#' @describeIn to_long Convert spectra to long format
#' @export
to_long.spectra = function(x, metadata = TRUE, ...){

    w  = bands(x)
    r  = value(x)
    n  = names(x)
    ns = nrow(x)
    nb = ncol(x)

    long = data.frame(sample_name = rep(n, times = nb),
                      band        = rep(w, each  = ns),
                      value       = as.vector(r),
                      stringsAsFactors = FALSE)

    if(metadata){
        m = meta(x)
        if(ncol(m) != 0){
            long = cbind(long, m[rep(seq_len(ns), times = nb), , drop = FALSE])
        }
    }

    long
}


#' Convert spectra to a tibble
#'
#' A tibble version of \code{\link{to_long}}. Requires the \code{tibble}
#' package.
#'
#' @param x spectra object
#' @param ... additional arguments passed to \code{\link{to_long}}
#' @return tibble in long format
#'
#' @author Jose Eduardo Meireles
#' @exportS3Method tibble::as_tibble
#'
#' @examples
#' library(spectrolab)
#' spec = as_spectra(spec_matrix_example, name_idx = 1)
#' if(requireNamespace("tibble", quietly = TRUE)){
#'     tbl = tibble::as_tibble(spec)
#' }
as_tibble.spectra = function(x, ...){

    if(!requireNamespace("tibble", quietly = TRUE)){
        stop("Package 'tibble' is needed for this function to work.\n",
             "Install it with: install.packages('tibble')")
    }

    tibble::as_tibble(to_long(x, ...))
}


#' Plot spectra with ggplot2
#'
#' Draws one line per sample (wavelength on the x axis, value on the y axis).
#' Requires the \code{ggplot2} package. For base-R plotting, see
#' \code{\link{plot.spectra}}.
#'
#' @param object spectra object
#' @param ... additional arguments (not currently used)
#' @return a ggplot object
#'
#' @author Jose Eduardo Meireles
#' @exportS3Method ggplot2::autoplot
#'
#' @examples
#' library(spectrolab)
#' spec = as_spectra(spec_matrix_example, name_idx = 1)
#' if(requireNamespace("ggplot2", quietly = TRUE)){
#'     ggplot2::autoplot(spec)
#' }
autoplot.spectra = function(object, ...){

    if(!requireNamespace("ggplot2", quietly = TRUE)){
        stop("Package 'ggplot2' is needed for this function to work.\n",
             "Install it with: install.packages('ggplot2')")
    }

    long = to_long(object, metadata = FALSE)

    ggplot2::ggplot(long, ggplot2::aes(x = band, y = value, group = sample_name)) +
        ggplot2::geom_line() +
        ggplot2::labs(x = "wavelength", y = "value")
}

## `band`/`value`/`sample_name` above are data-frame columns captured lazily by
## ggplot2::aes(), not undefined globals -- silence the R CMD check NOTE.
utils::globalVariables(c("band", "value", "sample_name"))
