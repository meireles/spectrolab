#' Is it a spectra object?
#'
#' \code{is_spectra} tests if the argument is a spectra class object
#'
#' @param x any object
#' @return boolean
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' spec  = as.spectra(spec_matrix_example)
#' spec1 = unclass(spec)
#' is_spectra(spec)
#' is_spectra(spec1)
is_spectra = function(x){
    inherits(x, "spectra")
}


#' Has a dip in the NIR region?
#'
#' \code{has_nir_dip} tests if spectra have a NIR dip
#'
#' Because the NIR dip is detected using features on the second derivative, high
#' frequency noise in the spectra is likely to spoil the method and give false
#' positives. Ideally, spectra should have been been smoothed (`spectrolab::smooth`)
#' before trying to detect an NIR dip. If param `smooth` is set to TRUE (default),
#' `has_nir_dip` does some basic smoothing internally.
#'
#' @param x spectra object
#' @param smooth boolean. Smooth spectra internally? Defaults to TRUE
#' @param ... additional parameters passed to `spectrolab::smooth` (method spline)
#' @return boolean vector
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' spec  = as.spectra(spec_matrix_example)
#'
#' # Find spec with NIR dip
#' # If there many samples need testing, set parallel = TRUE
#' dip = has_nir_dip(spec, parallel = FALSE)
#'
#' # remove NIR dip
#' spec = spec[ ! dip, ]
has_nir_dip = function(x, smooth = TRUE, ...){

    if(!is_spectra(x)){
        stop("Object must be of class spectra")
    }

    if(smooth){
        x = suppressMessages(smooth(x, method = "spline", ...))
    }

    w = wavelengths(x)

    if(any(diff(w) != 1)){
        x = suppressMessages(resample(x, seq.int(min(w), max(w), 1L)))
    }

    x = suppressMessages(spectrolab::normalize(x)[ , 700 : 800])
    z = apply(x, 1, diff, differences = 2)

    w = apply(z[30:98, ], 2, function(x){
        any(x > 3e-06)
    })

    as.vector(w)
}
