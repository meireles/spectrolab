#' Is it a spectra object?
#'
#' \code{is_spectra} tests if the argument is a spectra class object
#'
#' @param x any object
#' @return boolean
#'
#' @author Jose Eduardo Meireles
#' @export
is_spectra = function(x){
    inherits(x, "spectra")
}


#' Has a dip in the NIR region?
#'
#' \code{has_nir_dip} tests if spectra have a NIR dip
#'
#' @param x spectra object
#' @return boolean vector
#'
#' @author Jose Eduardo Meireles
#' @export
has_nir_dip = function(x){

    if( !is_spectra(x) ){
        stop("Object must be of class spectra")
    }

    if(any(diff(wavelengths(x)) != 1)) {
        y = spectrolab::resample(x[ , wavelengths(x, 700, 800)], 700:800)
    } else {
        y = x[ , 700:800]
    }

    z = apply(y, 1, diff, differences = 2)
    w = apply(z[30:65, ], 2, function(x){
        any(x > 0.0)
    })

    as.vector(w)
}
