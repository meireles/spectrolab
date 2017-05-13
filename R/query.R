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
#' Because an NIR dip is estimated from features of the second derivative, high
#' frequency noise in the spectra is likely to spoil the algorithm and give false
#' positives. Make sure that the spectra has been smoothed (`spectrolab::smooth`)
#' before trying to detect an NIR dip.
#'
#' @param x spectra object
#' @return boolean vector
#'
#' @author Jose Eduardo Meireles
#' @export
has_nir_dip = function(x){

    if(!is_spectra(x)){
        stop("Object must be of class spectra")
    }

    message("Make sure you have smoothed your spectra first!")

    if(any(diff(wavelengths(x)) != 1)) {
        y = spectrolab::resample(x[ , wavelengths(x, 700, 800)], 700:800)
    } else {
        y = x[ , 700:800]
    }

    z = apply(y, 1, diff, differences = 2)

    w1 = apply(z[10:50, ], 2, function(x){
        min(x) < -0.0008
    })

    w2 = apply(z[30:65, ], 2, function(x){
        any(x > 0.00005)
    })

    as.vector(w1 & w2)
}

