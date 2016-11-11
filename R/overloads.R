#' Get dimension of spectra object
#'
#' @param spec spectra object
#'
#' @return tuple of integers. c("n_samples", "n_wavelengths")
#' @export
dim.spectra = function(spec){
    c("n_samples"     = length(spec$names),
      "n_wavelengths" = length(spec$wavelengths))
}

#' Prints spectra object
#'
#' @param spec spectra object
#'
#' @return nothing. called for side effect
#' @export
print.spectra = function(spec){
    print.default(spec)
}


#' Compute spectra quantiles by wavelength
#'
#' @param spec spectra object
#' @param probs Probabilities to compute quantiles.
#'              Must be a vector of numerics between 0.0 and 1.0.
#'              Defaults to c(0.025, 0.25, 0.5, 0.75, 0.975)
#' @return spectra object with one spectrum for each prob
quantile.spectra = function(spec,
                            probs = c(0.025, 0.25, 0.5, 0.75, 0.975) ){

    ## probs must be between 0 and 1
    if( any(probs < 0.0) || any(probs > 1.0) ){
        stop("Probs must have values between 0 and 1")
    }

    ## probs should not be duplicated
    w = ! duplicated(probs)

    if(any(!w)){
        message("Duplicated probs being excluded: ", probs[ !w ])
        probs = probs[ w ]
    }

    ## Get quantiles
    f = function(x){ quantile(x,  probs)}
    y = apply(spec$reflectance, 2, f)

    ## Return spectra quantile object
    r = spectra(reflectance  = y,
                wavelengths  = spec$wavelengths,
                names = probs)
    class(r) = c(class(r), "spec_quantile")
    r
}
