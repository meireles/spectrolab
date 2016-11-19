#' Get dimension of spectra
#'
#' \code{dim} returns a vector with number of samples and bands (wavelengths)
#'
#' @param x spectra object
#'
#' @return tuple of integers: c("n_samples", "n_wavelengths")
#' @export
dim.spectra = function(x){
    c("n_samples"     = length(x$names),
      "n_wavelengths" = length(x$wavelengths))
}

#' Print spectra
#'
#' \code{print} prints to the console basic information about the spectra obj
#'
#' @param x spectra object
#'
#' @return nothing. called for side effect
#' @export
print.spectra = function(x){
    d = dim(x)
    r = range(wavelengths(x))
    cat("spectra object", "\n")
    cat("number of samples:", d[1],"\n")
    cat("wavelength range: ", r[1], " to ", r[2], " (", d[2], " bands)" ,"\n", sep = "")
}

#' Compute spectra quantiles
#'
#' \code{quantile} computes quantiles by wavelength and returns them as `spectra`
#'
#' @param x spectra object
#' @param probs Probabilities to compute quantiles.
#'              Must be a vector of numerics between 0.0 and 1.0.
#'              Defaults to c(0.025, 0.25, 0.5, 0.75, 0.975)
#' @return spectra object with one spectrum for each prob
quantile.spectra = function(x,
                            probs = c(0.025, 0.25, 0.5, 0.75, 0.975)){

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
    y = apply(x$reflectance, 2, f)

    ## Return spectra quantile object
    r = spectra(reflectance  = y,
                wavelengths  = x$wavelengths,
                names = probs)
    class(r) = c(class(r), "spec_quantile")
    r
}
