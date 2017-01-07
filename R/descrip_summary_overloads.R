#' Minimum reflectance
#'
#' \code{min} Returns the minimum reflectance value in a spectra object
#'
#' @param ... spectra object
#' @param na.rm boolean. remove NAs? Defaults to FALSE
#' @return single numeric value
#'
#' @author meireles
#' @export
min.spectra = function(...,na.rm = FALSE){
    min(reflectance(...), na.rm = na.rm)
}

#' Maximum reflectance
#'
#' \code{max} Returns the maximum reflectance value in a spectra object
#'
#' @param ... spectra object
#' @param na.rm boolean. remove NAs? Defaults to FALSE
#' @return single numeric value
#'
#' @author meireles
#' @export
max.spectra = function(...,na.rm = FALSE){
    max(reflectance(...), na.rm = na.rm)
}

#' Range of reflectance values
#'
#' \code{range} Returns the range of (min, max) reflectance values in spectra
#'
#' @param ... spectra object
#' @param na.rm boolean. remove NAs? Defaults to FALSE
#' @return tuple of numeric values (min, max)
#'
#' @author meireles
#' @export
range.spectra = function(...,na.rm = FALSE){
    range(reflectance(...), na.rm = na.rm)
}


#' Get dimension of spectra
#'
#' \code{dim} returns a vector with number of samples and bands (wavelengths)
#'
#' @param x spectra object
#' @return tuple of integers: c("n_samples", "n_wavelengths")
#'
#' @author meireles
#' @export
dim.spectra = function(x){
    c("n_samples"     = length(names(x)),
      "n_wavelengths" = length(wavelengths(x)) )
}

#' Print spectra
#'
#' \code{print} prints basic information about the spectra obj to the console
#'
#' @param x spectra object
#' @param ... other arguments passed to print. not implemented for spectra
#' @return nothing. called for side effect
#'
#' @author meireles
#' @export
print.spectra = function(x, ...){
    r_wvl   = range(wavelengths(x))
    n_met   = names(meta(x))
    l_met   = length(n_met)
    l_max   = 3L

    if(l_met > l_max){
        n_met = c( head(n_met, l_max) , "...")    ## overwriting n_met
        l_met = paste(l_max, "of", l_met)         ## overwriting l_met
    }

    cat("spectra object", "\n")
    cat("number of samples:", nrow(x),"\n")
    cat("wavelength range: ", r_wvl[1], " to ", r_wvl[2], " (", ncol(x), " bands)" ,"\n", sep = "")

    if(is.null(n_met)){
        cat("metadata: none", "\n")
    } else {
        cat("metadata (", l_met, "): ", sep = "")
        cat(paste(n_met, collapse = ", "), sep = "")
    }
}


#' Compute spectra quantiles
#'
#' \code{quantile} computes quantiles by wavelength and returns them as `spectra`
#'
#' @param x spectra object
#' @param probs Probabilities to compute quantiles.
#'              Must be a vector of numerics between 0.0 and 1.0.
#'              Defaults to c(0.025, 0.25, 0.5, 0.75, 0.975)
#' @param ... other arguments passed to quantile. not implemented for spectra
#' @return spectra object with one spectrum for each prob
#'
#' @importFrom stats quantile
#' @author meireles
#' @export
quantile.spectra = function(x,
                            probs = c(0.025, 0.25, 0.5, 0.75, 0.975),
                            ...){

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
    f = function(x){ quantile(x,  probs) }
    y = apply(reflectance(x) , 2, f)

    ## Return spectra quantile object
    r = spectra(reflectance  = y,
                wavelengths  = wavelengths(x),
                names = probs)
    class(r) = c(class(r), "spec_quantile")
    r
}
