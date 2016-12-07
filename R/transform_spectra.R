#' Vector normalize spectra
#'
#' \code{normalize} returns a spectra obj with vector normalized reflectances
#'
#' @param x spectra object to be vector normalized
#' @param ... nothing
#'
#' @return spectra object with normalized spectra
#' @export
normalize = function(x, ...){
    UseMethod("normalize")
}


#' @describeIn normalize Vector normalize spectra
#' @export
normalize.spectra = function(x, ...){

    refl            = reflectance(x)
    refl_squared    = refl * refl
    vec_ones        = rep.int(1L, ncol(refl_squared))
    spec_sq_rowsum  = refl_squared %*% vec_ones
    magnitudes      = as.vector(sqrt(spec_sq_rowsum))

    # normalize and construct a `spectra` object
    x[] = i_reflectance(refl / magnitudes)

    # TODO
    # add a magnitute attribute to the`spectra` object
    # it should go to the metadata data.frame...

    # return
    x
}



#' Smooth spline functions for spectra
#'
#' Gets spline functions for each spectrum in a spectra object
#'
#' @param x spectra object
#' @param ... additional parameters passed to smooth.spline except nknots, which
#'            is computed internally
#'
#' @return a list of spline functions
i_smooth_spline_spectra = function(x, ...){
    if( !is_spectra(x) ){
        stop("Object must be of class spectra")
    }

    scale   = c(0.1, 0.25, 0.5)
    cutres  = 100

    range   = diff(range( wavelengths(x) ))
    resol   = ceiling(range / ncol(x))
    fullres = floor(range / resol)
    propres = floor(range / resol * scale)
    nknots  = min( propres[propres >= cutres], fullres)

    apply(X      = reflectance(x),
          MARGIN = 1,
          FUN    = smooth.spline,
          x      = wavelengths(x),
          nknots = nknots, ...)
}

#' Smooth spectra
#'
#' \code{smooth} runs each spectrum by a smoothing and returns the spectra
#'
#' @param x spectra object
#' @param method Choose smoothing method: "spline" (default) or "moving_average"
#' @param ... additional parameters passed to \code{smooth.spline}.
#'
#' @return a spectra object of with smoothed spectra
#' @export
smooth = function(x, method = "spline", ...){
    UseMethod("smooth")
}

#' @describeIn smooth Smooth spectra
#' @export
smooth.spectra = function(x, method = "spline", ...){
    if(method == "spline") {
        s   = i_smooth_spline_spectra(x, ...)
        x[] = do.call(rbind, sapply(s, `[`, "y"))
        return(x)
    } else if (method == "moving_average") {
        stop("Sorry, not implemented yet!")
    }
}


#' Resample spectra
#'
#' \code{resample} returns spectra resampled to new wavelengths using smoothing.
#' Possible to increase or decrease the spectral resolution.
#'
#' @param y spectra object
#' @param new_x numeric vector of wavelengths to sample from spectra
#' @param ... additional parameters passed to the \code{smooth.spline} function.
#'
#' @return spectra object with resampled spectra
#' @export
resample = function(x, new_x, ...) {
    UseMethod("resample")
}


#' @describeIn resample Resample spectra
#' @export
resample.spectra = function(y, new_x, ...) {
    r = range(wavelengths(y))

    if(min(new_x) < r[1] || max(new_x) > r[2]){
        stop("New wavelength values must be within the data's range: ", r[1], " to ", r[2])
    }

    s = i_smooth_spline_spectra(y, ...)
    f = function(o, p){ predict(o, p)[["y"]] }
    g = lapply(X = s, FUN = f, p = new_x)

    ## Digging in internal bits of the spectra object, which is NOT GOOD
    y[["wavelengths"]] = i_wavelengths(new_x)
    y[["reflectance"]] = i_reflectance( do.call(rbind, g) )

    y
}
