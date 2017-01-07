library("devtools")
devtools::use_package("devtools")
devtools::use_package("parallel")

################################################################################
# Vector normalization
################################################################################

#' Vector normalize spectra
#'
#' \code{normalize} returns a spectra obj with vector normalized reflectances
#'
#' @param x spectra object to be vector normalized
#' @param ... nothing
#' @return spectra object with normalized spectra
#'
#' @author meireles
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

################################################################################
# Smoothing functions
################################################################################

#' Smooth spline functions for spectra
#'
#' Gets spline functions for each spectrum in a spectra object
#'
#' @param x spectra object
#' @param parallel boolean. Do computation in parallel? Defaults to TRUE
#' @param ... additional parameters passed to smooth.spline except nknots, which
#'            is computed internally
#' @return a list of spline functions
#'
#' @importFrom parallel detectCores mclapply
#'
#' @author meireles
i_smooth_spline_spectra = function(x, parallel = TRUE, ...) {
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

    d = reflectance(x)
    r = lapply( seq.int(nrow(x)), function(y){ d[y, ]})
    w = wavelengths(x)

    if(parallel) {
        n = parallel::detectCores()
        return(parallel::mclapply(r, smooth.spline, x = w, nknots = nknots, mc.cores = n, ...))
    } else {
        return(lapply(r, smooth.spline, x = w, nknots = nknots, ...))
    }
}

#' Smooth function from `stats`
#' @rdname stats::smooth Default smoothing function
smooth.default = stats::smooth

#' Smooth spectra
#'
#' \code{smooth} runs each spectrum by a smoothing and returns the spectra
#'
#' @param x spectra object
#' @param method Choose smoothing method: "spline" (default) or "moving_average"
#' @param ... additional parameters passed to \code{smooth.spline}.
#' @return a spectra object of with smoothed spectra
#'
#' @author meireles
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

################################################################################
# Resampling spectra
################################################################################

#' Resample spectra
#'
#' \code{resample} returns spectra resampled to new wavelengths using smoothing.
#' Possible to increase or decrease the spectral resolution.
#'
#' @param x spectra object
#' @param new_wvls numeric vector of wavelengths to sample from spectra
#' @param ... additional parameters passed to the \code{smooth.spline} function.
#' @return spectra object with resampled spectra
#'
#' @author meireles
#' @export
resample = function(x, new_wvls, ...) {
    UseMethod("resample")
}


#' @describeIn resample Resample spectra
#' @export
resample.spectra = function(x, new_wvls, ...) {

    ## Do not predict points outside the original wavelength range
    r = range(wavelengths(x))

    if(min(new_wvls) < r[1] || max(new_wvls) > r[2]){
        stop("New wavelength values must be within the data's range: ", r[1], " to ", r[2])
    }

    ## Smooth and predict
    s = i_smooth_spline_spectra(x, ...)
    f = function(o, p){ predict(o, p)[["y"]] }
    g = lapply(X = s, FUN = f, p = new_wvls)
    d = i_reflectance( do.call(rbind, g) )

    ## Wavelength number may change, so using the "safe" setter will fail
    ## Instead of reaching inside the spectra object, I am using the "unsafe"
    ## version of the wavelength setter.
    wavelengths(x, unsafe = TRUE) = new_wvls

    ## THIS IS BAD. Figure out an "unsafe" version of the reflectance setter
    x[["reflectance"]] = d

    x
}
