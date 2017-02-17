library("devtools")
devtools::use_package("devtools")
devtools::use_package("parallel")

################################################################################
# Combine spectral datasets
################################################################################

#' Combine spectral datasets
#'
#' \code{combine} binds two spectral datasets. Both spectra must have the
#' very same wavelenegth labels, but different metadata are acceptable
#'
#' @param s1 spectra object 1
#' @param s2 spectra object 1
#' @return combined spectra object
#'
#' @author Jose Eduardo Meireles
#' @export
combine = function(s1, s2){
   UseMethod("combine")
}


#' @describeIn combine Combines two spectral datasets
#' @export
combine.spectra = function(s1, s2){
    if( !is_spectra(s2) ){
        stop("Object `b` must be of class spectra")
    }

    if(any( suppressWarnings(wavelengths(s1) != wavelengths(s2)) )){
        stop("Spectra must have the same wavelenegths. Consider using `resample()` first")
    }

    r = rbind(reflectance(s1), reflectance(s2))
    n = c(names(s1), names(s2))
    w = wavelengths(s1)               ## OK because I tested for equality before

    if(enforce01(s1) != enforce01(s2)){
        warning("Spectra objects have different enforce01 requirements.\n  Setting enforce01 to FALSE...")
        e = FALSE
    } else {
        e = enforce01(s1)             ## OK because I tested for inequality before
    }

    ## Merge metadata
    m1 = meta(s1)
    m2 = meta(s2)
    mn = union(names(m1), names(m2))

    m3 = data.frame(matrix(nrow     = nrow(m1) + nrow(m2),
                           ncol     = length(mn),
                           dimnames = list(NULL, mn)),
                    check.names = FALSE)

    m3[1 : nrow(m1), names(m1)] = m1
    m3[(1 + nrow(m1)) : nrow(m3), names(m2)] = m2

    spectra(r, w, n, m3, e)
}


################################################################################
# Aggregate
################################################################################

#' Aggregate
#'
#' @param x spectra object
#' @param by vector of factors to guide the aggregation
#' @param FUN function to be applied
#' @param ... extra args to FUN
#' @return spectra object
#'
#' @importFrom stats aggregate
#'
#' @author Jose Eduardo Meireles
#' @export
aggregate.spectra = function(x, by, FUN, ...){

    if(!is.list(by)){
        by = list(by)
    }

    r = stats::aggregate(as.matrix(x), by, FUN, ...)
    m = stats::aggregate(meta(x), by, FUN, ...)
    n = r[ , 1]
    s = as.spectra(r, 1)
    meta(s) = m[ , -1]
    s
}



################################################################################
# Vector normalization
################################################################################

#' Vector normalize spectra
#'
#' \code{normalize} returns a spectra obj with vector normalized reflectances
#'
#' @param x spectra object. Wavelengths must be strictly increasing
#' @param quiet booean. Warn about change in y value units? Defaults to FALSE
#' @param ... nothing
#' @return spectra object with normalized spectra
#'
#' @author Jose Eduardo Meireles
#' @export
normalize = function(x, quiet = FALSE, ...){
    UseMethod("normalize")
}


#' @describeIn normalize Vector normalize spectra
#' @export
normalize.spectra = function(x, quiet = FALSE, ...){

    i_test_increasing_wavelengths(wavelengths(x), stop = TRUE)

    if(!quiet){
        message("Vector nomalizing spectra...")
        message("Note that y values will not be true reflectances anymore!")

        if( "normalization_magnitude" %in% names(meta(x , "normalization_magnitude")) ){
            warning("spectra were apparently already vector normalized.\n  normalization magnitudes may not make sense.")
        }
    }

    refl            = reflectance(x)
    refl_squared    = refl * refl
    vec_ones        = rep.int(1L, ncol(refl_squared))
    spec_sq_rowsum  = refl_squared %*% vec_ones
    magnitudes      = as.vector(sqrt(spec_sq_rowsum))

    # normalize and construct a `spectra` object
    x[] = i_reflectance(refl / magnitudes)

    # add a magnitute attribute to the`spectra` object
    meta(x, "normalization_magnitude") = magnitudes

    x
}

################################################################################
# Smoothing functions
################################################################################

#' Smooth spline functions for spectra
#'
#' Gets spline functions for each spectrum in a spectra object.
#'
#' @param x spectra object. Wavelengths must be strictly increasing
#' @param parallel boolean. Do computation in parallel? Defaults to TRUE.
#'                 Unfortunately, the parallelization doesn't work on Windows.
#' @param ... additional parameters passed to smooth.spline except nknots, which
#'            is computed internally
#' @return a list of spline functions
#'
#' @importFrom parallel detectCores mclapply
#' @importFrom stats smooth.spline
#'
#' @keywords internal
#' @author Jose Eduardo Meireles
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

        if( .Platform$OS.type == "windows" ){
            message("Parallelization is not availiable for windows. Using 1 core...")
            n = 1
        }

        return(parallel::mclapply(r, stats::smooth.spline, x = w, nknots = nknots, mc.cores = n, ...))
    } else {
        return(lapply(r, stats::smooth.spline, x = w, nknots = nknots, ...))
    }
}

#' Default smoothing function
#'
#' @inherit stats::smooth
#' @importFrom stats smooth
smooth.default = stats::smooth

#' Smooth spectra
#'
#' \code{smooth} runs each spectrum by a smoothing and returns the spectra
#'
#' @param x spectra object. Wavelengths must be strictly increasing
#' @param method Choose smoothing method: "spline" (default) or "moving_average"
#' @param ... additional parameters passed to \code{smooth.spline}.
#' @return a spectra object of with smoothed spectra
#'
#' @author Jose Eduardo Meireles
#' @export
smooth = function(x, method = "spline", ...){
    UseMethod("smooth")
}

#' @describeIn smooth Smooth spectra
#' @export
smooth.spectra = function(x, method = "spline", ...){

    i_test_increasing_wavelengths(wavelengths(x), stop = TRUE)

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
#' @param x spectra object. Wavelengths must be strictly increasing
#' @param new_wvls numeric vector of wavelengths to sample from spectra
#' @param ... additional parameters passed to the \code{smooth.spline} function.
#' @return spectra object with resampled spectra
#'
#' @importFrom stats predict
#'
#' @author Jose Eduardo Meireles
#' @export
resample = function(x, new_wvls, ...) {
    UseMethod("resample")
}


#' @describeIn resample Resample spectra
#' @export
resample.spectra = function(x, new_wvls, ...) {

    ## Enforce increasing wavelengths in spectra object
    i_test_increasing_wavelengths(wavelengths(x), stop = TRUE)

    ## Do not predict points outside the original wavelength range
    r = range(wavelengths(x))

    if(min(new_wvls) < r[1] || max(new_wvls) > r[2]){
        stop("New wavelength values must be within the data's range: ", r[1], " to ", r[2])
    }

    ## Smooth and predict
    s = i_smooth_spline_spectra(x, ...)
    f = function(o, p){ stats::predict(o, p)[["y"]] }
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
