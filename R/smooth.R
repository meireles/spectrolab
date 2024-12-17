################################################################################
# Smoothing functions
################################################################################

#' Generic Smoothing function
#'
#' @param x data to smooth over
#' @param ... additional arguments
#' @return smoothed data
#'
#' @export
smooth = function(x, ...){
    UseMethod("smooth")
}

#' Default smoothing function
#'
#' @param x data to smooth over
#' @param ... additional arguments
#' @return smoothed data
#'
#' @importFrom stats smooth
#' @export
smooth.default = function(x, ...){
    stats::smooth(x, ...)
}

#' Smooth spectra
#'
#' \code{smooth} runs each spectrum by a smoothing and returns the spectra
#'
#' @param x spectra object. bands must be strictly increasing
#' @param method Choose smoothing method: "spline" (default) or "moving_average"
#' @param ... additional parameters passed to \code{smooth.spline} or parameters
#'            `n` and `save_bands_to_meta` for the moving average smoothing.
#' @return a spectra object of with smoothed spectra
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#'
#' spec = as_spectra(spec_matrix_example, name_idx = 1)
#' spec = smooth(spec, parallel = FALSE)
smooth.spectra = function(x, method = "spline", ...){

    if(! i_is_increasing(bands(x))){
        stop("smooth requires strictly increasing band values.\nMatch sensor overlap before attempting to smooth the spectra.")
    }

    if(method == "spline") {
        smooth_spline(x, ...)
        return(x)
    } else if (method == "moving_average") {
        smooth_moving_avg(x, ...)
    }
}

#' Smooth spline functions for spectra
#'
#' Gets spline functions for each spectrum in a spectra object.
#'
#' @param x spectra object. bands must be strictly increasing
#' @param parallel boolean. Do computation in parallel? Defaults to TRUE.
#'                 Unfortunately, the parallelization does not work on Windows.
#' @param ... additional parameters passed to smooth.spline except nknots, which
#'            is computed internally
#' @param return_fn Boolean. If TRUE, \code{smooth_spline} returns the spline
#'                  functions instead of the smoothed spectra. Defaults to FALSE
#' @return Smoothed spectra or, if return_fn = TRUE, a list of spline functions.
#'
#' @importFrom stats smooth.spline
#' @importFrom parallel detectCores mclapply
#'
#' @author Jose Eduardo Meireles
#' @export
smooth_spline = function(x, parallel = TRUE, return_fn = FALSE, ...) {

    if( !is_spectra(x) ){
        stop("Object must be of class spectra")
    }

    i_mind_the_gap_smoothing(x)

    w       = bands(x)
    l       = nrow(x)

    scale   = c(0.1, 0.25, 0.5)
    cutres  = 100

    range   = diff(range(w))
    resol   = ceiling(range / ncol(x))
    fullres = floor(range / resol)
    propres = floor(range / resol * scale)
    nknots  = min( propres[propres >= cutres], fullres)

    d = value(x)
    r = lapply( seq.int(nrow(x)), function(y){ d[y, ]})

    # parallel?
    p = requireNamespace("parallel", quietly = TRUE)
    p = p && parallel && l > 1 && .Platform$OS.type != "windows"

    if(p){
        n = parallel::detectCores() - 1L
        b = floor(seq.int(0, length(r), length.out = min(n, l) + 1L))
        c = cut(seq.int(length(r)), b, include.lowest = TRUE)
        s = split(r, c)
        s = parallel::mclapply(s, function(z){
            lapply(z, stats::smooth.spline, x = w, nknots = nknots, ...)},
            mc.cores = n)
        f = unlist(s, recursive = FALSE, use.names = FALSE)
    } else {
        f = lapply(r, stats::smooth.spline, x = w, nknots = nknots, ...)
    }

    if(return_fn){
        return(f)
    } else {
        x[] = do.call(rbind, sapply(f, `[`, "y"))
        return(x)
    }
}


#' Smooth moving average for spectra
#'
#' @param x spectra object
#' @param n = NULL
#' @param save_bands_to_meta boolean. keep lost ends of original wvls in metadata
#' @return spectra object
#'
#' @author Jose Eduardo Meireles
#' @export
smooth_moving_avg = function(x, n = NULL, save_bands_to_meta = TRUE){
    if( !is_spectra(x) ){
        stop("Object must be of class spectra")
    }

    i_mind_the_gap_smoothing(x)

    if(is.null(n)){
        scale   = c(2, 3, 4, 5, 7, 10, 15, 20)
        cutres  = 150

        range   = diff(range( bands(x) ))
        resol   = ceiling(range / ncol(x))
        propres = floor(range / resol / scale)
        n       = max(c(scale[propres >= cutres]), 1)
    }

    if(n == 1){
        stop("Not enough resolution to smooth using moving average. n param was 1.")
    }

    message("Simple moving average over n: ", n)

    r   = value(x)
    s   = t(apply(r, 1, i_mav, n = n))
    w   = which(apply(is.na(s), 2, all))
    ww  = bands(x)[w]
    x[] = s
    x   = x[ , setdiff(bands(x), ww) ]

    if(length(w) != 0){

        message("Smoothing transformed some values into NAs and those bands were removed")

        if(save_bands_to_meta){
            message("However, the original values for those bands were kept as metadata")

            meta(x) = matrix(r[ , w],
                             nrow = nrow(x),
                             dimnames = list(NULL, paste("removed_wvl_", ww, sep = "")) )
        }
    }

    x
}


# smooth_fwhm = function(x, fwhm = NULL){
#     b = bands(x)
#     s = value(x)
#
#     if(missing(fwhm)){
#         fwhm = 2*(mean(diff(b)))
#     }
#
#     resample_fwhm(wavelengths = b, reflectance = s, new_wavelengths = b, fwhm = fwhm)
# }


