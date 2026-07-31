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
#' @param method Choose smoothing method: "gaussian" (default), "spline", "moving_average", or "sgolay"
#' @param ... additional parameters passed to methods \code{smooth_fwhm}, \code{smooth_spline}, \code{smooth_moving_avg}, \code{smooth_sgolay}
#' @return a spectra object of with smoothed spectra
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#'
#' spec = as_spectra(spec_matrix_example, name_idx = 1)
#' spec = smooth(spec)
smooth.spectra = function(x, method = "gaussian", ...){

    if(! i_is_increasing(bands(x))){
        stop("smooth requires strictly increasing band values.\nMatch sensor overlap before attempting to smooth the spectra.")
    }

    if(method == "gaussian"){
        smooth_fwhm(x, ...)
    } else if(method == "spline") {
        smooth_spline(x, ...)
    } else if (method == "moving_average") {
        smooth_moving_avg(x, ...)
    } else if (method == "sgolay") {
        smooth_sgolay(x, ...)
    } else {
        stop("unknown smoothing method: '", method,
             "'. Choose 'gaussian', 'spline', 'moving_average', or 'sgolay'.")
    }
}

#' Smooth spline functions for spectra
#'
#' Gets spline functions for each spectrum in a spectra object.
#'
#' @param x spectra object. bands must be strictly increasing
#' @param parallel boolean. Do computation in parallel? Defaults to TRUE.
#'                 Unfortunately, the parallelization does not work on Windows.
#' @param cores number of cores to fork when \code{parallel = TRUE}. Defaults to
#'              \code{getOption("mc.cores", 2L)}. The default is deliberately
#'              conservative (CRAN caps checks at two cores and forking as many
#'              processes as the machine has cores is rarely a win for this
#'              workload); raise it explicitly, or set \code{options(mc.cores=)},
#'              on a big machine with many spectra.
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
smooth_spline = function(x,
                         parallel  = TRUE,
                         return_fn = FALSE,
                         cores     = getOption("mc.cores", 2L),
                         ...) {

    if( !is_spectra(x) ){
        stop("Object must be of class spectra")
    }

    i_mind_the_gap_smoothing(x)

    w       = bands(x)
    l       = nrow(x)

    scale   = c(0.1, 0.25, 0.5)
    cutres  = 100

    wl_span = diff(range(w))
    resol   = ceiling(wl_span / ncol(x))
    fullres = floor(wl_span / resol)
    propres = floor(wl_span / resol * scale)
    nknots  = min( propres[propres >= cutres], fullres)

    d = value(x)
    r = lapply( seq.int(nrow(x)), function(y){ d[y, ]})

    # parallel? mclapply schedules elements across cores itself, so no manual
    # chunking is needed.
    p = requireNamespace("parallel", quietly = TRUE)
    p = p && parallel && l > 1 && .Platform$OS.type != "windows"

    if(p){
        ## Never fork more workers than there are spectra, cores on the machine,
        ## or the caller asked for. detectCores() - 1L used to be the hard-wired
        ## default, which spawns e.g. 127 processes on a big host and exceeds the
        ## two-core ceiling CRAN checks run under.
        n = max(1L, min(as.integer(cores), l, parallel::detectCores()))
        f = parallel::mclapply(r, stats::smooth.spline, x = w, nknots = nknots, ...,
                               mc.cores = n)
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
#' @param n number of bands going into each moving average. When \code{NULL}
#'          (default) a window is chosen from the band resolution.
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

        wl_span = diff(range( bands(x) ))
        resol   = ceiling(wl_span / ncol(x))
        propres = floor(wl_span / resol / scale)
        n       = max(c(scale[propres >= cutres]), 1)
    }

    if(n == 1){
        stop("Not enough resolution to smooth using moving average. n param was 1.")
    }

    message("Simple moving average over n: ", n)

    r   = value(x)
    s   = t(apply(r, 1, i_mav, n = n))
    w   = which(apply(is.na(s), 2, all))   # column INDICES fully NA'd by smoothing
    ww  = bands(x)[w]

    ## Drop the NA'd bands by column index, never by label: a duplicated overlap
    ## wavelength would make label selection pull the wrong (or extra) columns.
    ## See the duplicate-band contract in R/getters_and_setters.R.
    keep = if(length(w) == 0){ seq_len(ncol(x)) } else { seq_len(ncol(x))[-w] }
    out  = new_spectra(value = s[ , keep, drop = FALSE],
                       bands = bands(x)[keep],
                       names = names(x),
                       meta  = meta(x))
    attr(out, "sensor_info") = attr(x, "sensor_info")
    x = out

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


#' Smooth spectra with a gaussian model
#'
#' @param x spectra
#' @param fwhm Full Width at Half Maximum.
#'
#' @return smoothed spectra
#' @export
smooth_fwhm = function(x, fwhm = NULL){
    if( !is_spectra(x) ){
        stop("Object must be of class spectra")
    }
    b = bands(x)
    ## Test the value, not missing(): the documented default is fwhm = NULL, and
    ## an explicit `smooth_fwhm(x, fwhm = NULL)` must auto-compute too.
    if(is.null(fwhm)){
        fwhm = 2 * make_fwhm(x, new_bands = b, new_fwhm = NULL, return_type = "old")
    }

    resample(x, new_bands = b, fwhm = fwhm)
}
