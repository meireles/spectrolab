################################################################################
# Savitzky-Golay smoothing and derivatives
################################################################################

#' Apply a Savitzky-Golay filter row-wise
#'
#' \code{i_sgolay_apply} runs \code{signal::sgolayfilt} over every row (sample)
#' of a value matrix. Shared by \code{smooth_sgolay} (m = 0) and
#' \code{deriv_spectra} (m = order).
#'
#' @param r numeric matrix, samples in rows and bands in columns
#' @param p polynomial order
#' @param n filter length (must be odd and greater than p)
#' @param m derivative order (0 = smoothing only)
#' @param ts sample spacing, used to scale the derivative
#' @return numeric matrix, same shape as r
#'
#' @author Jose Eduardo Meireles
#' @keywords internal
i_sgolay_apply = function(r, p, n, m, ts){

    if(n %% 2 == 0){
        stop("n (filter length) must be odd.")
    }

    if(n <= p){
        stop("n (filter length) must be greater than p (polynomial order).")
    }

    t(apply(r, 1, function(row){ signal::sgolayfilt(row, p = p, n = n, m = m, ts = ts) }))
}


#' Smooth spectra with a Savitzky-Golay filter
#'
#' \code{smooth_sgolay} fits a local polynomial of order \code{p} over a moving
#' window of \code{n} bands and replaces each band value with the fitted value.
#' Requires the \code{signal} package.
#'
#' @param x spectra object. bands must be strictly increasing
#' @param p polynomial order. Defaults to 3
#' @param n filter length (number of bands in the moving window). Must be odd
#'          and greater than p. Defaults to \code{p + 3 - p \%\% 2}
#' @return spectra object with Savitzky-Golay smoothed spectra
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' spec = as_spectra(spec_matrix_example, name_idx = 1)
#' if(requireNamespace("signal", quietly = TRUE)){
#'     spec_sg = smooth_sgolay(spec)
#' }
smooth_sgolay = function(x, p = 3, n = p + 3 - p %% 2){

    if( !is_spectra(x) ){
        stop("Object must be of class spectra")
    }

    if(!requireNamespace("signal", quietly = TRUE)){
        stop("Package 'signal' is needed for Savitzky-Golay smoothing.\n",
             "Install it with: install.packages('signal')")
    }

    i_mind_the_gap_smoothing(x)

    w   = bands(x)
    ts  = stats::median(diff(w))
    r   = value(x)
    s   = i_sgolay_apply(r, p = p, n = n, m = 0, ts = ts)
    x[] = s

    x
}


#' Compute spectral derivatives
#'
#' \code{deriv_spectra} computes the order-th derivative of each spectrum using
#' a Savitzky-Golay filter. Requires the \code{signal} package.
#'
#' @param x spectra object. bands must be strictly increasing
#' @param order derivative order. Must be a positive integer. Defaults to 1
#' @param p polynomial order. Defaults to 3
#' @param n filter length (number of bands in the moving window). Must be odd
#'          and greater than p. Defaults to \code{p + 3 - p \%\% 2}
#' @param quiet boolean. Suppress the message about y values no longer being
#'              reflectance/radiance? Defaults to FALSE
#' @return spectra object with derivative spectra. Values are no longer
#'         reflectance/radiance
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' spec = as_spectra(spec_matrix_example, name_idx = 1)
#' if(requireNamespace("signal", quietly = TRUE)){
#'     spec_d1 = deriv_spectra(spec, order = 1)
#' }
deriv_spectra = function(x, order = 1, p = 3, n = p + 3 - p %% 2, quiet = FALSE){

    if( !is_spectra(x) ){
        stop("Object must be of class spectra")
    }

    if(! i_is_increasing(bands(x))){
        stop("deriv_spectra requires strictly increasing band values.\nMatch sensor overlap before computing derivatives.")
    }

    if(! i_is_whole(order) || order < 1){
        stop("order must be a positive integer")
    }

    if(!requireNamespace("signal", quietly = TRUE)){
        stop("Package 'signal' is needed to compute spectral derivatives.\n",
             "Install it with: install.packages('signal')")
    }

    i_mind_the_gap_smoothing(x)

    if(!quiet){
        message("Computing order-", order, " derivative spectra...")
        message("Note that y values will not be reflectance/radiance values anymore!")
    }

    w   = bands(x)
    ts  = stats::median(diff(w))
    r   = value(x)
    s   = i_sgolay_apply(r, p = p, n = n, m = order, ts = ts)
    x[] = s

    x
}
