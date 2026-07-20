################################################################################
# Continuum removal
################################################################################

#' Find the upper convex hull ("continuum") of a spectrum
#'
#' \code{i_upper_hull_idx} finds the indices of \code{r} that lie on the upper
#' convex hull of the (w, r) point set, i.e. the continuum line. A point of the
#' convex hull is on the upper hull if it lies on or above the chord connecting
#' the leftmost and rightmost points -- points on the lower hull lie below it.
#'
#' @param w numeric vector of wavelengths, strictly increasing
#' @param r numeric vector of reflectance/radiance values, same length as w
#' @return integer vector of indices (into w/r), sorted by wavelength
#'
#' @importFrom grDevices chull
#'
#' @author Jose Eduardo Meireles
#' @keywords internal
i_upper_hull_idx = function(w, r){

    h  = grDevices::chull(w, r)

    x1 = w[[1]]
    y1 = r[[1]]
    x2 = w[[length(w)]]
    y2 = r[[length(r)]]

    chord_y = y1 + (y2 - y1) * (w[h] - x1) / (x2 - x1)
    keep    = r[h] >= (chord_y - sqrt(.Machine$double.eps))

    h = h[keep]
    h[order(w[h])]
}


#' Remove the continuum from a single spectrum
#'
#' @param r numeric vector of reflectance/radiance values
#' @param w numeric vector of wavelengths, strictly increasing, same length as r
#' @return numeric vector, r divided by its continuum
#'
#' @importFrom stats approx
#'
#' @author Jose Eduardo Meireles
#' @keywords internal
i_continuum_remove_one = function(r, w){
    h    = i_upper_hull_idx(w, r)
    hull = stats::approx(w[h], r[h], xout = w)$y
    r / hull
}


#' Continuum removal
#'
#' \code{continuum_removal} divides each spectrum by its continuum -- the
#' upper convex hull over wavelength/value space -- which suppresses broad
#' absorption features and emphasizes local absorption depth. Values in the
#' output are no longer reflectance/radiance.
#'
#' @param x spectra object. bands must be strictly increasing
#' @param ... additional arguments (not currently used)
#' @return spectra object with continuum-removed spectra
#'
#' @author Jose Eduardo Meireles
#' @export
continuum_removal = function(x, ...){
    UseMethod("continuum_removal")
}


#' @describeIn continuum_removal Continuum removal
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' spec = as_spectra(spec_matrix_example, name_idx = 1)
#' spec_cr = continuum_removal(spec)
continuum_removal.spectra = function(x, ...){

    if(! i_is_increasing(bands(x))){
        stop("continuum_removal requires strictly increasing band values.\nMatch sensor overlap before attempting continuum removal.")
    }

    w   = bands(x)
    r   = value(x)
    s   = t(apply(r, 1, i_continuum_remove_one, w = w))
    x[] = s

    ## No longer a raw reflectance/radiance quantity -- clear rather than carry
    ## a now-misleading label forward. See R/provenance.R.
    quantity(x) = NA_character_

    x
}
