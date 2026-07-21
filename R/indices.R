################################################################################
# Two-band spectral indices
################################################################################

#' Find the nearest band to a target wavelength
#'
#' @param b numeric vector of band wavelengths
#' @param target target wavelength
#' @param tolerance maximum allowed distance (in the same units as b) between
#'        target and the nearest available band
#' @return integer index into b
#'
#' @author Jose Eduardo Meireles
#' @keywords internal
i_nearest_band_idx = function(b, target, tolerance){
    d   = abs(b - target)
    idx = which.min(d)

    if(d[idx] > tolerance){
        stop("No band found within ", tolerance, " (units) of ", target,
             ". Nearest available band is ", b[idx], ".")
    }

    idx
}


#' Compute a two-band normalized difference index
#'
#' Engine behind every function built with \code{\link{make_spectral_index}}.
#' Computes \code{(R[band1] - R[band2]) / (R[band1] + R[band2])} for each
#' spectrum, where \code{R[band]} is the value at the band nearest \code{band}
#' (within \code{tolerance}).
#'
#' @param x spectra object. bands must be strictly increasing
#' @param band1 first band wavelength
#' @param band2 second band wavelength
#' @param tolerance maximum allowed distance between a requested band and the
#'        nearest available band
#' @return named numeric vector (one value per sample)
#'
#' @author Jose Eduardo Meireles
#' @keywords internal
i_two_band_index = function(x, band1, band2, tolerance){

    if(!is_spectra(x)){
        stop("x must be a spectra object")
    }

    if(! i_is_increasing(bands(x))){
        stop("spectral indices require strictly increasing band values.\nMatch sensor overlap before computing indices.")
    }

    b  = bands(x)
    i1 = i_nearest_band_idx(b, band1, tolerance)
    i2 = i_nearest_band_idx(b, band2, tolerance)

    v  = value(x)
    r1 = v[ , i1]
    r2 = v[ , i2]

    idx = (r1 - r2) / (r1 + r2)
    names(idx) = names(x)

    idx
}


#' Create a two-band normalized-difference spectral index function
#'
#' \code{make_spectral_index} builds and returns a function that computes
#' \code{(R[band1] - R[band2]) / (R[band1] + R[band2])} for a spectra object.
#' This is how every entry in \code{\link{spectral_index}} (NDVI, PRI, ...) is
#' defined; use it directly to define your own two-band indices without
#' adding new top-level exports.
#'
#' @param band1 first band wavelength
#' @param band2 second band wavelength
#' @param tolerance default maximum allowed distance between a requested band
#'        and the nearest available band; can be overridden in the returned
#'        function's call. Defaults to 1
#' @return a function \code{function(x, tolerance = tolerance)} that, when
#'         called on a spectra object, returns a named numeric vector (one
#'         value per sample)
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' spec = as_spectra(spec_matrix_example, name_idx = 1)
#' my_ndvi = make_spectral_index(800, 680)
#' my_ndvi(spec)
make_spectral_index = function(band1, band2, tolerance = 1){
    force(band1)
    force(band2)
    default_tolerance = tolerance

    function(x, tolerance = default_tolerance){
        i_two_band_index(x, band1 = band1, band2 = band2, tolerance = tolerance)
    }
}


#' Built-in two-band spectral indices
#'
#' A named list of ready-to-use spectral index functions, each built with
#' \code{\link{make_spectral_index}}. Call an entry directly on a spectra
#' object, e.g. \code{spectral_index$ndvi(x)}.
#'
#' @format A list of functions, each \code{function(x, tolerance)}:
#' \describe{
#'   \item{ndvi}{Normalized Difference Vegetation Index: (R800 - R680) / (R800 + R680)}
#'   \item{pri}{Photochemical Reflectance Index: (R570 - R531) / (R570 + R531)}
#' }
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' spec = as_spectra(spec_matrix_example, name_idx = 1)
#' spectral_index$ndvi(spec)
#' spectral_index$pri(spec)
spectral_index = list(
    ndvi = make_spectral_index(800, 680),
    pri  = make_spectral_index(570, 531)
)
