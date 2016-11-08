################################################################################
# Spectra functions
################################################################################

#' Verify if object are spectra
#'
#' @param spec any object
#'
#' @return boolean
#' @export
is_spectra = function(spec){
    inherits(spec, "spectra")
}

#' Title
#'
#' @param spec
#'
#' @return
#' @export
normalize_spectra = function(spec){

    if( !is_spectra(spec) ){
        stop("Object must be of class spectra")
    }

    spec_squared    = reflectance(spec) * reflectance(spec)
    magnitudes      = sqrt( rowSums(spec_squared) )
    spec[]          = i_reflectance(spec_squared / magnitudes)
    spec$magnitudes = magnitudes
    spec
}

