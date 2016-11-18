#' Vector normalization of spectra
#'
#' @param spec spectra object to be vector normalized
#'
#' @return spectra object with normalized spectra
#' @export
normalize_spectra = function(spec){

    if( !is_spectra(spec) ){
        stop("Object must be of class spectra")
    }

    refl            = reflectance(spec)
    refl_squared    = refl * refl
    vec_ones        = rep.int(1L, ncol(refl_squared))
    spec_sq_rowsum  = refl_squared %*% vec_ones
    magnitudes      = as.vector(sqrt(spec_sq_rowsum))

    # normalize and construct a `spectra` object
    spec[] = i_reflectance(refl / magnitudes)

    # add a magnitute attribute to the`spectra` object
    # spec$magnitudes = magnitudes

    # return
    spec
}
