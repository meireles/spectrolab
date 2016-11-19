#' Vector normalize spectra
#'
#' \code{normalize_spectra} returns a spectra obj with vector normalized reflectances
#'
#' @param x spectra object to be vector normalized
#'
#' @return spectra object with normalized spectra
#' @export
normalize_spectra = function(x){

    if( !is_spectra(x) ){
        stop("Object must be of class spectra")
    }

    refl            = reflectance(x)
    refl_squared    = refl * refl
    vec_ones        = rep.int(1L, ncol(refl_squared))
    spec_sq_rowsum  = refl_squared %*% vec_ones
    magnitudes      = as.vector(sqrt(spec_sq_rowsum))

    # normalize and construct a `spectra` object
    x[] = i_reflectance(refl / magnitudes)

    # add a magnitute attribute to the`spectra` object
    # spec$magnitudes = magnitudes

    # return
    x
}
