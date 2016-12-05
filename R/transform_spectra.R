#' Vector normalize spectra
#'
#' \code{normalize} returns a spectra obj with vector normalized reflectances
#'
#' @param x spectra object to be vector normalized
#' @param ... nothing
#'
#' @return spectra object with normalized spectra
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
