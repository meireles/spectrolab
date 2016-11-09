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
#' @param spec spectra object to be vector normalized
#'
#' @return
#' @export
normalize_spectra = function(spec){

    if( !is_spectra(spec) ){
        stop("Object must be of class spectra")
    }
    refl            = reflectance(spec)
    refl_squared    = refl * refl
    vec_ones        = rep.int(1L, ncol(refl_squared))
    spec_sq_rowsum  = refl_squared %*% vec_ones
    #magnitudes      = sqrt( rowSums(refl_squared) )
    magnitudes      = as.vector(sqrt( spec_sq_rowsum ))
    spec[]          = i_reflectance(refl / magnitudes)
    spec$magnitudes = magnitudes
    spec
}


# #' Vector normalize spectra
# #'
# #' @param x martrix with spectra to be converted. Rows must be individual samples.
# #' Columns must be labeled with the corrspondent wavelength.
# #'
# #' @return A list containing the converted matrix and a vector of magnitudes
# normalize_spectra = function(x) {
#     x = as.matrix(x)
#     y = rep.int(1, ncol(x_squared))
#     x_squared   = x * x
#     s_sq_rowsum = x_squared %*% y
#     magnitudes  = as.vector(sqrt(s_sq_rowsum))
#     return(list(normalized_spectra = x / magnitudes, magnitudes = magnitudes))
# }


