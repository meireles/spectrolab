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

