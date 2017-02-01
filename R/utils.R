################################################################################
# Spectra functions
################################################################################

#' Is it a spectra object?
#'
#' \code{is_spectra} tests if the argument is a spectra class object
#'
#' @param x any object
#' @return boolean
#'
#' @author Jose Eduardo Meireles
#' @export
is_spectra = function(x){
    inherits(x, "spectra")
}
