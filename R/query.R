#' Is it a spectra object?
#'
#' \code{is_spectra} tests if the argument is a spectra class object
#'
#' @param x any object
#' @return boolean
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' spec  = as.spectra(spec_matrix_example, name_idx = 1)
#' spec1 = unclass(spec)
#' is_spectra(spec)
#' is_spectra(spec1)
is_spectra = function(x){
    inherits(x, "spectra")
}
