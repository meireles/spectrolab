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


#' Wrap function to try to keep text
#'
#' Function operator that returns a function f that tries to keep text.
#'
#' \code{try_keep_txt} takes a function f as argument, typically a mathematical
#' operation such as mean, median, etc. and returns a modified version of it that
#' will try return a string of unique values in case functin f emits a warning.
#' Useful when aggregating over spectral metadata that has both numeric values
#' (which you want to aggregate) and text values, which you want to keep.
#'
#' @param f function to be applied
#' @return modified function f (f').
#'
#' @author Jose Eduardo Meireles
#' @export
try_keep_txt = function(f){
    function(x, ...){
        r = tryCatch(
            { f(x, ...) },
            warning = function(w){  paste(unique(x), collapse = ", ") }
        )
        r
    }
}
