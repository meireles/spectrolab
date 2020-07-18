#' Wrap function to try to keep text
#'
#' Function operator returning a function f that tries to keep text.
#'
#' \code{try_keep_txt} takes a function f as argument, typically a mathematical
#' operation such as mean, median, etc. and returns a modified version of it that
#' will try return a string of unique values in case function f emits a warning.
#' Useful when aggregating over spectral metadata that has both numeric values
#' (which you want to aggregate) and text values, which you want to keep.
#'
#' @param f function to be applied
#' @return modified function f (f').
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' g = try_keep_txt(mean)
#' g(c(1, 2))
#' g(c("a", "b"))
try_keep_txt = function(f){
    function(x, ...){
        r = tryCatch(
            { f(x, ...) },
            warning = function(w){  paste(unique(x), collapse = ", ") },
            error   = function(e){  paste(unique(x), collapse = ", ") }
        )
        r
    }
}
