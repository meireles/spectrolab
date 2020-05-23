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


#' Pairwise value ratios
#'
#' \code{ratio} computes pairwise ratios between bands
#'
#' @param x spectra
#' @param simplify coerce to matrix or keep result as list
#' @return list or matrix
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#'
#' # Ratios of visible part of the spectrum
#' spec = as.spectra(spec_matrix_example, name_idx = 1)[ , 400:700 ]
#' spec_ratio_mat = ratio(spec)
ratio = function(x, simplify = FALSE){
    UseMethod("ratio")
}


#' @describeIn ratio Compute pairwise value ratios
#' @export
ratio.spectra = function(x, simplify = FALSE){
    message("ratio may take a while...")
    spm  = as.matrix(x)
    wvl  = bands(x)
    pwc  = i_index_pairwise_combn(ncol(x))

    res  = lapply(names(pwc), function(y){
        i   = as.numeric(y)
        j   = pwc[[y]][ , "iter"]
        mat = spm[ , i]  / spm[ , j, drop = FALSE]
        colnames(mat) = paste(wvl[i], wvl[j], sep = "/")
        mat
    })

    names(res) = wvl[ as.numeric(names(pwc)) ]

    if(simplify){
        res = do.call(cbind, res)
    }
    res
}

