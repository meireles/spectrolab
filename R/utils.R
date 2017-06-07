#' Wrap function to try to keep text
#'
#' Function operator that returns a function f that tries to keep text.
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
try_keep_txt = function(f){
    function(x, ...){
        r = tryCatch(
            { f(x, ...) },
            warning = function(w){  paste(unique(x), collapse = ", ") }
        )
        r
    }
}


#' Pairwise reflectance ratios
#'
#' \code{ratio} computes pairwise ratios between bands
#'
#' @param x spectra
#' @param simplify coerce to matrix or keep result as list
#' @return list or matrix
#'
#' @author Jose Eduardo Meireles
#' @export
ratio = function(x, simplify = FALSE){
    UseMethod("ratio")
}


#' @describeIn ratio Compute pairwise reflectance ratios
#' @export
ratio.spectra = function(x, simplify = FALSE){
    spm  = as.matrix(x)
    wvl  = wavelengths(x)
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
