################################################################################
# From utils.R
################################################################################

#' Pairwise value ratios -- Deprecated!
#'
#' \code{ratio} computes pairwise ratios between bands
#'
#'  Deprecated!
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
#' spec = as_spectra(spec_matrix_example, name_idx = 1)[ , 400:700 ]
#' spec_ratio_mat = ratio(spec)
ratio = function(x, simplify = FALSE){
    warning("Deprecated!")
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

