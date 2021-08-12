#' Pairwise index combinations
#'
#' \code{i_index_pairwise_combn} returns the split pairwise combination of idxs
#' in a matrix
#'
#' @param n number of indices
#' @return list of matrices. For each focal index up to n - 1, a matrix with the
#' other indexes it interacts with (2nd col) and the "absolute position" of that
#' pairwise interaction (1st column) is returned.
#'
#' @author Jose Eduardo Meireles
#' @keywords internal
i_index_pairwise_combn = function(n){

    if( ! i_is_whole(n) | n < 2 ){
        stop("n must be an integer > 1")
    }

    focal_seq = seq.int(1, n - 1)
    focal_idx = rep.int(focal_seq, rev(focal_seq))

    iter_seq  = seq.int(2, n)
    iter_idx  = unlist(sapply(iter_seq, function(x){ seq.int(x, n)} ))

    pos_idx   = seq.int(along.with = iter_idx)

    pcomb = data.frame("pos" = pos_idx,  "iter" = iter_idx, check.names = FALSE)
    split(pcomb, focal_idx)
}


#' Pairwise value ratios
#'
#' \code{pairwise_ratio} computes pairwise ratios between bands
#'
#' @param x spectra
#' @param simplify coerce to matrix or keep result as list
#' @return list or matrix
#'
#' @author Jose Eduardo Meireles
#' @keywords internal
#'
#' @examples
#' library(spectrolab)
#'
#' # Ratios of visible part of the spectrum
pairwise_ratio = function(x, simplify = FALSE){
    message("ratio may take a while...")
    spm  = as.matrix(x)
    bds  = bands(x)
    pwc  = i_index_pairwise_combn(ncol(x))

    res  = lapply(names(pwc), function(y){
        i   = as.numeric(y)
        j   = pwc[[y]][ , "iter"]
        mat = spm[ , i]  / spm[ , j, drop = FALSE]
        colnames(mat) = paste(bds[i], bds[j], sep = "/")
        mat
    })

    names(res) = bds[ as.numeric(names(pwc)) ]

    if(simplify){
        res = do.call(cbind, res)
    }
    res
}
