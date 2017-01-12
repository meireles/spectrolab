#' Is whole number?
#'
#' \code{i_is_whole} Tests if x is (are) whole numbers
#'
#' @param x single value or vector of numbers
#' @return boolean
#'
#' @author meireles
i_is_whole = function(x){
    as.integer(x) == x
}

#' Compatible with being an index?
#'
#' \code{i_is_index} Tests if x fit the requirements of being indices
#'
#' @param x numeric values
#' @param max_length max acceptable values for x
#' @param all boolean. If TRUE, a single logical value is retuned. Else, a vector
#'                     of length = length(x) is returned
#' @param quiet boolean. get warnings?
#' @return boolean
#'
#' @author meireles
i_is_index = function(x, max_length, all = TRUE, quiet = TRUE){
    if(quiet){
        w = suppressWarnings(i_is_whole(x))
    } else {
        w = i_is_whole(x)
    }

    p = x > 0 & x <= round(max_length, digits = 0)
    r = w & p

    if(all){
        r = all(r)
    }

    r
}


#' Match label
#'
#' @param x label vector
#' @param i picked label or NULL
#' @return matched indices
#'
#' @author meireles
#' @export
i_match_label = function(x, i){
    l = length(x)

    if(missing(i) || is.null(i)){
        return(seq.int(l))
    }

    m = which(x %in% i)
    n = setdiff(i, x)

    if( length(n) != 0 || length(n) == length(i) ){
        stop("Following labels not found: ", n)
    }

    m
}


#' Match label or index
#'
#' @param x label vector
#' @param i picked label or idx or NULL
#' @return matched indices
#'
#' @author meireles
#' @export
i_match_label_or_idx = function(x, i){
    l = length(x)

    if(missing(i) || is.null(i)){
        return(seq.int(l))
    }

    if(i_is_index(x = i, max_length = l)){
        return(as.integer(i))
    }

    ## Could use i_match_label here, though that would
    ## duplicate the is missing check
    m = which(x %in% i)
    n = setdiff(i, x)

    if( length(n) != 0 || length(n) == length(i) ){
        stop("Sample subscript out of bounds: ", n)
    }

    m
}



#' Find plot boundaries in user space
#'
#' \code{i_plot_boundaries} gets plot boundaries in user space as matrix or vec
#'
#' @param return_mat return a matrix instead of vector? defaults to FALSE
#' @return vector or matrix, depending on return_mat value
#'
#' @author meireles
i_plot_boundaries = function(return_mat = FALSE) {
    bounds = setNames(par("usr"), c("xmin", "xmax", "ymin", "ymax"))

    if(return_mat) {
        bounds = matrix(bounds, ncol = 2,
                        dimnames = list(c("min", "max"), c("x", "y")))
    }
    bounds
}


#' Moving Average
#'
#' \code{i_mav} computes the moving average of a vector.
#'
#' @param x numeric vector
#' @param n number of points going into the average
#' @param sides TODO
#' @return numeric vector
#'
#' @references http://stackoverflow.com/questions/743812/calculating-moving-average-in-r
#' @author meireles
i_mav = function(x, n = 3, sides = 2){
    filter(x, rep( (1/n), n), sides = sides)
}
