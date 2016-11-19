#' Is whole number?
#'
#' \code{i_is_whole} Tests if x is (are) whole numbers
#'
#' @param x single value or vector of numbers
#'
#' @return boolean
i_is_whole = function(x){
    as.integer(x) == x
}

#' Compatible with being an index?
#'
#' \code{i_is_index} Tests if x is (are) fir the requirements of being indices
#'
#' @param x numeric values
#' @param max_length max acceptable values for x
#' @param quiet get warnings?
#'
#' @return boolean
i_is_index = function(x, max_length, quiet = TRUE){
    if(quiet){
        w = suppressWarnings(i_is_whole(x))
    } else {
        w = i_is_whole(x)
    }

    p = (x > 0 && x <= round(max_length, digits = 0) )

    all(w) && all(p)
}


#' Find plot boundaries in user sapce
#'
#' \code{i_plot_boundaries} gets plot boundaries in user sapce as matrix or vec
#'
#' @param return_mat return a matrix instead of vector? defaults to FALSE
#'
#' @return vector or matrix, depending on return_mat value
i_plot_boundaries = function(return_mat = FALSE) {
    bounds = setNames(par("usr"), c("xmin", "xmax", "ymin", "ymax"))

    if(return_mat) {
        bounds = matrix(bounds, ncol = 2,
                        dimnames = list(c("min", "max"), c("x", "y")))
    }
    bounds
}
