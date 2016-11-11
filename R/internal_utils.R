#' Test if argument is a whole number
#'
#' @param x single value or vector of numbers
#'
#' @return boolean
i_is_whole = function(x){
    as.integer(x) == x
}

#' Are numbers compatible with being an index?
#'
#' @param x numeric values
#' @param max_length max acceptable values for x
#' @param verbose get warnings?
#'
#' @return
i_is_index = function(x, max_length, quiet = TRUE){
    if(quiet){
        w = suppressWarnings(i_is_whole(x))
    } else {
        w = i_is_whole(x)
    }

    p = (x > 0 && x <= round(max_length, digits = 0) )

    all(w) && all(p)
}


#' Get plot boundaries in user sapce
#'
#' @param return_mat boolean. return a matrix? defaults to FALSE
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
