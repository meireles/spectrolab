#' Is the umber a whole number?
#'
#' @param x numbers
#'
#' @return boolean
i_is_whole = function(x){
    as.integer(x) == x
}

#' Is number compatible with being an index?
#'
#' @param x numeric values
#' @param max_length max acceptable values for x
#' @param verbose get warnings?
#'
#' @return
i_is_index = function(x, max_length, verbose = FALSE){
    if(!verbose){
        w = suppressWarnings(i_is_whole(x))
    } else {
        w = i_is_whole(x)
    }

    p = (x > 0 && x <= round(max_length, digits = 0) )
    all(w) && all(p)
}
