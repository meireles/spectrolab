#' Is whole number?
#'
#' \code{i_is_whole} Tests if x is (are) whole numbers
#'
#' @param x single value or vector of numbers
#' @return boolean
#'
#' @keywords internal
#' @author Jose Eduardo Meireles
i_is_whole = function(x){
    r = suppressWarnings(as.integer(x) == x)
    r[is.na(r)] = FALSE
    r
}

#' Compatible with being an index?
#'
#' \code{i_is_index} Tests if x fit the requirements of being indices
#'
#' @param x numeric values
#' @param max_length Max acceptable values for x
#' @param all boolean. If TRUE, a single logical value is retuned. Else, a vector
#'                     of length = length(x) is returned
#' @param allow_negative boolean. Allow negative integers (used in subseting?).
#'                       defaults to FALSE
#' @param quiet boolean. Get warnings?
#' @return boolean
#'
#' @keywords internal
#' @author Jose Eduardo Meireles
i_is_index = function(x, max_length, all = TRUE, allow_negative = FALSE, quiet = TRUE){
    if(quiet){
        w = suppressWarnings(i_is_whole(x))
    } else {
        w = i_is_whole(x)
    }

    if(allow_negative){
        ## Even if allow_negative == TRUE, I cannot allow mixing positive and
        ## negative indices
        if(all(x > 0)) {
            p = x <= round(max_length, digits = 0)
        } else if(all(x < 0)){
            p = x >= round( - max_length, digits = 0)
        } else {
            stop("cannot mix positive and negative indices")
        }
    } else {
        p = x > 0 & x <= round(max_length, digits = 0)
    }

    r = w & p
    if(all){ r = all(r) }
    r
}


#' Match label
#'
#' @param x label vector
#' @param i picked label or NULL
#' @param full boolean. If TRUE, a full list of results is returned
#' @param allow_empty_lookup boolean. If TRUE, x is allowed to be NULL. Defaults
#'        to false
#' @return matched indices, or list in case full = TRUE
#'
#' @keywords internal
#' @author Jose Eduardo Meireles
i_match_label = function(x, i, full = FALSE, allow_empty_lookup = FALSE){

    r = list(matched = NULL, unmatched = NULL, not_element = NULL)
    l = length(x)

    ## Case x doesn't exist
    if(l == 0){
        if(allow_empty_lookup){
            return(NULL)
        } else {
            stop("Invalid label vector (x)")
        }
    }

    ## Case i == NULL: return all incices
    if(missing(i) || is.null(i)){
        r[] = list(seq.int(l), NULL, NULL)

        if(full){
            return(r)
        } else {
            return(r[["matched"]])
        }
    }

    m = which(  x %in% i)
    u = which(! x %in% i)
    n = setdiff(i, x)

    if(full){
        r[] = list(m, u, n)
        return(r)
    } else {
        if( length(n) != 0 || length(n) == length(i) ){
            stop("Following labels not found: ", n)
        }
        return(m)
    }
}


#' Match label or index
#'
#' @param x label vector
#' @param i picked label or idx or NULL
#' @param full boolean. If TRUE, a full list of results is returned
#' @param allow_empty_lookup boolean. If TRUE, x is allowed to be NULL. Defaults
#'        to false
#' @param allow_negative boolean. Allow indices to be negative? Defaults to FALSE
#' @return matched indices
#'
#' @keywords internal
#' @author Jose Eduardo Meireles
i_match_label_or_idx = function(x, i, full = FALSE, allow_empty_lookup = FALSE, allow_negative = FALSE){

    l = length(x)

    if(l == 0){
        if(allow_empty_lookup){
            return(NULL)
        } else {
            stop("Invalid label vector (x)")
        }
    }

    d = i_is_index(x = i, max_length = l, all = FALSE, allow_negative = allow_negative)

    if(any(d)){
        i = as.integer(i)
                                            ## In case the indices are positive
        if(all(i > 0)){
            r = list(matched     = i[d],
                     unmatched   = setdiff(seq(l), i[d]),
                     not_element = i[!d])
        } else {                            ## In case the indices are negative
            ip = abs(i)
            r = list(matched     = setdiff(seq(l), ip[d]),
                     unmatched   = ip[d],
                     not_element = i[!d])
        }


        if(full){
            return(r)
        } else {
            if( length(r[["not_element"]]) != 0 ){
                stop("Following labels not found: ", r[["not_element"]])
            } else {
                return(r[["matched"]])
            }
        }

    } else {
        return( i_match_label(x, i, full = full, allow_empty_lookup = allow_empty_lookup) )
    }

}



#' Find plot boundaries in user space
#'
#' \code{i_plot_boundaries} gets plot boundaries in user space as matrix or vec
#'
#' @param return_mat return a matrix instead of vector? defaults to FALSE
#' @return vector or matrix, depending on return_mat value
#'
#' @importFrom stats setNames
#' @importFrom graphics par
#'
#' @keywords internal
#' @author Jose Eduardo Meireles
i_plot_boundaries = function(return_mat = FALSE) {
    bounds = stats::setNames(graphics::par("usr"), c("xmin", "xmax", "ymin", "ymax"))

    if(return_mat) {
        bounds = matrix(bounds, ncol = 2,
                        dimnames = list(c("min", "max"), c("x", "y")))
    }
    bounds
}


#' Tests if a plot device exists
#'
#' @return boolean
#'
#' @importFrom graphics par
#'
#' @keywords internal
#' @author Jose Eduardo Meireles
i_plot_exists = function(){
    tryCatch( {graphics::par(new = TRUE); TRUE},
              warning = function(x){FALSE})
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
#'
#' @importFrom stats filter
#' @keywords internal
#'
#' @author Jose Eduardo Meireles
i_mav = function(x, n = 3, sides = 2){
    stats::filter(x, rep( (1/n), n), sides = sides)
}
