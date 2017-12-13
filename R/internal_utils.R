#' Is increasing
#'
#' \code{i_is_increasing} tests if wavelength values are increasing
#'
#' Many transform functions can only (or at least should only) be applied to
#' spectra with monotonically varying (very likely increasing) wavelength values.
#' \code{i_is_increasing} tests that case and may throw an error
#' or return the boolean result from the test.
#'
#' @param x wavelengths
#' @param stop boolean. Throw error if test fails? Defaults to TRUE
#' @param call boolean. If stop = TRUE, should the function call be printed?
#' @return boolean
#'
#' @author Jose Eduardo Meireles
#' @keywords internal
i_is_increasing = function(x, stop = TRUE, call = FALSE){
    y = all(diff(x) >= 0.0)
    if( !y && stop){
        stop("Wavelength values must be strictly increasing. You probably need to run `match_overlap` first", call. = call)
    }
    y
}


#' Is whole number?
#'
#' \code{i_is_whole} Tests if x is (are) whole numbers
#'
#' @param x single value or vector of numbers
#' @return boolean
#'
#' @author Jose Eduardo Meireles
#' @keywords internal
i_is_whole = function(x){
    r = suppressWarnings(as.integer(x) == x)
    r[is.na(r)] = FALSE
    r
}

#' Compatible with being an index?
#'
#' \code{i_is_index} Tests if x fit the requirements of being indices
#'
#' This function potentially allows negative indices, given that they may be used
#' with the intent of removing an entry that corresponds to the index. Conversely,
#' zero is never used as an index in R and is not recognized as such here.
#'
#' @param x numeric values
#' @param max_length Max acceptable values for x (inclusive). Must be >= 1
#' @param allow_negative boolean. Count negative integers as indices? defaults to FALSE
#'
#' @return boolean
#'
#' @author Jose Eduardo Meireles
#' @keywords internal
i_is_index = function(x, max_value, allow_negative = FALSE){

    if(max_value < 1){
        stop("max_value must be >= 1")
    }

    w = i_is_whole(x)

    ## In case there are no whole numbers, return result
    if( all( !w )){
        warning("None of the indices are whole numbers.")
        return(w)
    }

    ## Case some values are whole numbers.
    ## Even if negative values are allowed, x cannot have both negative and
    ## positive values...
    if(all(x >= 0)){
        p = x <= round(max_value, digits = 0) & x != 0
    } else if (all(x <= 0) ){
        if(allow_negative){
            p = x >= round( - max_value, digits = 0)  & x != 0
        } else {
            p = x >= round(  max_value, digits = 0)  & x != 0
        }
    } else {
        stop("cannot mix positive and negative indices")
    }

    ## Combine information from `w` and `p` and return
    w & p
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
#' @author Jose Eduardo Meireles
#' @keywords internal
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

    ds = match(x, i)
    dw = which(!is.na(ds))
    ds = order(ds[dw])

    m = dw[ds]
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
#' @author Jose Eduardo Meireles
#' @keywords internal
i_match_label_or_idx = function(x, i, full = FALSE, allow_empty_lookup = FALSE, allow_negative = FALSE){


    ################################################################################
    # match index function
    # HACK. this function should've been declared outside
    ################################################################################

    i_match_index = function(ii, dd, ll){
        ii = as.integer(ii)
        ## In case the indices are positive
        if(all(ii > 0)){
            r = list(matched     = ii[dd],
                     unmatched   = setdiff(seq(ll), ii[dd]),
                     not_element = ii[!dd])
        } else {                            ## In case the indices are negative
            ip = abs(ii)
            r = list(matched     = setdiff(seq(ll), ip[dd]),
                     unmatched   = ip[dd],
                     not_element = ii[!dd])
        }
        r
    }

    ################################################################################
    # Begin i_match_label_or_idx
    ################################################################################

    l = length(x)

    if(l == 0){
        if(allow_empty_lookup){
            return(NULL)
        } else {
            stop("Invalid label vector (x)")
        }
    }

    ########################################
    # First try to match to label
    ########################################
    m = i_match_label(x, i, full = TRUE, allow_empty_lookup = allow_empty_lookup)

    ## return matched by lable if clean cut
    if(length(m$matched) > 0 && length(m$not_element) == 0){
        if(full){
            return(m)
        } else {
            return(m$matched)
        }
    }

    ########################################
    # Now match to index
    ########################################
    d = i_is_index(x = i, max_value = l, allow_negative = allow_negative)

    if (any(d)){
        r = i_match_index(ii = i, dd = d, ll = l)
    }

    ## return matched by index if clean cut
    if(all(d)){
        if(full){
            return(r)
        } else {
            return(r$matched)
        }
    }

    ########################################
    # Try both
    ########################################

    message("Trying to match by label...")
    if(length(m$matched) > 0 && length(m$not_element) != length(i)){
        if(full){
            return(m)
        } else {
            warning("Following label not found:", i[m$not_element])
            return(m$matched)
        }
    }

    message("Trying to match by index...")
    if (any(d)) {
        if(full){
            return(r)
        } else {
            if( length(r[["not_element"]]) != 0 ){
                warning("Following indices not found: ", r[["not_element"]])
                return(r[["matched"]])
            }
        }
    }
    stop("No match.")
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
#
#' @author Jose Eduardo Meireles
#' @keywords internal
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
#' @author Jose Eduardo Meireles
#' @keywords internal
i_plot_exists = function(){
    tryCatch( {graphics::par(new = TRUE); TRUE}, warning = function(x){FALSE})
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
#'
#' @author Jose Eduardo Meireles
#' @keywords internal
i_mav = function(x, n = 3, sides = 2){
    stats::filter(x, rep( (1/n), n), sides = sides)
}



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
