#' Get internal indexes for spectra attributes
#'
#' \code{i_match_ij_spectra} gets position matching if the reflectance matrix
#'
#' @param this spectra
#' @param i sample names or indices
#' @param j wavelengths, not indices
#'
#' @return list if row indices and column indices
i_match_ij_spectra = function(this, i = NULL, j = NULL){
    ## subset by samples i.e. rows
    if(is.null(i)){
        r_match = seq(nrow(this))
    } else {
        r_match    = which(names(this) %in% i)
        r_no_match = setdiff(i, names(this))

        if( length(r_no_match) != 0 || length(r_no_match) == length(i) ){
            if( i_is_index(i, dim(this)["n_samples"]) ) {
                r_match = as.integer(i)
            } else {
                stop("Sample subscript out of bounds: \n", r_no_match)
            }
        }
    }

    ## subset by wavelength. i.e. columns
    if(is.null(j)){
        c_match = seq(ncol(this))
    } else {
        c_match = match(j, wavelengths(this))
        if(any(is.na(c_match))){
            stop("Wavelength subscript out of bounds. Use wavelength labels instead of raw indices.")
        }
    }
    list(r_idx = r_match, c_idx = c_match)
}


#' Subset spectra
#'
#' \code{`[`} Subsets spectra by sample names (rows) or (and) wavelengths (columns)
#'
#' Subset operations based on samples (first argument) will match either sample
#' names or indexes, in that order. That is, if you subset x[1:2 , ] and your
#' sample names contain 1 and 2, you will get the spectra with names in
#' c(1, 2) and not (at least necessarily) the first and second samples in the
#' `spectra` object.
#'
#' @param this spectra object
#' @param i Sample names (preferred) or index.
#' @param j Wavelength labels, as numeric or character Do not use indexes.
#'
#' @return spectra object
#' @export
`[.spectra` = function(this, i, j){

    if(missing(i)){ i = NULL }
    if(missing(j)){ j = NULL }

    m = i_match_ij_spectra(this = this, i = i, j = j)

    ############################################################################
    ## Original implementation
    ##
    ## must profile. Not good because it doesn't carry over attributes of the
    ## recletance data. I will try to patch by only re-running the

    ## subset. drop = false is needed to return a matrix instead of vec when nsample = 1
    # this$reflectance  = this$reflectance[ m[["r_idx"]] , m[["c_idx"]], drop = FALSE ]
    # this$wavelengths  = this$wavelengths[ m[["c_idx"]] ]
    # this$names        = this$names[ m[["r_idx"]] ]
    #
    # this
    ############################################################################

    if(length(m[["c_idx"]]) == 1) {
        out        = this$reflectance[ m[["r_idx"]] , m[["c_idx"]], drop = TRUE ]
        names(out) = this$names[ m[["r_idx"]] ]
        return(out)

    } else {
        out = spectra(reflectance = this$reflectance[ m[["r_idx"]] , m[["c_idx"]], drop = FALSE ],
                      wavelengths = this$wavelengths[ m[["c_idx"]] ],
                      names       = this$names[ m[["r_idx"]] ],
                      meta        = this$meta[ m[["r_idx"]] ],
                      enforce01   = attr(this$reflectance, "enforce01") )
        return(out)
    }
}

#' Assign reflectance values to spectra
#'
#' \code{`[<-`} assigns the rhs values to spectra
#'
#' @param this spectra object (lhs)
#' @param i sample name
#' @param j wavelength
#' @param value value to be assigned (rhs)
#'
#' @return nothing. modifies spectra as side effect
#' @export
`[<-.spectra` = function(this, i, j, value){
    if(missing(i)){ i = NULL }
    if(missing(j)){ j = NULL }
    m = i_match_ij_spectra(this = this, i = i, j = j)
    l = lapply(m, length)
    e = attr(this$reflectance, "enforce01")

    if(is_spectra(value)){
        value = reflectance(value)
    }

    ## In case `value` is a scalar:
    ##    1. Do not enforce dimension constraints
    ##    2. Use default behavior of applying the scalar to all elements in
    ##       the matrix
    if(length(unlist(value, recursive = TRUE)) == 1) {
        this$reflectance[ m[["r_idx"]], m[["c_idx"]] ] = i_reflectance(value, enforce01 = e)
    } else {
        this$reflectance[ m[["r_idx"]], m[["c_idx"]] ] = i_reflectance(value, nwavelengths = l[["c_idx"]], nsample = l[["r_idx"]], enforce01 = e)
    }

    this
}

########################################
# reflectance
########################################

#' Get spectra reflectance
#'
#' \code{reflectance} returns the reflectance matrix from spectra
#'
#' @param x spectra object
#'
#' @return matrix with samples in rows and wavelengths in columns
#' @export
reflectance = function(x){
    UseMethod("reflectance")
}

#' Set spectra reflectance
#'
#' \code{reflectance} Assigns the rhs to the reflectance of the lhs spectra obj
#'
#' @param x spectra object
#' @param value value to be assigned to the lhs
#'
#' @return nothing. called for its side effect
#' @export
`reflectance<-` = function(x, value){
    UseMethod("reflectance<-")
}


#' @describeIn reflectance Get spectra reflectance
#' @export
reflectance.spectra = function(x){
    x$reflectance
}


#' @describeIn reflectance<- Get spectra reflectance
#' @export
`reflectance<-.spectra` = function(x, value){
    x[] = value
}

########################################
# Reflectance: SIDE EFFECT!
########################################

#' reflectance constraint status
#'
#' \code{enforce01} gets if a reflectance constraint (0 - 1) is being enforced
#'
#' @param x spectra object
#'
#' @return Boolean
#' @export
enforce01 = function(x){
    UseMethod("enforce01")
}

#' Enforce reflectance between 0 and 1
#'
#' \code{enforce01<-} sets or unsets a spectra reflectance constraint (0 - 1)
#'
#' @param x spectra object
#' @param value boolean.
#'
#' @return nothing. has a **side effect** of changing if a constraint is enforced
#' @export
`enforce01<-` = function(x, value){
    UseMethod("enforce01<-")
}

#' @describeIn enforce01 Get reflectance constraint status
#' @export
enforce01.spectra = function(x){
    attr(x$reflectance, "enforce01")
}

#' @describeIn enforce01<- Set reflectance constraint status
#' @export
`enforce01<-.spectra` = function(x, value){
    if(! is.logical(value)){
        stop("value must be boolean")
    }
    if(value){
        if(min(range(x)) < 0 || max(range(x)) > 1.0){
            stop("Cannot set enforce01 = TRUE because reflectance values outside 0-1 were found. Take care of those refletance values and try again.")
        }
    }

    attr(x$reflectance, "enforce01") <- value
    x
}


########################################
# sample names
########################################

#' Get spectra sample names
#'
#' \code{names} returns a vector of sample names
#'
#' @param x spectra object
#'
#' @return vector of sample names
#' @export
names.spectra = function(x){
    x$names
}


#' Set spectra sample names
#'
#' \code{names} assigns sample names to lhs
#'
#' @param x spectra object (lhs)
#' @param value values to be assigned (rhs)
#'
#' @return nothing. called for its side effect.
#' @export
`names<-.spectra` = function(x, value){

    ## Assign sample names using sample names using internal constructor.
    ## This should:
    ##  (1) check for all requirements of names, including length (i.e. nrow(x))
    ##  (2) throw if requirements are not met.
    x$names = i_names(value, nrow(x))

    ## Return
    x
}

########################################
# wavelengths
########################################

#' Get spectra wavelength labels
#'
#' \code{wavelengths} returns a vector of wavelength labels from spectra
#'
#' @param x spectra object
#' @param min = NULL
#' @param max = NULL
#' @param return_num boolean. return vector of numeric values (default).
#'                   otherwise, a vector of strings is returned
#' @return vector of wavelengths. numeric if `return_num` = TRUE (default).
#' @export
wavelengths = function(x, min = NULL, max = NULL, return_num = TRUE){
    UseMethod("wavelengths")
}


#' Set wavelength labels
#'
#' \code{wavelengths} sets wavelength labels of lhs to the rhs values
#'
#' @param x spectra object (lhs)
#' @param unsafe boolean. Skip safety check? Defaults to FALSE
#' @param value rhs
#'
#' @return nothing. called for its side effect.
#' @export
`wavelengths<-` = function(x, unsafe = FALSE, value){
    UseMethod("wavelengths<-")
}


#' @describeIn wavelengths Set spectra wavelength labels
#' @export
wavelengths.spectra = function(x, min = NULL, max = NULL, return_num = TRUE) {

    wl = as.numeric(x$wavelengths)

    if(is.null(min) && is.null(max)) {
        if(return_num) {
            return( wl )
        } else {
            return( as.character(wl) )
        }
    }

    min = ifelse(is.null(min), min(wl), as.numeric(min))
    max = ifelse(is.null(max), max(wl), as.numeric(max))

    pick = wl >= min & wl <= max

    if(any(pick)){

        wl = wl[pick]

        if(return_num) {
            return( wl )
        } else {
            return( as.character(wl) )
        }

    } else {
        stop("No wavelength matches the given conditions")
    }

}


#' @describeIn wavelengths<- Set spectra wavelength labels
#' @export
`wavelengths<-.spectra` = function(x, unsafe = FALSE, value){

    ## Assign new wavelength values constructed using the internal constructor.
    ## Unless unsafe == TRUE, this should:
    ##  (1) check for all requirements of wavelengths, including length (i.e. ncol(x) )
    ##  (2) throw if requirements are not met.
    if(unsafe){
        x$wavelengths = i_wavelengths(value, NULL)
    } else {
        x$wavelengths = i_wavelengths(value, ncol(x))
    }
    ## return
    x
}
