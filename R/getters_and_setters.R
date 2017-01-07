#' Get internal indexes for spectra attributes
#'
#' \code{i_match_ij_spectra} gets position matching if the reflectance matrix
#'
#' @param this spectra
#' @param i sample names or indices
#' @param j wavelengths, not indices
#' @return list if row indices and column indices
#'
#' @author meireles
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
#' @param simplify Boolean. If TRUE (default), single band selections
#'                 are returned as a named vector of reflectance values
#' @return usually a spectra object, but see param `simplify`
#'
#' @author meireles
#' @export
`[.spectra` = function(this, i, j, simplify = TRUE){

    if(missing(i)){ i = NULL }
    if(missing(j)){ j = NULL }

    m = i_match_ij_spectra(this = this, i = i, j = j)

    ## In
    if(simplify && length(m[["c_idx"]]) == 1) {
        out        = reflectance(this)[ m[["r_idx"]] , m[["c_idx"]], drop = TRUE ]
        names(out) = names(this)[ m[["r_idx"]] ]
        return(out)

    } else {
        out = spectra(reflectance = reflectance(this)[ m[["r_idx"]] , m[["c_idx"]], drop = FALSE ],
                      wavelengths = wavelengths(this)[ m[["c_idx"]] ],
                      names       = names(this)[ m[["r_idx"]] ],
                      meta        = this$meta[ m[["r_idx"]] ],                 ### TODO: Figure out $ operator
                      enforce01   = enforce01(this) )
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
#' @return nothing. modifies spectra as side effect
#'
#' @author meireles
#' @export
`[<-.spectra` = function(this, i, j, value){
    if(missing(i)){ i = NULL }
    if(missing(j)){ j = NULL }
    m = i_match_ij_spectra(this = this, i = i, j = j)
    l = lapply(m, length)
    e = enforce01(this)

    if(is_spectra(value)){
        value = reflectance(value)
    }

    ## In case `value` is a scalar:
    ##   1. Do not check for dimension constraints, which leads to
    ##   2. assiging `value` to all elements in the reflectance matrix
    if(is.vector(value) && length(value) == 1){
        l = list(NULL)   ## assign the two elements in `l` to NULL
    }

    this$reflectance[ m[["r_idx"]], m[["c_idx"]] ] = i_reflectance(value, nwavelengths = l[["c_idx"]], nsample = l[["r_idx"]], enforce01 = e)

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
#' @return matrix with samples in rows and wavelengths in columns
#'
#' @author meireles
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
#' @return nothing. called for its side effect
#'
#' @author meireles
#' @export
`reflectance<-` = function(x, value){
    UseMethod("reflectance<-")
}


#' @describeIn reflectance Get spectra reflectance
#' @export
reflectance.spectra = function(x){
    x$reflectance
}


#' @describeIn reflectance<- Set spectra reflectance
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
#' @return Boolean
#'
#' @author meireles
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
#' @return nothing. has a *side effect* of changing if a constraint is enforced
#'
#' @author meireles
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
#' @return vector of sample names
#'
#' @author meireles
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
#' @return nothing. called for its side effect.
#'
#' @author meireles
#' @export
`names<-.spectra` = function(x, value){
    x$names = i_names(value, nrow(x))
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
#'
#' @author meireles
#' @export
wavelengths = function(x, min = NULL, max = NULL, return_num = TRUE){
    UseMethod("wavelengths")
}


#' Set wavelength labels
#'
#' \code{wavelengths} sets wavelength labels of lhs to the rhs values
#'
#' @param x spectra object (lhs)
#' @param unsafe boolean. Skip length safety check? Defaults to FALSE
#' @param value rhs
#' @return nothing. called for its side effect.
#'
#' @author meireles
#' @export
`wavelengths<-` = function(x, unsafe = FALSE, value){
    UseMethod("wavelengths<-")
}


#' @describeIn wavelengths Set spectra wavelength labels
#' @export
wavelengths.spectra = function(x, min = NULL, max = NULL, return_num = TRUE) {

    wl   = as.numeric(x$wavelengths)
    min  = ifelse(is.null(min), min(wl), as.numeric(min) )
    max  = ifelse(is.null(max), max(wl), as.numeric(max) )
    pick = wl >= min & wl <= max

    if(all(!pick)){
        stop("No wavelength matches the given conditions")
    }

    if(!return_num) {
        wl = as.character(wl)
    }

    wl[pick]
}


#' @describeIn wavelengths<- Set spectra wavelength labels
#' @export
`wavelengths<-.spectra` = function(x, unsafe = FALSE, value){

    if(unsafe){
        x$wavelengths = i_wavelengths(value, NULL)
    } else {
        x$wavelengths = i_wavelengths(value, ncol(x))
    }
    x
}


########################################
# meta
########################################


#' Get metadata
#'
#' \code{meta} returns metadata of spectra
#'
#' @param x spectra object
#' @param i sample index or names
#' @param k metadata column index or names
#' @param simplify boolean. defaults to TRUE
#' @return TODO
#'
#' @author meireles
#' @export
meta = function(x, i, k, simplify = TRUE){
    UseMethod("meta")
}

#' Set metadata
#'
#' \code{meta} sets metadata
#'
#' @param x spectra object (lhs)
#' @param value rhs
#' @return nothing. called for its side effect
#'
#' @author meireles
#' @export
`meta<-` = function(x, value){
    UseMethod("meta<-")
}

#' @describeIn meta get metadata
#' @export
meta.spectra = function(x, i, k, simplify = TRUE){

    if(is.null(x$meta)){
        return(NULL)
    }

    if(missing(i)){
        i = seq.int(nrow(x$meta))
    }
    if(missing(k)){
        k = seq.int(ncol(x$meta))
    }

    m = i_match_ij_spectra(this = x, i = i, j = NULL)

    x$meta[ m[["r_idx"]], k, drop = simplify]
}

#' @describeIn meta<- set metadata
#' @export
`meta<-.spectra` = function(x, value){
    x$meta = i_meta(value, nrow(x))
    x
}
