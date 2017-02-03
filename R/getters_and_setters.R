#' Get internal indexes for spectra attributes
#'
#' \code{i_match_ij_spectra} gets index position matching i and j
#'
#' @param x spectra
#' @param i sample names or indices or boolean vector
#' @param j wavelengths or boolean vector, NOT INDICES
#' @return list if row indices and column indices
#'
#' @keywords internal
#' @author Jose Eduardo Meireles
i_match_ij_spectra = function(x, i = NULL, j = NULL){

    if(is.logical(i)){
        if(length(i) != nrow(x)) {
            stop("boolean vector i must have the same length as the number of samples")
        }
        if(any(i)){
         i =  which(i)
        } else {
            stop("All boolean values are FALSE (no sample matched)")
        }
    }


    if(is.logical(j)){
        if(length(j) != ncol(x)){
            stop("boolean vector i must have the same length as the number of samples")
        }

        if(any(j)){
            j = wavelengths(x)[ which(j) ]
        } else {
            stop("All boolean values are FALSE (no wavelengths matched)")
        }
    }

    r_idx = i_match_label_or_idx( names(x) , i)
    c_idx = i_match_label(wavelengths(x), j)

    list(r_idx = r_idx, c_idx = c_idx)
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
#' @param x spectra object
#' @param i Sample names (preferred), index, or a logical vector of length nrow(x)
#' @param j Wavelength labels, as numeric or character
#'          or a logical vector of length ncol(x). Do not use indexes!
#' @param simplify Boolean. If TRUE (default), single band selections
#'                 are returned as a named vector of reflectance values
#' @return usually a spectra object, but see param `simplify`
#'
#' @author Jose Eduardo Meireles
#' @export
`[.spectra` = function(x, i, j, simplify = TRUE){

    if(missing(i)){ i = NULL }
    if(missing(j)){ j = NULL }

    m = i_match_ij_spectra(x = x, i = i, j = j)

    if(simplify && length(m[["c_idx"]]) == 1) {
        out        = reflectance(x)[ m[["r_idx"]] , m[["c_idx"]], drop = TRUE ]
        names(out) = names(x)[ m[["r_idx"]] ]
        return(out)

    } else {
        out = spectra(reflectance = reflectance(x)[ m[["r_idx"]] , m[["c_idx"]], drop = FALSE ],
                      wavelengths = wavelengths(x)[ m[["c_idx"]] ],
                      names       = names(x)[ m[["r_idx"]] ],
                      meta        = meta(x, label = NULL, sample =  m[["r_idx"]]),
                      enforce01   = enforce01(x) )
        return(out)
    }
}

#' Assign reflectance values to spectra
#'
#' \code{`[<-`} assigns the rhs values to spectra
#'
#' @param x spectra object (lhs)
#' @param i Sample names (preferred), index, or a logical vector of length nrow(x)
#' @param j Wavelength labels, as numeric or character
#'          or a logical vector of length ncol(x). Do not use indexes!
#' @param value value to be assigned (rhs). Must either data coercible to numeric
#'              or another `spectra` obj
#' @return nothing. modifies spectra as side effect
#'
#' @author Jose Eduardo Meireles
#' @export
`[<-.spectra` = function(x, i, j, value){
    if(missing(i)){ i = NULL }
    if(missing(j)){ j = NULL }
    m = i_match_ij_spectra(x = x, i = i, j = j)
    l = lapply(m, length)
    e = enforce01(x)

    if(is_spectra(value)){
        value = reflectance(value)
    }

    ## In case `value` is a scalar:
    ##   1. Do not check for dimension constraints, which leads to
    ##   2. assiging `value` to all elements in the reflectance matrix
    if(is.vector(value) && length(value) == 1){
        l = list(NULL)   ## assign the two elements in `l` to NULL
    }

    x$reflectance[ m[["r_idx"]], m[["c_idx"]] ] = i_reflectance(value, nwavelengths = l[["c_idx"]], nsample = l[["r_idx"]], enforce01 = e)

    x
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
#' @author Jose Eduardo Meireles
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
#' @author Jose Eduardo Meireles
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
#' @author Jose Eduardo Meireles
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
#' @author Jose Eduardo Meireles
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
#' @author Jose Eduardo Meireles
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
#' @author Jose Eduardo Meireles
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
#' @author Jose Eduardo Meireles
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
#' @author Jose Eduardo Meireles
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
#' @param label metadata column index or label
#' @param sample sample index or name
#' @param simplify boolean. defaults to FALSE
#' @param quiet boolean. warn about non-existent metadata? defaults to TRUE
#' @return TODO
#'
#' @author Jose Eduardo Meireles
#' @export
meta = function(x, label, sample, simplify = FALSE, quiet = TRUE){
    UseMethod("meta")
}

#' Set metadata
#'
#' \code{meta} sets metadata
#'
#' @param x spectra object (lhs)
#' @param label metadata column label
#' @param sample sample name
#' @param value rhs. TODO
#' @return nothing. called for its side effect
#'
#' @author Jose Eduardo Meireles
#' @export
`meta<-` = function(x, label, sample, value){
    UseMethod("meta<-")
}

#' @describeIn meta get metadata
#' @export
meta.spectra = function(x, label = NULL, sample = NULL, simplify = FALSE, quiet = TRUE){

    m = i_match_label_or_idx(names(x), i = sample)
    l = i_match_label_or_idx(colnames(x$meta),
                             label,
                             full = TRUE,
                             allow_empty_lookup = TRUE)

    ## Case: User provided a non-existent label
    if( length(l[["not_element"]]) != 0 ){
        if(!quiet){
            message("Following label(s) do(es) not exist: ", label)
        }
        return(NULL)
    }

    x$meta[ m, l[["matched"]], drop = simplify]
}

#' @describeIn meta<- set metadata
#' @export
`meta<-.spectra` = function(x, label = NULL, sample = NULL, value) {

    ## It turns out that is.vector returns TRUE for a list
    ## So I am testing for list BEFORE I test for vector.
    ## Bottomline, order of testing matters here!
    if(is.data.frame(value)){
        vv = value
        nv = ncol(vv)
        lv = colnames(vv)
    } else if (is.matrix(value)) {
        vv = as.data.frame(value)
        nv = ncol(vv)
        lv = colnames(vv)
    } else if (is.list(value)){
        if(length(value) == 1){
            vv = value[[1]]
        } else {
            vv = data.frame(value)
        }
        nv = length(value)
        lv = names(value)
    } else if (is.vector(value)){
        vv = value
        nv = 1
        lv = NULL
    } else if (is.null(value)) {
        vv = NULL
        nv = 0
        lv = NULL
    } else {
        stop("value must be of the following: data.frame, matrix, list or vector")
    }

    if( ! is.null(label) && any(length(label) == nv, nv == 0) ) {
        lv = label
    }

    s     = i_match_label_or_idx(names(x), sample, full = FALSE)
    s_all = is.null(sample) || length(s) == nrow(x)

    m = x$meta

    if(is.null(lv)){
        if(s_all){
            m[ , ] = vv
        } else {
            m[s , ] = vv
        }
    } else {
        if(s_all){
            m[ , lv ] = vv
        } else {
            m[s , lv] = vv
        }
    }

    x$meta = i_meta(m, nrow(x))
    return(x)
}
