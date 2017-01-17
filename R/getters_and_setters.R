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

    r_idx = i_match_label_or_idx( names(this) , i)
    c_idx = i_match_label(wavelengths(this), j)

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

    if(simplify && length(m[["c_idx"]]) == 1) {
        out        = reflectance(this)[ m[["r_idx"]] , m[["c_idx"]], drop = TRUE ]
        names(out) = names(this)[ m[["r_idx"]] ]
        return(out)

    } else {
        out = spectra(reflectance = reflectance(this)[ m[["r_idx"]] , m[["c_idx"]], drop = FALSE ],
                      wavelengths = wavelengths(this)[ m[["c_idx"]] ],
                      names       = names(this)[ m[["r_idx"]] ],
                      meta        = meta(this)[ m[["r_idx"]],  ],
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
#' @param label metadata column index or label
#' @param sample sample index or name
#' @param simplify boolean. defaults to FALSE
#' @return TODO
#'
#' @author meireles
#' @export
meta = function(x, label, sample, simplify = FALSE){
    UseMethod("meta")
}

#' Set metadata
#'
#' \code{meta} sets metadata
#'
#' @param x spectra object (lhs)
# #' @param label metadata column label
# #' @param sample sample name
#' @param value rhs. TODO
#' @return nothing. called for its side effect
#'
#' @author meireles
#' @export
`meta<-` = function(x, value){
    # function(x, label, sample, value)
    UseMethod("meta<-")
}

#' @describeIn meta get metadata
#' @export
meta.spectra = function(x, label = NULL, sample = NULL, simplify = FALSE){

    if(is.null(x$meta)){
        return(NULL)
    }

    m = i_match_ij_spectra(this = x, i = sample, j = NULL)
    l = i_match_label_or_idx(colnames(x$meta), label)

    x$meta[ m[["r_idx"]], l, drop = simplify]
}

#' @describeIn meta<- set metadata
#' @export
`meta<-.spectra` = function(x, value){
    x$meta = i_meta(value, nrow(x))
    x
    #`meta<-.spectra` = function(x, label, sample = NULL, value){

    # m_is_null = is.null(x$meta)
    # v_is_null = is.null(value)
    #
    # ########################################
    # ## 1st RETURNING block
    # ########################################
    #
    # ## Nothing happening
    # if(m_is_null && v_is_null){
    #     return(x)
    # }
    #
    # ## Assignment on blank
    # ## sample: ignored.
    # ## label: takes precedence over names in value
    # if(m_is_null){
    #     v = as.data.frame(value)
    #     if(length(label) == length(v)){ colnames(v) = label }
    #     x$meta = i_meta(v, nrow(x))
    #     return(x)
    # }
    #
    # ########################################
    # ## Matching
    # ########################################
    #
    # ## Sample match
    # s_match     = i_match_label_or_idx(names(x), sample, full = FALSE)
    # s_match_all = length(s_match) == nrow(x)
    #
    # ## Label names
    # l_match_f   = i_match_label_or_idx(names(x$meta), label, full = TRUE)
    # l_match     = l_match_f[["matched"]]
    # l_match_all = length(l_match) == ncol(x$meta)
    #
    # ########################################
    # ## 2nd RETURNING block
    # ########################################
    #
    # ## Removing metadata
    # if(v_is_null){
    #     if(l_match_all){
    #         x$meta = i_meta(NULL, nrow(x))
    #         return(x)
    #     } else if(length(l_match) > 0){
    #         m = x$meta
    #         m[ , l_match] = NULL
    #         x$meta = i_meta(m, ncol(x))
    #         return(x)
    #     }
    # }
    #
    # ########################################
    # ## MODIFYING x$meta
    # ########################################
    # m = x$meta
    # m[ s_match, label] = value
    # x$meta = i_meta(m, nrow(x))
    # x
}
