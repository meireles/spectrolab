#' Get internal indexes for spectra attributes
#'
#' \code{i_match_ij_spectra} gets index position matching i and j
#'
#' @param x spectra
#' @param i sample names or indices or boolean vector
#' @param j bands or boolean vector, NOT INDICES
#' @param allow_negative boolean. Allow indices i to be negative? Defaults to
#'                       FALSE
#' @return list if row indices and column indices
#'
#' @keywords internal
#' @author Jose Eduardo Meireles
i_match_ij_spectra = function(x, i = NULL, j = NULL, allow_negative = FALSE){

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
            j = bands(x)[ which(j) ]
        } else {
            stop("All boolean values are FALSE (no bands matched)")
        }
    }

    r_idx = i_match_label_or_idx( names(x) , i, allow_negative = allow_negative)
    c_idx = i_match_label(bands(x), j, allow_negative = allow_negative)

    list(r_idx = r_idx, c_idx = c_idx)
}


#' Subset spectra
#'
#' \code{`[`} Subsets spectra by sample names (rows) or (and) bands (columns)
#'
#' Subset operations based on samples (first argument) will match sample
#' names or indexes, in that order. The spectra constructor ensures that names are
#' not numeric nor are coercible to numeric, such that x[1:2, ] will return the
#' first and second samples in the `spectra` object. Subsetting based on bands
#' (second argument) matches the band labels, not indices! That is, x[ , 600]
#' will give you the value data for the 600nm band and not the 600th
#' band. Boolean vectors of the appropriate length can be used to subset samples
#' and bands.
#'
#' @param x spectra object
#' @param i Sample names (preferred), index, or a logical vector of length nrow(x)
#' @param j band labels, as numeric or character
#'          or a logical vector of length ncol(x). Do not use indexes!
#' @param simplify Boolean. If TRUE (default), single band selections
#'                 are returned as a named vector of value values
#' @return usually a spectra object, but see param `simplify`
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' spec = as.spectra(spec_matrix_example, name_idx = 1)
#' head(names(spec), n = 3)
#' # by name
#' spec1 = spec[ "species_7" , ]
#' spec1
#' # by band
#' spec2 = spec[ , 400:700 ]
#' spec2
`[.spectra` = function(x, i, j, simplify = TRUE){

    if(missing(i)){ i = NULL }
    if(missing(j)){ j = NULL }

    m = i_match_ij_spectra(x = x, i = i, j = j, allow_negative = TRUE)

    if(simplify && length(m[["c_idx"]]) == 1) {
        out        = value(x)[ m[["r_idx"]] , m[["c_idx"]], drop = TRUE ]
        names(out) = names(x)[ m[["r_idx"]] ]
        return(out)

    } else {
        out = spectra(value = value(x)[ m[["r_idx"]] , m[["c_idx"]], drop = FALSE ],
                      bands = bands(x)[ m[["c_idx"]] ],
                      names = names(x)[ m[["r_idx"]] ],
                      meta  = meta(x, label = NULL, sample =  m[["r_idx"]])
        )
        return(out)
    }
}

#' Assign values to spectra
#'
#' \code{`[<-`} assigns the rhs values to spectra
#'
#' @param x spectra object (lhs)
#' @param i Sample names (preferred), index, or a logical vector of length nrow(x)
#' @param j band labels, as numeric or character
#'          or a logical vector of length ncol(x). Do not use indexes!
#' @param value value to be assigned (rhs). Must either data coercible to numeric
#'              or another `spectra` obj
#' @return nothing. modifies spectra as side effect
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' spec = as.spectra(spec_matrix_example, name_idx = 1)
#' spec[ , 400:500] = spec[ , 400:500] * 1.2
#' spec
`[<-.spectra` = function(x, i, j, value){

    if(missing(i)){ i = NULL }
    if(missing(j)){ j = NULL }

    m = i_match_ij_spectra(x = x, i = i, j = j, allow_negative = FALSE)
    l = lapply(m, length)


    ## In case "value" is a spectra object, every component of spectra must be updated
    if(is_spectra(value)){
        if( !identical(bands(x)[m$c_idx], bands(value))){
            stop("wavelenegths not compatible")
        }

        if( any(colnames(meta(x)) !=  colnames(meta(value))) ){
            stop("metadata columns not compatible. names must be exactly the same")
        }

        if(l$r_idx == nrow(value)){
            value(x)[m$r_idx, m$c_idx] = value(value)
            names(x)[m$r_idx]                = names(value)
            meta(x)[m$r_idx, ]               = meta(value)
        } else if ( nrow(value) == 1){
            value(x)[m$r_idx, m$c_idx] = value(value)[ rep(x = 1, l$r_idx), ]
            names(x)[m$r_idx]                = rep(names(value), l$r_idx)
            meta(x)[m$r_idx, ]               = meta(value)[rep(1, l$r_idx), ]
        } else {
            stop("spectra not compatible.")
        }
    } else {
        ## In case "value" is something else, only update the value

        ## If value is a scalar
        if(is.vector(value) && length(value) == 1){
            value(x)[ m$r_idx , m$c_idx ] = matrix(value, l$r_idx, l$c_idx)
        } else {
            value(x)[ m$r_idx , m$c_idx ] = value
        }
    }

    x
}

########################################
# value
########################################

#' Get spectra value
#'
#' \code{value} returns the value matrix from spectra
#'
#' @param x spectra object
#' @return matrix with samples in rows and bands in columns
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' spec = as.spectra(spec_matrix_example, name_idx = 1)
#' is.matrix(value(spec))
value = function(x){
    UseMethod("value")
}

#' Set spectra value
#'
#' \code{value} Assigns the rhs to the value of the lhs spectra obj
#'
#' @param x spectra object
#' @param value value to be assigned to the lhs
#' @return nothing. called for its side effect
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' spec = as.spectra(spec_matrix_example, name_idx = 1)
#' # scale all refletance values by 2
#' value(spec) = value(spec) * 2
`value<-` = function(x, value){
    UseMethod("value<-")
}


#' @describeIn value Get spectra value
#' @export
value.spectra = function(x){
    x$value
}

#' @describeIn value<- Set spectra value
#' @export
`value<-.spectra` = function(x, value){

    ### ORIGINAL code was calling the spectra setter ###
    # x[] = value
    # x
    ####################################################

    x$value = i_value(value, nbands = ncol(x), nsample = nrow(x))
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
#'
#' @examples
#' library(spectrolab)
#' spec = as.spectra(spec_matrix_example, name_idx = 1)
#' names(spec)
names.spectra = function(x){
    x$names
}


#' Set spectra sample names
#'
#' \code{names} assigns sample names to lhs
#'
#' Sample names must not be coercible to numeric. That is, names such as "1" and
#' "153.44" are invalid even if they are encoded as character. names will add the
#' prefix "spec_" to any element of value that is coercible to numeric.
#'
#' @param x spectra object (lhs)
#' @param value values to be assigned (rhs)
#' @return nothing. called for its side effect.
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' spec = as.spectra(spec_matrix_example, name_idx = 1)
#' names(spec) = toupper(names(spec))
`names<-.spectra` = function(x, value){
    x$names = i_names(value, nrow(x), prefix = NULL)
    x
}

########################################
# bands
########################################

#' Get spectra band labels
#'
#' \code{bands} returns a vector of band labels from spectra
#'
#' @param x spectra object
#' @param min = NULL
#' @param max = NULL
#' @param return_num boolean. return vector of numeric values (default).
#'                   otherwise, a vector of strings is returned
#' @return vector of bands. numeric if `return_num` = TRUE (default).
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' spec = as.spectra(spec_matrix_example, name_idx = 1)
#' head(bands(spec))
bands = function(x, min = NULL, max = NULL, return_num = TRUE){
    UseMethod("bands")
}


#' Set band labels
#'
#' \code{bands} sets band labels of lhs to the rhs values
#'
#' @param x spectra object (lhs)
#' @param unsafe boolean. Skip length safety check? Defaults to FALSE
#' @param value rhs
#' @return nothing. called for its side effect.
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' spec = as.spectra(spec_matrix_example, name_idx = 1)
#' bands(spec) = bands(spec) / 1000
`bands<-` = function(x, unsafe = FALSE, value){
    UseMethod("bands<-")
}


#' @describeIn bands Get spectra band labels
#' @export
bands.spectra = function(x, min = NULL, max = NULL, return_num = TRUE) {

    wl   = as.numeric(x$bands)
    min  = ifelse(is.null(min), min(wl), as.numeric(min) )
    max  = ifelse(is.null(max), max(wl), as.numeric(max) )
    pick = wl >= min & wl <= max

    if(all(!pick)){
        stop("No band matches the given conditions")
    }

    if(!return_num) {
        wl = as.character(wl)
    }

    wl[pick]
}


#' @describeIn bands<- Set spectra band labels
#' @export
`bands<-.spectra` = function(x, unsafe = FALSE, value){

    if(unsafe){
        x$bands = i_bands(value, NULL)
    } else {
        x$bands = i_bands(value, ncol(x))
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
#' @return data frame or vector
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' spec = as.spectra(spec_matrix_example, name_idx = 1)
#' spec = normalize(spec)
#' meta(spec, "normalization_magnitude")
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
#'
#' @examples
#' library(spectrolab)
#' spec = as.spectra(spec_matrix_example, name_idx = 1)
#' meta(spec, "random") = rnorm(nrow(spec), mean(10), sd = 2)
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
