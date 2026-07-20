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

    r_idx = i_match_label_or_idx( names(x) , i, allow_negative = allow_negative)

    ## A logical `j` is inherently positional: convert the mask straight to
    ## column indices. Do NOT round-trip it through band labels --- a mask that
    ## selects one of several duplicated overlap wavelengths would otherwise
    ## return every column sharing that label (see the duplicate-band contract).
    if(is.logical(j)){
        if(length(j) != ncol(x)){
            stop("boolean vector j must have the same length as the number of bands")
        }
        if(any(j)){
            c_idx = which(j)
        } else {
            stop("All boolean values are FALSE (no bands matched)")
        }
    } else {
        c_idx = i_match_label(bands(x), j, allow_negative = allow_negative)
    }

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
#' Band labels need not be unique. A raw, un-spliced full-range spectrum can carry
#' the same wavelength twice where two detectors overlap. Selecting a duplicated
#' band label (e.g. \code{x[, 975.6]} when 975.6 is sampled by two detectors)
#' returns \strong{all} matching bands --- exactly as selecting a duplicated
#' sample name returns all matching rows --- and emits a message so the
#' duplication is not silent. Splice the sensors with \code{\link{match_sensors}}
#' to obtain unique, strictly increasing bands.
#'
#' @param x spectra object
#' @param i Sample names (preferred), index, or a logical vector of length nrow(x)
#' @param j band labels, as numeric or character
#'          or a logical vector of length ncol(x). Do not use indexes!
#' @param simplify Boolean. If TRUE (default), a selection matching exactly one
#'                 band is returned as a named vector of values.
#' @return usually a spectra object, but see param `simplify`
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' spec = as_spectra(spec_matrix_example, name_idx = 1)
#' head(names(spec), n = 3)
#' # by name
#' spec1 = spec[ "species_7" , ]
#' spec1
#' # by band
#' spec2 = spec[ , 400:700 ]
#' spec2
`[.spectra` = function(x, i, j, simplify = TRUE){

    j_given = !missing(j)
    if(missing(i)){ i = NULL }
    if(missing(j)){ j = NULL }

    m = i_match_ij_spectra(x = x, i = i, j = j, allow_negative = TRUE)

    ## When the user explicitly selects bands and the selection lands on
    ## duplicated labels, all matching bands are returned (as for duplicated
    ## sample names). Say so rather than surprise the user with extra columns.
    if( j_given ){
        sel_bands = bands(x)[ m[["c_idx"]] ]
        dups      = unique(sel_bands[ duplicated(sel_bands) ])
        if( length(dups) > 0 ){
            message("Selected band label(s) ", paste(dups, collapse = ", "),
                    " match more than one band (duplicated wavelengths); all ",
                    "matching bands were returned. Run match_sensors() to splice ",
                    "overlapping sensors into unique, increasing bands.")
        }
    }

    if(simplify && j_given && length(m[["c_idx"]]) == 1) {
        out        = value(x)[ m[["r_idx"]] , m[["c_idx"]], drop = TRUE ]
        names(out) = names(x)[ m[["r_idx"]] ]
        return(out)

    } else {
        out = spectra(value = value(x)[ m[["r_idx"]] , m[["c_idx"]], drop = FALSE ],
                      bands = bands(x)[ m[["c_idx"]] ],
                      names = names(x)[ m[["r_idx"]] ],
                      meta  = meta(x, label = NULL, sample =  m[["r_idx"]])
        )

        ## Carry the per-sample sensor_info provenance through the subset (rows
        ## follow the selected samples). See R/sensor_info.R.
        si = sensor_info(x)
        if( !is.null(si) ){
            attr(out, "sensor_info") = si[ m[["r_idx"]], , drop = FALSE ]
        }

        ## quantity/wavelength_unit are whole-object provenance, unaffected by
        ## subsetting rows or bands. See R/provenance.R.
        attr(out, "quantity")        = attr(x, "quantity")
        attr(out, "wavelength_unit") = attr(x, "wavelength_unit")

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
#' @return the modified `spectra` object
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' spec = as_spectra(spec_matrix_example, name_idx = 1)
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
            stop("wavelengths not compatible")
        }

        if( !identical(colnames(meta(x)), colnames(meta(value))) ){
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
#' spec = as_spectra(spec_matrix_example, name_idx = 1)
#' is.matrix(value(spec))
value = function(x){
    UseMethod("value")
}

#' @title Set spectra value
#' @name value<-
#' @description \code{value<-} Assigns the rhs to the value of the lhs spectra obj
#'
#' @param x spectra object
#' @param value value to be assigned to the lhs
#' @return the modified `spectra` object
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' spec = as_spectra(spec_matrix_example, name_idx = 1)
#' # scale all refletance values by 2
#' value(spec) = value(spec) * 2
`value<-` = function(x, value){
    UseMethod("value<-")
}


#' Get values
#'
#' @describeIn value Get spectra value
#' @export
value.spectra = function(x){
    x$value
}


#' @describeIn value<- Set spectra value
#' @export
`value<-.spectra` = function(x, value){
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
#' spec = as_spectra(spec_matrix_example, name_idx = 1)
#' names(spec)
names.spectra = function(x){
    x$names
}


#' Set spectra sample names
#'
#' \code{names} assigns sample names to lhs
#'
#' @param x spectra object (lhs)
#' @param value values to be assigned (rhs)
#' @return the modified `spectra` object
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' spec = as_spectra(spec_matrix_example, name_idx = 1)
#' names(spec) = toupper(names(spec))
`names<-.spectra` = function(x, value){
    x$names = i_names(value, nrow(x))
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
#' @param min numeric or NULL (default). Keep only bands >= `min`. NULL means no
#'            lower bound.
#' @param max numeric or NULL (default). Keep only bands <= `max`. NULL means no
#'            upper bound.
#' @param return_num boolean. return vector of numeric values (default).
#'                   otherwise, a vector of strings is returned
#' @return vector of bands (possibly empty). numeric if `return_num` = TRUE
#'         (default), character otherwise.
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' spec = as_spectra(spec_matrix_example, name_idx = 1)
#' head(bands(spec))
bands = function(x, min = NULL, max = NULL, return_num = TRUE){
    UseMethod("bands")
}


#' Set band labels
#'
#' \code{bands} sets band labels of lhs to the rhs values
#'
#' @param x spectra object (lhs)
#' @param value rhs
#' @return the modified `spectra` object
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' spec = as_spectra(spec_matrix_example, name_idx = 1)
#' bands(spec) = bands(spec) / 1000
`bands<-` = function(x, value){
    UseMethod("bands<-")
}


#' Get band labels
#'
#' @describeIn bands Get spectra band labels
#' @export
bands.spectra = function(x, min = NULL, max = NULL, return_num = TRUE) {

    wl = x$bands   # numeric by construction (i_bands / validate_spectra)

    ## Empty band set (e.g. a 0-band selection): return empty rather than choking
    ## on min(numeric(0)); keeps dim()/ncol() usable on such objects.
    if(length(wl) == 0){
        return(if(return_num){ wl } else { character(0) })
    }

    lo   = if(is.null(min)){ min(wl) } else { as.numeric(min) }
    hi   = if(is.null(max)){ max(wl) } else { as.numeric(max) }
    pick = wl >= lo & wl <= hi

    if(!return_num){
        wl = as.character(wl)
    }

    ## A range that matches nothing yields an empty vector (a normal getter
    ## result), not an error.
    wl[pick]
}

#' @describeIn bands<- Set spectra band labels
#' @export
`bands<-.spectra` = function(x, value){
    x$bands = i_bands(value, ncol(x))
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
#' @param quiet boolean. If TRUE (default), a request for a non-existent metadata
#'        column emits a warning and returns the columns that did match; if FALSE
#'        it is a hard error. Either way it never silently returns NULL.
#' @return data frame or vector
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' spec = as_spectra(spec_matrix_example, name_idx = 1)
#' spec = normalize(spec)
#' meta(spec, "normalization_magnitude")
meta = function(x, label = NULL, sample = NULL, simplify = FALSE, quiet = TRUE){
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
#' @return the modified `spectra` object
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' spec = as_spectra(spec_matrix_example, name_idx = 1)
#' meta(spec, "random") = rnorm(nrow(spec), mean(10), sd = 2)
`meta<-` = function(x, label = NULL, sample = NULL, value){
    UseMethod("meta<-")
}

#' Get metadata
#'
#' @describeIn meta get metadata
#' @export
meta.spectra = function(x, label = NULL, sample = NULL, simplify = FALSE, quiet = TRUE){

    m = i_match_label_or_idx(names(x), i = sample)

    if( is.null(label) ){
        cols = seq_len(ncol(x$meta))
    } else if ( is.numeric(label) ){
        ## Numeric label: resolve as a column index (documented feature).
        cols = i_match_label_or_idx(colnames(x$meta), label, allow_empty_lookup = TRUE)
    } else {
        ## Character label: match by name. Use i_match_label (not the label-or-idx
        ## resolver) so an unknown label is reported here instead of throwing a
        ## generic "No match.", and so the columns that DID match are still
        ## returned. A missing label is never a silent NULL: `quiet = TRUE`
        ## (default) warns and returns the matched columns; FALSE is a hard error.
        lm   = i_match_label(colnames(x$meta), label, full = TRUE, allow_empty_lookup = TRUE)
        cols = lm[["matched"]]

        if( length(lm[["not_element"]]) != 0 ){
            msg = paste0("metadata column(s) not found: ",
                         paste(lm[["not_element"]], collapse = ", "))
            if(quiet){ warning(msg, call. = FALSE) } else { stop(msg, call. = FALSE) }
        }
    }

    x$meta[ m, cols, drop = simplify]
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
