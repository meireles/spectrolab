#' Get internal indexes for spectra attributes
#'
#' \code{i_match_ij_spectra} gets position matching if the relfectance matrix
#'
#' @param this spectra
#' @param i sample names or indices
#' @param j wavelengths, not indices
#'
#' @return list if row indices and colimn indices
i_match_ij_spectra = function(this, i = NULL, j = NULL){
    ## subset by samples i.e. rows
    if(is.null(i)){
        r_match = seq(nrow(this$reflectance))
    } else {
        r_match    = which(this$names %in% i)
        r_no_match = setdiff(i, this$names)

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
        c_match = seq(ncol(this$reflectance))
    } else {
        c_match = match(j, this$wavelengths)
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
#' @param j Wavelength labels, as numeric or characeter. Do not use indexes.
#'
#' @return spectra object
#' @export
`[.spectra` = function(this, i, j){

    if(missing(i)){ i = NULL }
    if(missing(j)){ j = NULL }

    m = i_match_ij_spectra(this = this, i = i, j = j)

    ## subset. drop = false is needed to return a matrix instead of vec when nsample = 1
    this$reflectance  = this$reflectance[ m[["r_idx"]] , m[["c_idx"]], drop = FALSE ]
    this$wavelengths  = this$wavelengths[ m[["c_idx"]] ]
    this$names        = this$names[ m[["r_idx"]] ]

    this
}


#' Assign reflectance vlaues to spectra
#'
#' \code{`[<-`} assigns the rhs values to spectra
#'
#' @param this spectra object (lhs)
#' @param i sample name
#' @param j wavelength
#' @param value value to be asigned (rhs)
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
#'
#' @return matrix with samples in rows and wavelengths in columns
#' @export
reflectance = function(x){
    UseMethod("reflectance")
}

#' Set spectra reflectance
#'
#' \code{reflectance} NOT IMPLEMENTED. Use the \code{`[<-`} notation instead.
#'
#' @param x spectra object
#' @param value value to be assigned to the lhs
#'
#' @return nothing. deleted function
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
    stop("reflectance() does not allow assignment.
         Please use the x[] <- fuction instead")
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
#' @param return_num boolean. return vector of numeric values (default).
#'                   otherwise, a vector of strings is returned
#' @return vector of wavelengths. numeric if `return_num` = TRUE (default).
#' @export
wavelengths = function(x, return_num = TRUE){
    UseMethod("wavelengths")
}


#' Set wavelength labels
#'
#' \code{wavelengths} sets wavelength labels of lhs to the rhs values
#'
#' @param x spectra object (lhs)
#' @param value rhs
#'
#' @return nothing. called for its side effect.
#' @export
`wavelengths<-` = function(x, value){
    UseMethod("wavelengths<-")
}


#' @describeIn wavelengths Set spectra wavelength labels
#' @export
wavelengths.spectra = function(x, return_num = TRUE) {
    if(return_num){
        return( as.numeric(x$wavelengths) )
    } else {
        return( as.character(x$wavelengths) )
    }
}


#' @describeIn wavelengths<- Set spectra wavelength labels
#' @export
`wavelengths<-.spectra` = function(x, value){

    ## Assign new wavelength values constructed using the internal constructor.
    ## This should:
    ##  (1) check for all requirements of wavelengths, including length (i.e. ncol(x) )
    ##  (2) throw if requirements are not met.
    x$wavelengths = i_wavelengths(value, ncol(x))

    ## return
    x
}
