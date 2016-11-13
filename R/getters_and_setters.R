#' Subsets a spectra object
#'
#' Subsets spectra objects by sample names (rows) or wavelengths (columns).
#'
#' Subset operations based on samples (first argument) will match either sample
#' names or indexes, in that order. That is, if you subset x[1:2 , ] and your
#' sample names contain 1 and 2, you will get the spectra with names %in%
#' c(1, 2) and not (at least necessarily) the first and second samples in the
#' `spectra` object.
#'
#' @param i Sample names (preffered) or index.
#' @param j Wavelength labels, as numeric or characeter. Do not use indexes.
#'
#' @return spectra object
#' @export
`[.spectra` = function(this, i, j, verbose = FALSE){

    ## subset by samples i.e. rows
    if(missing(i)){
        r_match = seq(nrow(this$reflectance))
    } else {
        r_match    = which(this$names %in% i)
        r_no_match = setdiff(i, this$names)

        if( length(r_no_match) != 0 || length(r_no_match) == length(i) ){
            if( i_is_index(i, dim(this)[1])) {
                r_match = as.integer(i)
            } else {
                stop("Sample subscript out of bounds: \n", r_no_match)
            }
        }
    }

    ## subset by wavelength. i.e. columns
    if(missing(j)){
        c_match = seq(ncol(this$reflectance))
    } else {
        c_match = match(j, this$wavelengths)
        if(any(is.na(c_match))){
            stop("Wavelength subscript out of bounds. Use wavelength labels instead of raw indices.")
        }
    }

    ## subset. drop = false is needed to return a matrix instead of vec
    ## when nsample = 1
    this$reflectance  = this$reflectance[ r_match , c_match, drop = FALSE ]
    this$wavelengths  = this$wavelengths[ c_match ]
    this$names        = this$names[ r_match ]

    this
}


#' Assign reflectance vlaues to spectra
#'
#' @param i sample name
#' @param j wavelength
#'
#' @return nothing. modifies spectra as side effect
#' @export
`[<-.spectra` = function(this, i, j, value){
    this$reflectance[i, j] = i_reflectance(value)
    this
}

########################################
# reflectance
########################################

#' Get the reflectance from spectra
#'
#' @param spec spectra object
#'
#' @return matrix with samples in rows and wavelengths in columns
#' @export
reflectance = function(spec){
    if( !is_spectra(spec) ){
        stop("Object must be of class spectra")
    }
    spec$reflectance
}


#' Set reflectance in spectra
#'
#' @param spec Spectra object
#'
#' @return nothing. deleted function
#' @export
`reflectance<-` = function(spec, value){
    if( !is_spectra(spec) ){
        stop("Object must be of class spectra")
    }

    stop("reflectance() does not allow assignment.
         Please use the x[] <- fuction instead")
}



########################################
# sample names
########################################

#' Get sample names from spectra
#'
#' @param spec A spectra object
#'
#' @return vector of sample names
#' @export
names.spectra = function(spec){
    spec$names
}


#' Set sample names in spectra
#'
#' @param spec spectra object to have their sample names modified
#'
#' @return nothing. called for its side effect.
#' @export
`names<-.spectra` = function(spec, value){

    ## Length of samples in spec
    nsampl = length(spec$names)

    ## Make copy of spec
    spec_p = spec

    ## Assign sample names
    spec_p$names = value

    ## Construct sample names using internal constructor. This should:
    ##  (1) check for all requirements of names, including length
    ##  (2) throw if requirements are not met.
    new_name = i_names(spec_p$names, nsampl)

    ## Assign new names to spec object
    spec$names = new_name

    ## Return
    spec
}

########################################
# wavelengths
########################################

#' Get wavelength labels from spectra
#'
#' @param spec spectra object
#' @param return_num boolean. return vector of numeric values (default).
#'                   otherwise, a vector of strings is returned
#' @return vector of wavelengths. numeric if `return_num` = TRUE (default).
#' @export
wavelengths = function(spec, return_num = TRUE){
    if( !is_spectra(spec) ){
        stop("Object must be of class spectra")
    }

    if(return_num){
        return( as.numeric(spec$wavelengths) )
    } else {
        return( as.character(spec$wavelengths) )
    }
}


#' Set sample names in spectra
#'
#' @param spec Spectra object
#'
#' @return
#' @export
`wavelengths<-` = function(spec, value){
    if( !is_spectra(spec) ){
        stop("Object must be of class spectra")
    }

    ## Length of samples in spec
    nwl = length(spec$wavelengths)

    ## Make copy of spec
    spec_p = spec

    ## Assign sample names
    spec_p$wavelengths = value

    ## Construct wavelengths using internal constructor. This should:
    ##  (1) check for all requirements of wavelengths, including length
    ##  (2) throw if requirements are not met.
    new_wl = i_wavelengths(spec_p$wavelengths, nwl)

    ## Assign new wavelength to spec object
    spec$wavelengths = new_wl

    ## return
    spec
}
