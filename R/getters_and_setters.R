#' Subsets a spectra object.
#'
#' Subsets spectra objects by samples (rows) or wavelengths (columns).
#'
#' Subset operations based on samples (first argument) will match either sample
#' names or indexes, in that order. That is, if you subset x[1:2 , ] and your
#' sample names contain 1 and 2, you will get the spectra with sample_names %in%
#' c(1, 2) and not (at least necessarily) the first and second samples in the
#' `spectra` object.
#'
#' @param i Sample names (preffered) or index.
#' @param j Wavelength labels, as numeric or characeter. Do not use indexes.
#'
#' @return
#' @export
`[.spectra` = function(this, i, j, verbose = FALSE){

    ## subset by samples i.e. rows
    if(missing(i)){
        r_match = seq(nrow(this$reflectance))
    } else {
        r_match    = which(this$sample_names %in% i)
        r_no_match = setdiff(i, this$sample_names)

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
    this$sample_names = this$sample_names[ r_match ]

    this
}


#' Assign reflectance vlaues to spectra
#'
#' @param i
#' @param j
#'
#' @return
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
#' @return
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

    stop("Reflectance cannot be assigned to.\nPlease use the spec[] <- fuction instead")
}



########################################
# sample names
########################################

#' Get sample names from spectra
#'
#' @param spec A spectra object
#'
#' @return
#' @export
sample_names = function(spec){
    if( !is_spectra(spec) ){
        stop("Object must be of class spectra")
    }

    spec$sample_names
}


#' Set sample names in spectra
#'
#' @param spec Spectra object
#'
#' @return
#' @export
`sample_names<-` = function(spec, value){
    if( !is_spectra(spec) ){
        stop("Object must be of class spectra")
    }

    ## Length of samples in spec
    nsampl = length(spec$sample_names)

    ## Make copy of spec
    spec_p = spec

    ## Assign sample names
    spec_p$sample_names = value

    ## Construct sample names using internal constructor. This should:
    ##  (1) check for all requirements of sample_names, including length
    ##  (2) throw if requirements are not met.
    new_name = i_sample_names(spec_p$sample_names, nsampl)

    ## Assign new names to spec object
    spec$sample_names = new_name

    ## Return
    spec
}

########################################
# wavelengths
########################################

#' Get wavelength labels from spectra
#'
#' @param spec Spectra object
#'
#' @return
#' @export
wavelengths = function(spec){
    if( !is_spectra(spec) ){
        stop("Object must be of class spectra")
    }
    spec$wavelengths
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
