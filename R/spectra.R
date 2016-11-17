################################################################################
#' Implementation of s3 class for spectra
#'
################################################################################

## Spectra class structure and requirements
##
## Structure:
##
## -- reflectance: (1) numerical matrix
##                 (2) of dim N samples by M wavelengths
##                 (3) values between 0.0 and 1.0
##
## -- wavelengths: (1) numerical vector
##                 (2) of length M
##
## -- names: (1) character vector
##           (2) of length N

################################################################################
# Internal constructor helper for each spectra component
################################################################################

#' Construct reflectance matrix in the appropriate format
#'
#' Coerces input form different forms into data conformable
#' to reflectance, which is a numeric matrix
#'
#' @param x numeric matrix, dataframe or vector (in case of single spectrum)
#' @param nwavelengths Integer of expected number of wavelengths. If NULL (default) checking is skipped.
#' @param nsample Integer of expected number of samples.  If NULL (default) checking is skipped.
#' @param enforce01 Boolean. enforce reflectance to be between 0.0 and 1.0? Defaults to TRUE
#'
#' @return data conformable to relative reflectance: numeric matrix of
#'         values between 0.0 and 1.0.
#'
i_reflectance = function(x, nwavelengths = NULL, nsample = NULL, enforce01 = TRUE) {

    ## test if x dimensions conform to nwavelengths and nsample
    if(is.vector(x)) {
        x = t(matrix(as.numeric(as.character(x))))
    }

    if(is.matrix(x)){
        mode(x) = "numeric"
    }

    if(is.data.frame(x)) {
        x = as.matrix(x)     ## should convert factors to character
        mode(x) = "numeric"
    }

    ## test if x dimensions conform to nwavelengths and nsample
    if( !is.null(nwavelengths) && nwavelengths != ncol(x) ){
        stop("Number of columns in x must be equal nwavelengths")
    }

    if( !is.null(nsample) && nsample != nrow(x) ){
        stop("Number of rows in x must be equal nsample")
    }

    ## test if all values of x are between 0 and 1
    if(enforce01 && any(any(x < 0.0), any(x > 1.0)) ){
        stop("Reflectance values must be between 0 and 1")
    }

    ## Clean up matrix dimensio names
    dimnames(x) = NULL

    ## Return
    x
}


#' Construct sample names vector in the appropriate format
#'
#' @param x vector of labels. numeric or character
#' @param nsample Integer of expected number of samples. If NULL (default) checking is skipped.
#'
#' @return vector of sample names
i_names = function(x, nsample = NULL){

    if( ! is.null(dim(x)) ){
        stop("Sample names must be one dimensional")
    }

    x = as.character(unlist(x))

    if( !is.null(nsample) && nsample != length(x) ){
        stop("The length of x must be the same as nsample")
    }

    as.character(x)
}


#' Construct wavelength names in the appropriate format
#'
#' @param x vector of wavelengths. numeric or character
#' @param nwavelengths Integer of expected number of wavelengths. If NULL (default) checking is skipped.
#'
#' @return vector of wavelengths
i_wavelengths = function(x, nwavelengths = NULL) {
    if(! is.vector(x)) {
        stop("Wavelengths names must be in a vector")
    }

    if( !is.null(nwavelengths) && nwavelengths != length(x) ){
        stop("The length of x must be the same as nwavelengths")
    }

    if(any(duplicated(x))){
        stop("Wavelengths cannot have duplicated values")
    }

    x = suppressWarnings(as.numeric(x))
    n = is.na(x)
    if( any(n) ){
        stop("Wavelength cannot be converted to numeric: ", x[n])
    }

    x
}

#' Construct a metadata data.frame in the appropriate format
i_meta = function(x, nsample, ...){
    x = as.data.frame(x, ...)

    if( nsample != nrow(x) ){
        stop("The number of columns of meta must be the same as nsample")
    }

    x
}


########################################
# Public constructor interface
########################################

#' Create a spectra object
#'
#' @param reflectance N by M numeric matrix. N samples in rows. values between 0 and 1.
#' @param wavelengths wavelength names in vector of length M
#' @param names sample names in vector of length N
#' @param meta spectra metadata. defaults to NULL. Must be either of length or nrow
#'             equals to the number of samples (i.e. nrow(reflectance) or length(names) )
#'
#' @return spectra object
#' @export
spectra = function(reflectance, wavelengths, names, meta = NULL, ...) {
    wl_l  = length(wavelengths)
    spl_l = length(names)

    s = list( reflectance  = i_reflectance(reflectance,
                                           nwavelengths = wl_l,
                                           nsample = spl_l),
              wavelengths  = i_wavelengths(wavelengths),
              names        = i_names(names))

    if( ! is.null(meta) ){
        s["meta"] = i_meta(meta, nsample = spl_l, ...)
    }

    structure(s, class = c("spectra"))
}
