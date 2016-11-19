################################################################################
# Internal constructor helper for each spectra component
################################################################################

#' Internal constructor for reflectance matrix
#'
#' \code{i_reflectance} constructs reflectance matrix in the appropriate format
#'
#' Coerces input form different formats into data conformable to reflectance,
#' which is a numeric matrix with no dimension names.
#'
#' @param x numeric matrix, dataframe or vector (in case of single spectrum)
#' @param nwavelengths Integer of expected number of wavelengths.
#'                     If NULL (default) checking is skipped.
#' @param nsample Integer of expected number of samples.
#'                If NULL (default) checking is skipped.
#' @param enforce01 Boolean. enforce reflectance to be between 0.0 and 1.0?
#'                  Defaults to NULL, which ends up defaulting to FALSE internally
#' @return data conformable to relative reflectance: numeric matrix
i_reflectance = function(x, nwavelengths = NULL, nsample = NULL, enforce01 = NULL) {

    # Default enforce01 when param enforce01 == NULL
    if(is.null(enforce01)) {
        enforce01 = FALSE
    }

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

    ## add enforce01 attribute
    attr(x, which = "enforce01") = enforce01

    ## Return
    x
}


#' Internal constructor for sample names
#'
#' \code{i_names} constructs a sample name vector in the appropriate format
#'
#' @param x vector of labels. numeric or character
#' @param nsample Integer of expected number of samples.
#'                If NULL (default) checking is skipped.
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


#' Internal wavelength constructor for spectra
#'
#' \code{i_wavelengths} constructs wavelength labels in the appropriate format
#'
#' @param x vector of wavelengths. Either numeric or character
#' @param nwavelengths Integer of expected number of wavelengths.
#'                     If NULL (default) checking is skipped.
#'
#' @return vector of wavelengths
i_wavelengths = function(x, nwavelengths = NULL) {
    if(! is.vector(x)) {
        stop("Wavelengths names must be in a vector")
    }

    if( !is.null(nwavelengths) && nwavelengths != length(x) ){
        stop("The length of x must be the same as nwavelengths")
    }

    x = suppressWarnings(as.numeric(x))
    n = is.na(x)
    if( any(n) ){
        stop("Wavelength cannot be converted to numeric: ", x[n])
    }

    x
}

#' Internal metadata constructor for spectra
#'
#' \code{i_meta} constructs a metadata data.frame in the appropriate format
#'
#' @param x data.frame
#' @param nsample number of samples in spectra
#' @param ... additional arguments passed to as.data.frame
#'
#' @return data.frame
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

#' Spectra object constructor
#'
#' \code{spectra} "manually" creates a spectra object
#'
#' @param reflectance N by M numeric matrix. N samples in rows and M wavelengs
#'                    in columns
#' @param wavelengths wavelength names in vector of length M
#' @param names sample names in vector of length N
#' @param meta spectra metadata. defaults to NULL. Must be either of length or nrow
#'             equals to the number of samples (nrow(reflectance) or length(names))
#' @param enforce01 Force reflectance to be between 0 and 1. defaults to FALSE
#' @param ... additional arguments to metadata creation. not implemented yet
#'
#' @return spectra object
#' @export
spectra = function(reflectance,
                   wavelengths,
                   names,
                   meta      = NULL,
                   enforce01 = FALSE,
                   ...){

    wl_l  = length(wavelengths)
    spl_l = length(names)

    s = list( reflectance  = i_reflectance(reflectance,
                                           nwavelengths = wl_l,
                                           nsample      = spl_l,
                                           enforce01    = enforce01),
              wavelengths  = i_wavelengths(wavelengths),
              names        = i_names(names))

    if( ! is.null(meta) ){
        s["meta"] = i_meta(meta, nsample = spl_l, ...)
    }

    structure(s, class = c("spectra"))
}
