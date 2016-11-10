#' Convert matrix or data frame to spectra
#'
#' @param x matrix or dataframe. See details for format requirements
#'
#' @return spectra object
#' @export
as.spectra = function(x){
    if( is.matrix(x) ){
        return( as.spectra.matrix(x) )
    } else if ( is.data.frame(x) ){
        return( as.spectra.data.frame(x) )
    } else {
        stop("Not implemented. Check ?as.spectra")
    }
}

#' Convert matrix to spectra
#'
#' @param x matrix
#'
#' @return spectra object
as.spectra.matrix = function(x){
    r = x[ , -1 ]
    w = colnames(r)
    s = x[ , 1 ]

    spectra(r, w, s)
}

#' Convert data.frame to spectra
#'
#' @param x data.frame
#'
#' @return spectra object
as.spectra.data.frame = function(x){
    r = x[ , -1 ]
    w = colnames(r)
    s = x[ , 1 ]

    spectra(r, w, s)
}


#' Convert to matrix
#' @export
as.matrix = function(x, ...) {
    UseMethod("as.matrix")
}

#' Convert spectra to matrix
#'
#' @param spec spectra object
#' @param fix_dimnames boolean. if true (default), names are normalized with name.names()
#'
#' @return matrix of spectral reflectance. columns are wavelengths and rows are samples
#' @export
as.matrix.spectra = function(spec, fix_dimnames = TRUE){
    r = reflectance(spec)
    s = sample_names(spec)
    w = wavelengths(spec)

    if(fix_dimnames){
        s = sapply(s, make.names, unique = TRUE)
        w = sapply(w, make.names, unique = TRUE)
    }
    dimnames(r) = list(s, w)
    r
}
