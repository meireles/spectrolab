#' Convert matrix or data frame to spectra
#'
#' @param x matrix or dataframe. Samples are in rows and wavelengths in columns.
#'          First column must be the sample label and the remaining columns must
#'          hold reflectance data.
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


# #' Convert to matrix
# #' @export
# as.matrix = function(x, ...) {
#     UseMethod("as.matrix")
# }

#' Convert spectra to matrix
#'
#' @param spec spectra object
#' @param fix_names Use make.names to normalize names?
#'                  Pick one: "none" "row" "col" "both".
#' @return matrix of spectral reflectance. columns are wavelengths and rows are
#'         samples
#' @export
as.matrix.spectra = function(spec, fix_names = "none") {
    r = reflectance(spec)
    s = names(spec)
    w = wavelengths(spec)
    o = c("none", "row", "col", "both")

    if( length(intersect(fix_names, o)) != 1 ){
        stop("fix_names must be one of these options: ", o)
    }

    if(fix_names %in% c("row", "both")){
        s = sapply(s, make.names, unique = TRUE)
    }

    if(fix_names %in% c("col", "both")){
        w = sapply(w, make.names, unique = TRUE)
    }

    dimnames(r) = list(s, w)
    r
}
