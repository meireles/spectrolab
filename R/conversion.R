#' Convert matrix or data frame to spectra
#'
#' @param x matrix or dataframe. Samples are in rows and wavelengths in columns.
#'          First column must be the sample label and the remaining columns must
#'          hold reflectance data.
#' @return spectra object
#'
#' @author meireles
#' @export
as.spectra = function(x){
    UseMethod("as.spectra", x)
}

#' Convert matrix to spectra
#'
#' @param x matrix
#' @return spectra object
#'
#' @author meireles
#' @export
as.spectra.matrix = function(x){
    r = x[ , -1 ]
    w = colnames(r)
    s = x[ , 1 ]

    spectra(r, w, s)
}

#' Convert data.frame to spectra
#'
#' @param x data.frame
#' @return spectra object
#'
#' @author meireles
#' @export
as.spectra.data.frame = function(x){
    r = x[ , -1 ]
    w = colnames(r)
    s = x[ , 1 ]

    spectra(r, w, s)
}


#' Convert spectra to matrix
#'
#' @param x spectra object
#' @param fix_names Use make.names to normalize names?
#'                  Pick one: "none" "row" "col" "both".
#' @param ... does nothing
#' @return matrix of spectral reflectance. columns are wavelengths and rows are
#'         samples
#'
#' @author meireles
#' @export
as.matrix.spectra = function(x, fix_names = "none", ...) {
    r = reflectance(x)
    s = names(x)
    w = wavelengths(x)
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

#' Convert spectra to data.frame
#'
#' Returns a data.frame that includes sample names, metadata (if present) and
#' reflectance data. One advantage over as.matrix, is that the metadata are
#' returned.
#'
#' @param x spectra object
#' @param row.names does nothing
#' @param optional does nothing
#' @param fix_names Use make.names to normalize names?
#'                  Pick one: "none" "row" "col" "both".
#' @param ... does nothing
#' @return data.frame with: sample_name, metadata (if any) and reflectance.
#'
#' @author meireles
#' @export
as.data.frame.spectra = function(x,
                                 row.names = NULL,
                                 optional = FALSE,
                                 fix_names = "none",
                                 ...) {
    r = reflectance(x)
    s = names(x)
    w = wavelengths(x)
    m = meta(x)
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

    colnames(r) = w
    data.frame(sample_name = s, m, r, check.names = FALSE)
}


