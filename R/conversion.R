#' Convert matrix or data frame to spectra
#'
#' @param x matrix or dataframe. Samples are in rows and wavelengths in columns.
#'          First column must be the sample label and the remaining columns must
#'          hold reflectance data.
#' @param name_idx column index with sample names. Defaults to 1st col. If NULL
#'                 or 0, rownames(x) or a sequence of integers will be assigned
#'                 as names.
#' @param meta_idxs column indices with metadata (not name and not reflectance).
#'                  Defaults to NULL
#' @return spectra object
#'
#' @author Jose Eduardo Meireles
#' @export
as.spectra = function(x, name_idx = 1, meta_idxs = NULL){
    UseMethod("as.spectra", x)
}

#' Convert data.frame to spectra
#'
#' @param x data.frame
#' @param name_idx column index with sample names. Defaults to 1st col. If NULL
#'                 or 0, rownames(x) or a sequence of integers will be assigned
#'                 as names.
#' @param meta_idxs column indices with metadata (not name and not reflectance).
#'                  Defaults to NULL
#' @return spectra object
#'
#' @author Jose Eduardo Meireles
#' @export
as.spectra.data.frame = function(x, name_idx = 1, meta_idxs = NULL){

    if( is.null(name_idx) || name_idx == 0){
        name_idx = NULL
        s        = rownames(x)
    } else {
        s = x[ , name_idx, drop = TRUE]
    }

    m = x[ , meta_idxs, drop = FALSE]
    p = setdiff(seq(ncol(x)), c(name_idx, meta_idxs))
    r = x[ , p, drop = FALSE]
    w = colnames(r)

    spectra(r, w, s, m)
}


#' Convert matrix to spectra
#'
#' @param x matrix
#' @param name_idx column index with sample names. Defaults to 1st col
#' @param meta_idxs column indices with metadata (not name and not reflectance).
#'                  Defaults to NULL
#' @return spectra object
#'
#' @author Jose Eduardo Meireles
#' @export
as.spectra.matrix = function(x, name_idx = 1, meta_idxs = NULL){
    as.spectra(as.data.frame(x), name_idx = name_idx, meta_idxs = meta_idxs)
}


#' Convert spectra to matrix
#'
#' @param x spectra object
#' @param fix_names Use make.names to normalize names?
#'                  Pick one: "none" "row" "col" "both".
#' @param ... does nothing. Here for compatibility with S3 generics
#' @return matrix of spectral reflectance. columns are wavelengths and rows are
#'         samples
#'
#' @author Jose Eduardo Meireles
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
#' @param row.names does nothing. Here for compatibility with S3 generics
#' @param optional does nothing. Here for compatibility with S3 generics
#' @param fix_names Use make.names to normalize names?
#'                  Pick one: "none" "row" "col" "both".
#' @param ... does nothing. Here for compatibility with S3 generics
#' @return data.frame with: sample_name, metadata (if any) and reflectance.
#'
#' @author Jose Eduardo Meireles
#' @export
as.data.frame.spectra = function(x,
                                 row.names = NULL,
                                 optional = FALSE,
                                 fix_names = "none",
                                 ...) {

    y = as.matrix(x, fix_names = fix_names, ...)
    m = meta(x)

    data.frame(sample_name = rownames(y), m, y, check.names = FALSE, row.names = NULL)
}
