################################################################################
# Internal constructor helper for each spectra component
################################################################################

#' Internal constructor for value matrix
#'
#' \code{i_value} constructs value matrix in the appropriate format
#'
#' Coerces input form different formats into data conformable to value,
#' which is a numeric matrix with no dimension names.
#'
#' @param x numeric matrix, dataframe or vector (in case of single spectrum)
#' @param nbands Integer of expected number of bands.
#'                     If NULL (default) checking is skipped.
#' @param nsample Integer of expected number of samples.
#'                If NULL (default) checking is skipped.
#' @return data conformable to relative value: numeric matrix
#'
#' @keywords internal
#' @author Jose Eduardo Meireles
i_value = function(x, nbands = NULL, nsample = NULL) {

    ## test if x dimensions conform to nbands and nsample
    if(is.vector(x)) {
        x = t(matrix(as.numeric(as.character(x))))
    }

    if(is.matrix(x)){
        mode(x) = "numeric"
    }

    if(is.data.frame(x)) {
        ## Assumes that `as.matrix` converts factors to character
        x = as.matrix(x)
        mode(x) = "numeric"
    }

    ## test if x dimensions conform to nbands and nsample
    if( !is.null(nbands) && nbands != ncol(x) ){
        stop("Number of columns in x must be equal nbands")
    }

    if( !is.null(nsample) && nsample != nrow(x) ){
        stop("Number of rows in x must be equal nsample")
    }

    ## Clean up matrix dimensio names
    dimnames(x) = NULL

    x
}


#' Internal constructor for sample names
#'
#' \code{i_names} constructs a sample name vector in the appropriate format
#'
#' @param x vector of labels. Should be character. If numeric, a prefix will be added
#' @param nsample Integer of expected number of samples.
#'                If NULL (default) checking is skipped.
#' @param prefix String to use as prefix in case an element of x is numeric.
#'               Defaults to "spec_"
#' @return vector of sample names coerced to character
#'
#' @keywords internal
#' @author Jose Eduardo Meireles
i_names = function(x, nsample = NULL, prefix = "spec_"){

    if( ! is.null(dim(x)) ){
        stop("Sample names must be one dimensional")
    }

    x = as.character(unlist(x))

    if( !is.null(nsample) && nsample != length(x) ){
        stop("The length of x must be the same as nsample")
    }

    ## In case x has numeric elements...
    n = which(suppressWarnings(!is.na(as.numeric(x))))

    ## ...prepend them with `prefix`...
    if(length(n) > 0){

        ## while ensuring that `prefix` is a valid char
        if( suppressWarnings(!is.na(as.numeric(prefix))) ||
            is.null(prefix) ||
            is.na(prefix) ){
            prefix = "spec_"
        }

        x[n] = paste(prefix, x[n], sep = "")
    }

    as.character(x)
}


#' Internal band constructor for spectra
#'
#' \code{i_bands} constructs band labels in the appropriate format
#'
#' @param x vector of bands. Either numeric or character
#' @param nbands Integer of expected number of bands.
#'                     If NULL (default) checking is skipped.
#' @param warn_dup_band Warn about duplicated bands?
#' @return vector of bands
#'
#' @importFrom stats runif
#'
#' @keywords internal
#' @author Jose Eduardo Meireles
i_bands = function(x, nbands = NULL, warn_dup_band = FALSE) {
    if(! is.vector(x)) {
        stop("bands names must be in a vector")
    }

    if( !is.null(nbands) && nbands != length(x) ){
        stop("The length of x must be the same as nbands")
    }

    y = suppressWarnings(as.numeric(x))
    n = is.na(y)
    if( any(n) ){
        stop("band cannot be converted to numeric: ", x[n])
    }

    d = which(duplicated(y))

    if(length(d) > 0){

        position = d
        original = y[d]

        # Need to add a tiny percent (between 0.001% and 0.0012%) of smallest band diff
        # to the duplicated bands
        # Sort ensures that dups that show up later (order-wise) will have larger values
        # when a band has more than one duplicate
        scalars  = sort(runif(length(d), min = 0.00001, max = 0.00012))

        y[d] = y[d] + scalars * min(abs(diff(y[-d])))

        updated = y[d]

        if(warn_dup_band){
            cat("Duplicated band values are not allowed!\n")
            cat("Bands updated as follows:\n")
            print(data.frame("band_position"  = position,
                             "original_value" = original,
                             "updated_value"  = format(updated, digits = 12),
                             check.names = FALSE),
                  row.names = FALSE)
        }
    }

    y
}

#' Internal metadata constructor for spectra
#'
#' \code{i_meta} constructs a metadata data.frame in the appropriate format
#'
#' @param x data.frame
#' @param nsample number of samples in spectra
#' @param allow_null boolean. If TRUE (default) and x is NULL, the function will
#'                   return NULL regardless of nsample
#' @param ... additional arguments passed to as.data.frame
#' @return data.frame
#'
#' @keywords internal
#' @author Jose Eduardo Meireles
i_meta = function(x, nsample, allow_null = TRUE, ...){

    if(is.null(x) && allow_null){
        m = matrix(NA, nrow = nsample, ncol = 0)
        return(as.data.frame(m))
    }

    if( ! is.data.frame(x) ){
        stop("x must be a data.frame")
    }

    if( nsample != nrow(x) ){
        stop("The number of columns of meta must be the same as nsample")
    }

    if(ncol(x) > 0 && is.null(colnames(x))){
        stop("metadata elements (columns) must be named")
    }

    rownames(x) = NULL

    x
}


########################################
# Public constructor interface
########################################

#' Spectra object constructor
#'
#' \code{spectra} "manually" creates a spectra object
#'
#' @param value N by M numeric matrix. N samples in rows and M bands
#'                    in columns
#' @param bands band names in vector of length M
#' @param names sample names in vector of length N
#' @param meta spectra metadata. defaults to NULL. Must be either of length or nrow
#'             equals to the number of samples (nrow(value) or length(names))
#' @param ... additional arguments to metadata creation. not implemented yet
#' @return spectra object
#'
#' @note This function resorts to an ugly hack to deal with metadata assignment.
#'       Need to think a little harder to find a solution.
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' # 1. Create a value matrix.
#' #    In this case, by removing the first column that holds the species name
#' rf = spec_matrix_example[ , -1]
#'
#' # (2) Create a vector with band labels that match
#' #     the value matrix columns.
#' wl = colnames(rf)
#'
#' # (3) Create a vector with sample labels that match
#' #     the value matrix rows.
#' #     In this case, use the first colum of spec_matrix_example
#' sn = spec_matrix_example[ , 1]
#'
#' # Finally, construct the spectra object using the `spectra` constructor
#' spec = spectra(value = rf, bands = wl, names = sn)
spectra = function(value,
                   bands,
                   names,
                   meta      = NULL,
                   ...){

    ## HACK!!! affected blocks marked with ***
    ## The coersion logic for metadata (meta) is in the setter meta() instead of
    ## being in the ctor i_meta.
    ## This means that assigning metadata with `meta()` works in more situations
    ## than using the ctor, e.g.
    ##    meta(s) = list("clade" = c("A", "B", "C", ...))             ## OK
    ##    spectra(..., meta = list("clade" = c("A", "B", "C", ...)))  ## NO GO
    ##
    ## I will resort to an ugly hack to tackle that issue, but this should be
    ## fixed soon.

    wl_l  = length(bands)
    spl_l = length(names)

    s = list( value  = i_value(value,
                               nbands = wl_l,
                               nsample = spl_l),
              bands  = i_bands(bands),
              names  = i_names(names, prefix = NULL),     ## relies of the default prefix inside i_names
              meta   = i_meta(NULL, nsample = spl_l, ...) ## *** Ideally i_meta(meta, nsample = spl_l, ...)
    )

    s = structure(s, class = c("spectra")) ## *** This should be the returned obj
    meta(s) = meta                         ## *** so I shouldn't have to do this
    s
}
