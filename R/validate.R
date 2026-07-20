################################################################################
# Structural validation for spectra objects
################################################################################

#' Report the result of a validation check (internal)
#'
#' @param msg character vector of problem messages (empty if valid)
#' @param stop boolean. Error (TRUE) or warn (FALSE) when problems are found?
#' @return invisibly TRUE if valid, FALSE otherwise
#'
#' @keywords internal
#' @author Jose Eduardo Meireles
i_report_validation = function(msg, stop){
    if(length(msg) == 0){
        return(invisible(TRUE))
    }

    text = paste0("Invalid spectra object:\n- ", paste(msg, collapse = "\n- "))

    if(stop){
        stop(text, call. = FALSE)
    } else {
        warning(text, call. = FALSE)
    }

    invisible(FALSE)
}


#' Validate a spectra object's structural invariants
#'
#' \code{validate_spectra} checks that a \code{spectra} object satisfies the
#' four-slot invariant: \code{value} is a numeric matrix; \code{bands} is a
#' numeric vector whose length matches the number of columns of \code{value};
#' \code{names} is a character vector whose length matches the number of rows;
#' and \code{meta} is a \code{data.frame} with one row per sample. Band values
#' are NOT required to be strictly increasing, because raw multi-sensor data
#' legitimately carries duplicate/overlapping wavelengths before
#' \code{\link{match_sensors}} is run.
#'
#' The constructor \code{\link{spectra}} calls this automatically when
#' \code{options(spectrolab.debug = TRUE)}. Call it directly to check an object
#' that was built by hand or mutated through the setters.
#'
#' @param x a spectra object
#' @param stop boolean. If TRUE (default), an invalid object triggers an error;
#'             otherwise a warning is emitted and the function returns FALSE.
#' @return invisibly TRUE when \code{x} is valid; otherwise it errors (default)
#'         or, when \code{stop = FALSE}, warns and returns FALSE.
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' spec = as_spectra(spec_matrix_example, name_idx = 1)
#' validate_spectra(spec)
validate_spectra = function(x, stop = TRUE){

    msg = character(0)

    if( ! is_spectra(x) ){
        msg = c(msg, "object does not have class 'spectra'")
    }

    ## Check the skeleton before reaching into slots
    slots = names(unclass(x))
    if( ! is.list(x) || ! all(c("value", "bands", "names", "meta") %in% slots) ){
        msg = c(msg, "spectra must be a list with value, bands, names and meta slots")
        return(i_report_validation(msg, stop))
    }

    v = x$value
    b = x$bands
    n = x$names
    m = x$meta

    if( ! is.matrix(v) || ! is.numeric(v) ){
        msg = c(msg, "value must be a numeric matrix")
    }

    if( ! is.numeric(b) ){
        msg = c(msg, "bands must be numeric")
    }

    if( ! is.character(n) ){
        msg = c(msg, "names must be character")
    }

    if( is.matrix(v) ){
        if( nrow(v) != length(n) ){
            msg = c(msg, "nrow(value) must equal length(names)")
        }
        if( ncol(v) != length(b) ){
            msg = c(msg, "ncol(value) must equal length(bands)")
        }
    }

    if( ! is.data.frame(m) ){
        msg = c(msg, "meta must be a data.frame")
    } else if( is.matrix(v) && nrow(m) != nrow(v) ){
        msg = c(msg, "nrow(meta) must equal nrow(value)")
    }

    i_report_validation(msg, stop)
}
