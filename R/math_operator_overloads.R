#' Spectra Transpose
#'
#' spectra are not transposable. Transpose the value instead
#'
#' @param x spectra
#' @return No return value. Operation not allowed
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' s = as_spectra(spec_matrix_example, name_idx = 1)
#'
#' t(value(s))
#' t(as.matrix(s))
t.spectra = function(x){
    stop("spectra are not transposable. You can `t(value(x))` though.")
}

#' Arithmetic operators for spectra
#'
#' Overloads arithmetic operators for spectra using `Ops.`
#'
#' Unary \code{+}/\code{-} (e.g. \code{-spec}) are supported. When both sides
#' of a binary math operator are \code{spectra}, sample names and metadata
#' are kept from the result only when they agree between \code{e1} and
#' \code{e2}; when they disagree, they are cleared (with a warning) rather
#' than one side's values winning silently. The \code{quantity} and
#' \code{wavelength_unit} provenance (see \code{\link{quantity}}) are
#' reconciled the same way.
#'
#' @param e1 lhs
#' @param e2 rhs
#' @return Depends on the operator. math operators will return spectra and logical
#'         or comparison operators will return boolean matrices
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' spec  = as_spectra(spec_matrix_example, name_idx = 1)
#' spec1 = spec * 2
#' spec2 = spec + spec
#' all(spec1 == spec2)
Ops.spectra = function(e1, e2) {
    mathop = c("+", "-", "*", "/", "^")
    boolop = c("==", "!=", "<", "<=", ">=", ">")

    if( ! .Generic %in% c(mathop, boolop) ){ stop("Not implemented") }

    ## Unary +/- (e.g. -spec): e2 is not supplied at all in that case.
    if( missing(e2) ){
        if( ! .Generic %in% c("+", "-") ){
            stop("Unary '", .Generic, "' is not implemented for spectra")
        }
        if( .Generic == "-" ){
            e1[] = -value(e1)
        }
        return(e1)
    }

    is_spec   = c(is_spectra(e1), is_spectra(e2))
    w_is_spec = which(is_spec)
    s_is_spec = length(w_is_spec) == 1L

    # single spectra object
    if( s_is_spec ) {
        # spectra is first
        if( w_is_spec == 1 ){
            if(.Generic %in% mathop){
                e1[] = do.call(.Generic, list( value(e1), e2) )
                return(e1)
            }
            if(.Generic %in% boolop){
                return(do.call(.Generic, list( value(e1), e2) ))
            }
        }
        # spectra is second
        if(w_is_spec == 2) {
            if(.Generic %in% mathop){
                e2[] = do.call(.Generic, list( e1, value(e2)) )
                return(e2)
            }
            if(.Generic %in% boolop){
                return(do.call(.Generic, list( e1, value(e2)) ))
            }
        }
        # both arguments are spectra
    } else {
        if( any(dim(e1) != dim(e2)) ){
            stop("incompatible spectra dimensions")
        }
        if( any(bands(e1) != bands(e2)) ) {
            stop("band labels must be identical")
        }
        if(.Generic %in% mathop){
            e1[] = do.call(.Generic, list(value(e1), value(e2)) )

            if(any(names(e1) != names(e2))){
                warning("sample names not identical: removing sample names...")
                names(e1) = rep(NA, dim(e1)["n_samples"])
            }

            if(!identical(meta(e1), meta(e2))){
                warning("metadata not identical: removing metadata...")
                e1$meta = i_meta(NULL, nrow(e1))
            }

            quantity(e1)        = i_reconcile_provenance_scalar(quantity(e1), quantity(e2), "quantity")
            wavelength_unit(e1) = i_reconcile_provenance_scalar(wavelength_unit(e1), wavelength_unit(e2), "wavelength_unit")

            return(e1)
        }
        if(.Generic %in% boolop){
            return(do.call(.Generic, list(value(e1), value(e2)) ))
        }
    }
}
