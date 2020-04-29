#' Spectra Transpose
#'
#' spectra are not transposable. Transpose the value instead
#'
#' @param x spectra
#' @return nothing. operation not allowed
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' s = as.spectra(spec_matrix_example, name_idx = 1)
#'
#' # This will throw an error
#' \dontrun{
#' t(s)
#' }
#' # But these options should work
#' t(value(s))
#' t(as.matrix(s))
t.spectra = function(x){
    stop("spectra are not transposable. You can `t(value(x))` though.")
}

#' Arithmetic operators for spectra
#'
#' Overloads arithmetic operators for spectra using `Ops.`
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
#' spec  = as.spectra(spec_matrix_example, name_idx = 1)
#' spec1 = spec * 2
#' spec2 = spec + spec
#' all(spec1 == spec2)
Ops.spectra = function(e1, e2) {
    mathop = c("+", "-", "*", "/", "^")
    boolop = c("==", "!=", "<", "<=", ">=", ">")

    if( ! .Generic %in% c(mathop, boolop) ){ stop("Not implemented") }

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
        if( any(wavelengths(e1) != wavelengths(e2)) ) {
            stop("wavelength labels must be identical")
        }
        if(.Generic %in% mathop){
            e1[] = do.call(.Generic, list(value(e1), value(e2)) )
            if(any(names(e1) != names(e2))){
                warning("sample names not identical: removing sample names...")
                names(e1) = rep(NA, dim(e1)["n_samples"])
            }
            return(e1)
        }
        if(.Generic %in% boolop){
            return(do.call(.Generic, list(value(e1), value(e2)) ))
        }
    }
}

################################################################################
# Matrix multiplication operator is BROKEN
# Dudu -- 2016-11-19
#
# Because S3 methods dispatch on the first argument, spec %*% mat works but
# mat %*% spec doesn't.
# I am also not so sure how to properly implement this overload because `%*%`
# is a primitive without S3 generic. The current implementation draws from
# http://stackoverflow.com/questions/40580149/overload-matrix-multiplication-for-s3-class-in-r
#
# This feature will be put on hold until I figure this out
################################################################################

# #' Matrix multiplication
# #' @export
# `%*%.default` = .Primitive("%*%")
#
# #' S3 matrix multiplication method
# #'
# #' Defines a generic martix multiplication method
# #'
# #' @param x input
# #' @param ... additional args to matrix multiplication
# #' @export
# `%*%` = function(x, ...){
#     UseMethod("%*%", x)
# }
#
# #' spectra matrix multiplication
# #'
# #' Defines matrix multiplication for spectra
# #'
# #' @param x lhs
# #' @param y rhs
# #'
# #' @return matrix product
# #' @export
# `%*%.spectra` = function(x, y){
#     if( is_spectra(x)){ x = as.matrix(x) }
#     if( is_spectra(y)){ y = as.matrix(y) }
#
#     # The as.matrix() may keep some dimname info in the result matrix
#     # in contrast to value()
#     # Also, benchmark and decide.
#
#     # x = if(is_spectra(x)) value(x)
#     # y = if( is_spectra(y)) value(y)
#
#     x %*% y
#
# }
