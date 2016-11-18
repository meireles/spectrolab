#' Operator overloading for spectra
#'
#' @param e1 lhs
#' @param e2 rhs
#'
#' @return operators on spectra
#' @export
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
                e1[] = do.call(.Generic, list( reflectance(e1), e2) )
                return(e1)
            }
            if(.Generic %in% boolop){
                return(do.call(.Generic, list( reflectance(e1), e2) ))
            }
        }
        # spectra is second
        if(w_is_spec == 2) {
            if(.Generic %in% mathop){
                e2[] = do.call(.Generic, list( e1, reflectance(e2)) )
                return(e2)
            }
            if(.Generic %in% boolop){
                return(do.call(.Generic, list( e1, reflectance(e2)) ))
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
            e1[] = do.call(.Generic, list(reflectance(e1), reflectance(e2)) )
            if(any(names(e1) != names(e2))){
                warning("sample names not identical: removing sample names...")
                names(e1) = rep(NA, dim(e1)["n_samples"])
            }
            return(e1)
        }
        if(.Generic %in% boolop){
            return(do.call(.Generic, list(reflectance(e1), reflectance(e2)) ))
        }
    }
}


#' Default matrix multiplication
#' @export
`%*%.default` = .Primitive("%*%")


#' Matrix multiplication S3 method
#' @export
`%*%` = function(x, ...){
    UseMethod("%*%", x)
}

#' Matrix multiplication for spectra class
#'
#' @param e1 lhs
#' @param e2 rhs
#'
#' @return matrix product
#' @export
`%*%.spectra` = function(e1, e2){
    is_spec   = c(is_spectra(e1), is_spectra(e2))
    w_is_spec = which(is_spec)
    s_is_spec = length(w_is_spec) == 1L

    if(s_is_spec) {
        if(w_is_spec == 1) {
            reflectance(e1) %*% e2
        } else {
            e1 %*% reflectance(e2)
        }
    } else {
        reflectance(e1) %*% reflectance(e2)
    }
}
