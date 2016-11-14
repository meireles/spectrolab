#' Operator overloading for spectra
#'
#' @param e1
#' @param e2
#'
#' @return
#' @export
Ops.spectra = function(e1, e2){
    w = c(class(e1), class(e2))
    w = which(w == "spectra")

    if(w == 1){
        e1[] = do.call(.Generic, list( reflectance(e1), e2))
        return(e1)
    } else {
        e2[] = do.call(.Generic, list( reflectance(e2), e1))
        return(e2)
    }
}

#' Matrix multiplication
#'
#' @param e1
#' @param e2
#'
#' @return
#' @export
`%*%.spectra` = function(e1, e2){
    stop("Not implemented")
    # w = c(class(e1), class(e2))
    # w = which(w == "spectra")
    #
    # if(w == 1){
    #     reflectance(e1) %*% e2
    # } else {
    #     reflectance(e2) %*% e1
    # }
}
