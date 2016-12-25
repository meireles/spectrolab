##############################################
###### Preprocessing SVC data  ###############
#############################################
###### 12-24-2016 ##########################

#' Preprocess spectra
#'
#' Functions to clean up spectral datasets.
#'
#' @param spec spectra object
#' @param refl_high reflectance upper limit
#' @param refl_low reflectance lower limit
#' @param refl_diff maximum reflectance difference
#' @param wvl_high wavelength upper limit
#' @param wvl_low wavelength lower limit
#'
#' @return spectra object
#' @details The summary function \code{excl_shoulder} uses the reflectance value
#' at the NIR shoulder (780 nm) and excludes spectra below \code{refl_low} and
#' above \code{refl_high}.
#'
#' The functions \code{excl_high} and
#' \code{refl_high} exclude spectra with reflectance values above or below the
#' defined limits \code{excl_high} and \code{excl_low} at user defined
#' wavelengths \code{wvl_high} and \code{wvl_low}, respectively. \code{excl_dip}
#' exclude spectra if the difference between \code{wvl_high} and \code{wvl_low}
#' exceeds \code{refl_diff}.
#'
#' @name preprocess
NULL


#### exclude spectra above and below a threshold @ 780 nm NIR shoulder
#' @rdname preprocess
#' @examples
#' \dontrun{
#' excl_shoulder(acer_spectra, refl_high=0.7, refl_low=0.3)
#' }
#'
#' @export
excl_shoulder <- function(spec, refl_high, refl_low){
    mat_spec <- as.matrix(spec)
    sub_spec <- subset (mat_spec, mat_spec[,colnames(mat_spec)=="780"]<refl_high)
    sub_spec <- subset (sub_spec, sub_spec[,colnames(mat_spec)=="780"]>refl_low)
    sub_spec <- cbind(row.names(sub_spec), sub_spec)
    row.names(mat_spec)<- NULL
    sub_spec <- as.spectra(sub_spec)
    return(sub_spec)
}

#### exclude spectra with high reflectance at defined wavelenght
#' @rdname preprocess
#' @export

excl_high <- function(spec, refl_high, wvl_high){
    mat_spec <- as.matrix(spec)
    sub_spec <- subset (mat_spec, mat_spec[,colnames(mat_spec)==wvl_high]<refl_high)
    sub_spec <- cbind(row.names(sub_spec), sub_spec)
    row.names(mat_spec)<- NULL
    sub_spec <- as.spectra(sub_spec)
    return(sub_spec)
}


#### exclude spectra with low reflectance at defined wavelenght
#' @rdname preprocess
#' @examples
#' \dontrun{
#' excl_low(acer_spectra, refl_low=0.01, wvl_low=401)
#' }
#'
#' @export

excl_low <- function(spec, refl_low, wvl_low){
    mat_spec <- as.matrix(spec)
    sub_spec <- subset (mat_spec, mat_spec[,colnames(mat_spec)==wvl_low]>refl_low)
    sub_spec <- cbind(row.names(sub_spec), sub_spec)
    row.names(mat_spec)<- NULL
    sub_spec <- as.spectra(sub_spec)
    return(sub_spec)
}


#### exclude dips ###
#' @rdname preprocess
#' @examples
#' \dontrun{
#' excl_low(acer_spectra, wvl_low=770, wvl_high=800, refl_diff=0.02)
#' }
#'
#' @export

excl_dip <- function(spec, wvl_low, wvl_high, refl_diff){
    mat_spec <- as.matrix(spec)
    sub_spec <- subset (mat_spec,
                        mat_spec[,colnames(mat_spec)==wvl_high] -
                            mat_spec[,colnames(mat_spec)==wvl_low] < refl_diff)
    sub_spec <- cbind(row.names(sub_spec), sub_spec)
    row.names(mat_spec)<- NULL
    sub_spec <- as.spectra(sub_spec)
    return(sub_spec)
}



# ### exclude dips in NIR ###
# #' @rdname preprocess
# #' @export
# excldipnir_spec <- function (x) {
#   sub_spec <- subset(x, x$`800` - x$`770` < 0.02)
# }
#
# ### exclude smalldips and bumps in NIR ###
# #' @rdname preprocess
# #' @export
# exclminidipnir_spec <- function (x) {
#   sub_spec <- subset(x, abs(x$`780` - x$`760`) < 0.013)
# }
#
# ### exclude spectra shifted up ###
# #' @rdname preprocess
# #' @export
# exclups_spec <- function (x) {
#   sub_spec <- subset(x, x$`670` < 0.15)
# }
#
#
# ### exclude dips in SWIR  ###
# #' @rdname preprocess
# #' @export
# excldipswir_spec <- function (x) {
#   sub_spec <- subset(x, x$`1685` - x$`1660` < 0.0002)
# }
#
#
# ### exclude high red ###
# #' @rdname preprocess
# #' @export
# exclhired_spec <- function (x) {
#   sub_spec <- subset(x, x$`650` - x$`550` < 0)
# }
#
# ### exclude spectra with max in red not in green
# #' @rdname preprocess
# #' @export
# exclmaxvisshift_spec <- function(x){
#   sub_spec <- subset(x,x$`610` - x$`550` < 0)
# }
#
#
# ### exclude drop at the end of NIR ###
# #' @rdname preprocess
# #' @export
# excldropnir_spec <- function (x) {
#   sub_spec <- subset(x, x$`1250` - x$`1300` < 0.03)
# }
#
# ### exclude jump between VIS NIR sensor ###
# #' @rdname preprocess
# #' @export
# exclvisnirjump_spec <- function (x) {
#   sub_spec <- subset(x, abs(x$`1100` - x$`900`) < 0.025)
# }



# #' Preprocessing
# #'
# #' Function to read .sig files and combine them in a data.frame
# #'
# #' @param x A data frame containing reflectance values.
# #' @return The output is a dataframe.
# #' @name preprocess
# NULL
#
#
# #### exclude spectra with NIR shoulder <0.30, >0.65 @761 nm
# #' @rdname preprocess
# #' @export
# exclhilo_spec <- function(x){
#   sub_spec <- subset(x,x$`761`>0.30 & x$`761`<0.65)
#
# }
#
# ### bad measurements high in vis
# #' @rdname preprocess
# #' @export
# exclhivis_spec <- function(x){
#   sub_spec <- subset(x,x$`450`< 0.2)
# }
#
# #' @rdname preprocess
# #' @export
# exclhistart_spec <- function(x){
#   sub_spec <- subset(x,x$`400`< 0.15)
# }
#
#
# ### exclude spectra with bad start ###
# #' @rdname preprocess
# #' @export
# exclbadstart_spec <- function (x) {
#   sub_spec <- subset(x, abs(x$`400` - x$`420`) < 0.01)
# }
#
# ### exclude dips in NIR ###
# #' @rdname preprocess
# #' @export
# excldipnir_spec <- function (x) {
#   sub_spec <- subset(x, x$`800` - x$`770` < 0.02)
# }
#
# ### exclude smalldips and bumps in NIR ###
# #' @rdname preprocess
# #' @export
# exclminidipnir_spec <- function (x) {
#   sub_spec <- subset(x, abs(x$`780` - x$`760`) < 0.013)
# }
#
# ### exclude spectra shifted up ###
# #' @rdname preprocess
# #' @export
# exclups_spec <- function (x) {
#   sub_spec <- subset(x, x$`670` < 0.15)
# }
#
#
# ### exclude dips in SWIR  ###
# #' @rdname preprocess
# #' @export
# excldipswir_spec <- function (x) {
#   sub_spec <- subset(x, x$`1685` - x$`1660` < 0.0002)
# }
#
#
# ### exclude high red ###
# #' @rdname preprocess
# #' @export
# exclhired_spec <- function (x) {
#   sub_spec <- subset(x, x$`650` - x$`550` < 0)
# }
#
# ### exclude spectra with max in red not in green
# #' @rdname preprocess
# #' @export
# exclmaxvisshift_spec <- function(x){
#   sub_spec <- subset(x,x$`610` - x$`550` < 0)
# }
#
#
# ### exclude drop at the end of NIR ###
# #' @rdname preprocess
# #' @export
# excldropnir_spec <- function (x) {
#   sub_spec <- subset(x, x$`1250` - x$`1300` < 0.03)
# }
#
# ### exclude jump between VIS NIR sensor ###
# #' @rdname preprocess
# #' @export
# exclvisnirjump_spec <- function (x) {
#   sub_spec <- subset(x, abs(x$`1100` - x$`900`) < 0.025)
# }
