##############################################
###### Preprocessing SVC data  ###############
#############################################
###### 10-20-2016 ##########################

#' Preprocessing
#'
#' Function to read .sig files and combine them in a data.frame
#'
#' @param x A data frame containing reflectance values.
#' @return The output is a dataframe.
#' @name preprocess
NULL


#### exclude spectra with NIR shoulder <0.30, >0.65 @761 nm
#' @rdname preprocess
#' @export
exclhilo_spec <- function(x){
  sub_spec <- subset(x,x$`761`>0.30 & x$`761`<0.65)

}

### bad measurements high in vis
#' @rdname preprocess
#' @export
exclhivis_spec <- function(x){
  sub_spec <- subset(x,x$`450`< 0.2)
}

#' @rdname preprocess
#' @export
exclhistart_spec <- function(x){
  sub_spec <- subset(x,x$`400`< 0.15)
}


### exclude spectra with bad start ###
#' @rdname preprocess
#' @export
exclbadstart_spec <- function (x) {
  sub_spec <- subset(x, abs(x$`400` - x$`420`) < 0.01)
}

### exclude dips in NIR ###
#' @rdname preprocess
#' @export
excldipnir_spec <- function (x) {
  sub_spec <- subset(x, x$`800` - x$`770` < 0.02)
}

### exclude smalldips and bumps in NIR ###
#' @rdname preprocess
#' @export
exclminidipnir_spec <- function (x) {
  sub_spec <- subset(x, abs(x$`780` - x$`760`) < 0.013)
}

### exclude spectra shifted up ###
#' @rdname preprocess
#' @export
exclups_spec <- function (x) {
  sub_spec <- subset(x, x$`670` < 0.15)
}


### exclude dips in SWIR  ###
#' @rdname preprocess
#' @export
excldipswir_spec <- function (x) {
  sub_spec <- subset(x, x$`1685` - x$`1660` < 0.0002)
}


### exclude high red ###
#' @rdname preprocess
#' @export
exclhired_spec <- function (x) {
  sub_spec <- subset(x, x$`650` - x$`550` < 0)
}

### exclude spectra with max in red not in green
#' @rdname preprocess
#' @export
exclmaxvisshift_spec <- function(x){
  sub_spec <- subset(x,x$`610` - x$`550` < 0)
}


### exclude drop at the end of NIR ###
#' @rdname preprocess
#' @export
excldropnir_spec <- function (x) {
  sub_spec <- subset(x, x$`1250` - x$`1300` < 0.03)
}

### exclude jump between VIS NIR sensor ###
#' @rdname preprocess
#' @export
exclvisnirjump_spec <- function (x) {
  sub_spec <- subset(x, abs(x$`1100` - x$`900`) < 0.025)
}


