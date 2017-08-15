################################################################################
# Common stats functions: mean, median, var, sd and coefvar
################################################################################

#' Mean spectrum
#'
#' @param x spectra
#' @param na.rm boolean. remove NAs?
#' @param keep_txt_meta try to keep text in the metadata
#' @param ... nothing
#' @return single spectrum
#'
#' @author Jose Eduardo Meireles
#' @export
mean.spectra = function(x, na.rm = TRUE, keep_txt_meta = TRUE, ...){
    r = base::colMeans(as.matrix(x), na.rm = na.rm)
    w = wavelengths(x)
    n = "mean"
    e = enforce01(x)
    m = meta(x)

    if(ncol(m) != 0){
        m = apply(m, 2,
                  ifelse(keep_txt_meta, try_keep_txt(base::mean), base::mean),
                  na.rm = na.rm)
    }

    spectra(r, w, n, m, e)
}

#' Median spectrum
#'
#' @param x spectra
#' @param na.rm boolean. remove NAs?
#' @param ... nothing
#' @return single spectrum
#'
#' @importFrom stats median
#'
#' @author Jose Eduardo Meireles
#' @export
median.spectra = function(x, na.rm = TRUE, ...){
    r = apply(as.matrix(x), 2, stats::median, na.rm = na.rm)
    w = wavelengths(x)
    n = "mean"
    e = enforce01(x)
    m = meta(x)

    if(ncol(m) != 0){
        m = apply(m, 2, mean, na.rm = na.rm)
    }

    spectra(r, w, n, m, e)
}


#' Variance
#'
#' @param x a numeric vector, matrix or data frame
#' @param y NULL (default) or a vector, matrix or data frame with compatible
#'          dimensions to x.
#' @param na.rm logical. Should missing values be removed?
#' @param use an optional character string giving a method for computing covariances in
#'            the presence of missing values. This must be (an abbreviation of) one of
#'            the strings "everything", "all.obs", "complete.obs", "na.or.complete",
#'            or "pairwise.complete.obs"
#' @return variance
#' @export
var = function(x, y = NULL, na.rm = FALSE, use){
    UseMethod("var")
}

#' Default variance
#'
#' @inherit stats::var
#' @importFrom stats var
#' @export
var.default = stats::var


#' Variance spectrum
#'
#' @param x spectra
#' @param y nothing
#' @param na.rm boolean. remove NAs?
#' @param use nothing
#' @return single spectrum
#'
#' @importFrom stats var
#'
#' @author Jose Eduardo Meireles
#' @export
var.spectra = function(x, y = NULL, na.rm = TRUE, use){
    s_name = "var"
    aggregate(x = x, by = rep(s_name, nrow(x)), FUN = stats::var, na.rm = na.rm)
}


#' Standard deviation
#'
#' @param x a numeric vector or an R object which is coercible to one by as.double(x)
#' @param na.rm logical. Should missing values be removed?
#' @return standard deviation
#'
#' @export
sd = function(x, na.rm = FALSE){
    UseMethod("sd")
}

#' Default variance
#'
#' @inherit stats::sd
#' @importFrom stats sd
#' @export
sd.default = stats::sd


#' Standard deviation spectrum
#'
#' @param x spectra
#' @param na.rm boolean. remove NAs?
#' @return single spectrum
#'
#' @importFrom stats sd
#'
#' @author Jose Eduardo Meireles
#' @export
sd.spectra = function(x, na.rm = TRUE){
    s_name = "sd"
    aggregate(x = x, by = rep(s_name, nrow(x)), FUN = stats::sd, na.rm = na.rm)
}
