################################################################################
# Common stats functions: mean, median, var, sd and quantile
################################################################################

#' Mean spectrum
#'
#' \code{mean} computes the arithmetic mean spectrum.
#'
#' @param x spectra
#' @param na.rm boolean. remove NAs? Defaults to TRUE
#' @param keep_txt_meta try to keep text in the metadata
#' @param ... nothing
#' @return single spectrum
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' spec = as_spectra(spec_matrix_example, name_idx = 1)
#' mean(spec)
mean.spectra = function(x, na.rm = TRUE, keep_txt_meta = TRUE, ...){
    apply_by_band(x, base::mean, na.rm = na.rm, keep_txt_meta = keep_txt_meta, ...)
}

#' Median spectrum
#'
#' \code{median} computes the median spectrum
#'
#' @param x spectra
#' @param na.rm boolean. remove NAs? Defaults to TRUE
#' @param keep_txt_meta try to keep text in the metadata
#' @param ... nothing
#' @return single spectrum
#'
#' @importFrom stats median
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' spec = as_spectra(spec_matrix_example, name_idx = 1)
#' median(spec)
median.spectra = function(x, na.rm = TRUE, keep_txt_meta = TRUE, ...){
    apply_by_band(x, stats::median, na.rm = na.rm, keep_txt_meta = keep_txt_meta, ...)
}


#' Variance
#'
#' \code{var} computes the variance spectrum. Note that values will not reflect
#' value anymore, but the variance of the value instead.
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
#'
#' @export
var = function(x, y = NULL, na.rm = FALSE, use){
    UseMethod("var")
}

#' Variance
#'
#' \code{var} computes the variance spectrum. Note that values will not reflect
#' value anymore, but the variance of the value instead.
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
#'
#' @export
var.default = stats::var

#' Variance spectrum
#'
#' Forces keep_txt_meta = TRUE
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
#'
#' @examples
#' library(spectrolab)
#' spec = as_spectra(spec_matrix_example, name_idx = 1)
#' var(spec)
var.spectra = function(x, y = NULL, na.rm = TRUE, use){
    apply_by_band(x, stats::var, na.rm = na.rm, keep_txt_meta = TRUE)
}

#' Standard deviation
#'
#' \code{sd} computes the standard deviation spectrum. Note that values will not
#' reflect value anymore, but the sd of the value instead.
#'
#' @param x a numeric vector or an R object which is coercible to one by as.double(x)
#' @param na.rm logical. Should missing values be removed?
#' @return standard deviation
#'
#' @export
sd = function(x, na.rm = FALSE){
    UseMethod("sd")
}

#' Default standard deviation
#'
#' \code{sd} computes the standard deviation of the values in x. If na.rm is TRUE then missing values are removed before computation proceeds.
#'
#' @param x a numeric vector or an R object which is coercible to one by as.double(x)
#' @param na.rm logical. Should missing values be removed?
#' @return standard deviation of x
#'
#' @importFrom stats sd
#' @export
#'
#' @examples
#' x = rnorm(n = 200, mean = 0, sd = 1)
#' sd(x)
#'
sd.default = stats::sd

#' Standard deviation spectrum
#'
#' Forces keep_txt_meta = TRUE
#'
#' @param x spectra
#' @param na.rm boolean. remove NAs?
#' @return single spectrum
#'
#' @importFrom stats sd
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' spec = as_spectra(spec_matrix_example, name_idx = 1)
#' sd(spec)
sd.spectra = function(x, na.rm = TRUE){
    apply_by_band(x, stats::sd, na.rm = na.rm, keep_txt_meta = TRUE)
}


#' Compute spectra quantiles
#'
#' \code{quantile} computes quantiles by band and returns them as `spectra`.
#'
#' @param x spectra object. Must have at least the same number of sample that
#'          length(probs) has.
#' @param probs Probabilities to compute quantiles. Must be a vector of numerics
#'              between 0.0 and 1.0. Defaults to c(0.025, 0.25, 0.5, 0.75, 0.975).
#'              Duplicated probs will be removed.
#' @param na.rm remove NAs before computing quantiles? Defaults to TRUE
#' @param names names for each quantile spectrum. If NULL (default), names are set
#'              to `probs`. A char vector should otherwise be given. Recycled.
#' @param ... other arguments passed to quantile.
#' @return spectra object with one spectrum for each prob
#'
#' @importFrom stats quantile
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' spec = as_spectra(spec_matrix_example, name_idx = 1)
#' quantile(spec, probs = c(0.25, 0.75))
quantile.spectra = function(x,
                            probs = c(0.025, 0.25, 0.5, 0.75, 0.975),
                            na.rm = TRUE,
                            names = NULL,
                            ...){

    ## probs must be between 0 and 1
    if( any(probs < 0.0) || any(probs > 1.0) ){
        stop("Probs must have values between 0 and 1")
    }

    ## probs should not be duplicated
    probs = probs[ ! duplicated(probs) ]

    if(nrow(x) < length(probs)){
        stop("There are less samples (", nrow(x),") than probabilities (",  length(probs), ") for `quantile` to makes sense.")
    }

    ## Construct sample names
    if(is.null(names) | any(is.na(names))){
        n = as.character(probs)
    } else {
        n = as.character(names)
    }

    ## Return spectra quantile object
    x = apply_by_band(x, stats::quantile, probs = probs, na.rm = na.rm,
                      name = n, ...)

    class(x) = c(class(x), "spec_quantile")
    x
}
