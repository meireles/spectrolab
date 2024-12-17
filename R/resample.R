################################################################################
# Internal Functions
################################################################################

#' Get the FWHM from the difference between band values
#'
#' @param bands band values. numeric
#'
#' @return FWHM as a numeric vector
#' @keywords internal
fwhm_from_band_diff = function(bands){
    wave_diff = diff(bands)
    c(wave_diff[1], wave_diff)
}

#' Resample the FWHM to a new set of bands using a gaussian model
#'
#' @param old_bands band values of the original spectra
#' @param old_fwhm FWHM for the original spectra
#' @param new_bands band values for the resampled spectra
#' @param new_fwhm FWHM for the resampled spectra
#' @param return_type Either "max" (default) or "old". Max returns the maximum from either the old or the new FWHM for each band
#' @param k number of FWHM categories. Defaults to 0, which means, return the FWHM in as much detail as possible.
#'
#' @return a numeric vector of FWHM estimates
#'
#' @importFrom stats dnorm kmeans
#'
#' @keywords internal
i_make_fwhm = function(old_bands,
                       old_fwhm,
                       new_bands,
                       new_fwhm,
                       return_type = "max",
                       k           = 0){

    # Standard deviation from FWHM
    sigma0 = new_fwhm / (2 * sqrt(2 * log(2)))

    # Resample the OLD sensor FWHM to the resolution of the NEW sensor
    fwhm_old_new  = mapply(function(l, s){
        k0 = stats::dnorm(old_bands, mean = l, sd = s)
        sum(old_fwhm * k0) / sum(k0)
    }, l = new_bands, s = sigma0)

    if(return_type == "max"){
        fwhm = pmax(new_fwhm, fwhm_old_new)
    } else if (return_type == "old"){
        fwhm = fwhm_old_new
    } else {
        stop("return_type must be either 'max' or 'old'")
    }

    # K

    n_unique  = length(unique(fwhm))
    if(k == 0){
        NULL
    } else if (k > n_unique) {
        k = n_unique
        message("setting k to the number of unique band diff values")
        clust = stats::kmeans(fwhm, k)
        fwhm  = clust$centers[clust$cluster, 1]

    } else {
        clust = kmeans(fwhm, k)
        fwhm  = clust$centers[clust$cluster, 1]
    }
    names(fwhm)  = new_bands
    return(fwhm)
}

################################################################################
# Exported Functions
################################################################################

#' Resample the FWHM to a new set of bands using a gaussian model
#'
#' @param spec spectra object
#' @param new_bands band values to resample the spectra to
#' @param new_fwhm FWHM for the new bands
#' @param return_type either "max" or "old". If "old" (default), it returns the fwhm inferred from the original's spectra bands. If max (default), it returns the max between the new and old fwhm.
#' @param k number of unique FHWM to estimate
#'
#' @return FWHM as a numeric vector
#' @export
make_fwhm = function(spec,
                     new_bands,
                     new_fwhm = NULL,
                     return_type     = "max",
                     k               = 3){
    if(is.null(new_fwhm)){
        new_fwhm = fwhm_from_band_diff(new_bands)
    }

    b = bands(spec)

    fwhm = i_make_fwhm(old_bands = b,
                       old_fwhm = fwhm_from_band_diff(b),
                       new_bands = new_bands,
                       new_fwhm = new_fwhm,
                       return_type     = return_type,
                       k               = k)

    return(fwhm)
}


#' Resample spectra
#'
#' @param spec spectra object
#' @param new_bands band values to resample the spectra to
#' @param fwhm FWHM for the new bands
#'
#' @return resampled spectra
#'
#' @export
resample = function(spec,
                    new_bands,
                    fwhm) {

    bands        = bands(spec)
    reflectance  = value(spec)

    if(length(fwhm) == 1){
        fwhm = rep(fwhm, length.out = length(new_bands))
    } else if (length(fwhm) == length(new_bands)){
        NULL
    } else {
        stop("provide a single fwhm value or one for each new_band")
    }

    ## Enforce increasing bands in spectra object
    if(! i_is_increasing(bands)){
        stop("resample requires strictly increasing band values.\nMatch sensor overlap before attempting to resample the spectra.")
    }

    margin       = fwhm/4
    band_range   = range(bands(spec)) + c(-head(margin, 1), tail(margin, 1))
    within_range = which(new_bands > band_range[1] & new_bands < band_range[2])

    if(length(within_range) < length(new_bands)){
        message("trimmed new band values to fall within the range of the orignial bands.")
    }

    new_bands = new_bands[within_range]
    fwhm      = fwhm[within_range]

    # Standard deviation from FWHM
    sigma = fwhm / (2 * sqrt(2 * log(2)))

    # Resample reflectance to new bands
    resampled_reflectance = mapply(function(l, s){
        k = stats::dnorm(bands, mean = l, sd = s)
        colSums(t(reflectance) * k) / sum(k)
    }, l = new_bands, s = sigma)

    s = spectra(value = resampled_reflectance,
                bands = new_bands,
                names = names(spec),
                meta  = meta(spec))
    s
}
