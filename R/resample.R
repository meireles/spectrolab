################################################################################
# Internal Functions
################################################################################

#' Derive per-band FWHM from band spacing using the midpoint rule
#'
#' Estimates the Full Width at Half Maximum of each band from the spacing of its
#' neighbours: the interior bands get the centered difference
#' \code{(b[i+1] - b[i-1]) / 2}, and the two end bands get the one-sided
#' difference. On a uniform grid this is just the band spacing; on a non-uniform
#' grid it doubles as the wavelength-interval (\eqn{\Delta\lambda}) weight used
#' when resampling, which is what removes the bias at detector-boundary spacing
#' jumps. This is the same default SpectralPython uses.
#'
#' @param bands band values. numeric, length >= 2
#'
#' @return FWHM as a numeric vector, one per band
#' @keywords internal
i_fwhm_midpoint = function(bands){
    n = length(bands)
    if(n < 2){
        stop("need at least two bands to derive a FWHM from band spacing")
    }
    f      = numeric(n)
    f[1]   = bands[2] - bands[1]
    f[n]   = bands[n] - bands[n - 1]
    if(n > 2){
        f[2:(n - 1)] = (bands[3:n] - bands[1:(n - 2)]) / 2
    }
    f
}

#' Resample the FWHM to a new set of bands using a gaussian model
#'
#' @param old_bands band values of the original spectra
#' @param old_fwhm FWHM for the original spectra
#' @param new_bands band values for the resampled spectra
#' @param new_fwhm FWHM for the resampled spectra
#' @param return_type Either "max" (default) or "old". Max returns the maximum from either the old or the new FWHM for each band
#'
#' @return a numeric vector of FWHM estimates
#'
#' @importFrom stats dnorm
#'
#' @keywords internal
i_make_fwhm = function(old_bands,
                       old_fwhm,
                       new_bands,
                       new_fwhm,
                       return_type = "max"){

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

    names(fwhm)  = new_bands
    return(fwhm)
}

################################################################################
# Exported Functions
################################################################################

#' Estimate the effective FWHM of resampled spectra
#'
#' Estimates the Full Width at Half Maximum that a spectrum resampled onto
#' \code{new_bands} would effectively carry. This is a *metadata* heuristic
#' describing the resampled bandpass; it is not part of the value resampling done
#' by \code{\link{resample}}. The original sensor FWHM (derived from band spacing
#' with the midpoint rule when not supplied) is smeared onto \code{new_bands}
#' with a Gaussian weighting, and by default the larger of the requested and the
#' propagated FWHM is returned for each band.
#'
#' @param spec spectra object
#' @param new_bands band values to resample the spectra to
#' @param new_fwhm FWHM for the new bands. When \code{NULL} (default) it is
#'   derived from the spacing of \code{new_bands} using the midpoint rule.
#' @param return_type either "max" (default) or "old". If "old", returns the
#'   FWHM propagated from the original spectra's bands. If "max", returns the
#'   larger of the new and propagated FWHM for each band.
#'
#' @return FWHM as a numeric vector, one per new band
#' @export
make_fwhm = function(spec,
                     new_bands,
                     new_fwhm    = NULL,
                     return_type = "max"){
    if( !is_spectra(spec) ){
        stop("Object must be of class spectra")
    }
    if(is.null(new_fwhm)){
        new_fwhm = i_fwhm_midpoint(new_bands)
    }

    b = bands(spec)

    fwhm = i_make_fwhm(old_bands   = b,
                       old_fwhm    = i_fwhm_midpoint(b),
                       new_bands   = new_bands,
                       new_fwhm    = new_fwhm,
                       return_type = return_type)

    return(fwhm)
}


#' Resample spectra
#'
#' Resamples spectra onto \code{new_bands} using an overlap-integral model: each
#' *source* band is treated as a boxcar of width equal to its own FWHM, and each
#' *destination* band as a Gaussian response centered at the new band with
#' \code{sigma = fwhm / (2 * sqrt(2 * log(2)))}. The unnormalized weight of
#' source band \eqn{i} in destination band \eqn{j} is the Gaussian mass that
#' falls inside the source boxcar,
#' \deqn{w_{ij} = \Phi\!\left(\frac{b_i + f_i/2 - t_j}{\sigma_j}\right) -
#'               \Phi\!\left(\frac{b_i - f_i/2 - t_j}{\sigma_j}\right),}
#' and the weights are then normalized per destination band. Carrying the source
#' band width as a \eqn{\Delta\lambda} weight removes the bias that a pure
#' point-sampled ("delta function") kernel shows at detector-boundary spacing
#' jumps on non-uniform grids.
#'
#' Destination bands whose total (unnormalized) covered mass falls below
#' \code{coverage_min} -- typically those beyond the source range or inside a
#' gap -- are returned as \code{NA} with a single warning, rather than being
#' silently trimmed away.
#'
#' @param spec spectra object
#' @param new_bands band values to resample the spectra to
#' @param fwhm FWHM for the new (destination) bands. Either a single value
#'   broadcast to every new band, or one value per new band.
#' @param src_fwhm FWHM (boxcar width) of the *source* bands. When \code{NULL}
#'   (default) it is derived from the spacing of the source bands with the
#'   midpoint rule. Power users with known instrument bandpass metadata can pass
#'   a single value or one value per source band.
#' @param coverage_min minimum fraction of a destination band's Gaussian
#'   response that must be covered by source bands for the resampled value to be
#'   returned; below it the band is set to \code{NA}. Defaults to 0.5.
#'
#' @return resampled spectra
#'
#' @importFrom stats pnorm
#'
#' @export
resample = function(spec,
                    new_bands,
                    fwhm,
                    src_fwhm     = NULL,
                    coverage_min = 0.5) {

    if( !is_spectra(spec) ){
        stop("Object must be of class spectra")
    }

    bands        = bands(spec)
    reflectance  = value(spec)

    ## Validate the destination grid. Without this, unsorted or duplicated
    ## `new_bands` produced a spectra that violates the strictly-increasing
    ## invariant every downstream function requires, and the failure surfaced
    ## much later as a misleading "match sensor overlap first" error.
    if( !is.numeric(new_bands) || length(new_bands) == 0 ){
        stop("`new_bands` must be a non-empty numeric vector", call. = FALSE)
    }
    if( anyNA(new_bands) || !all(is.finite(new_bands)) ){
        stop("`new_bands` must be finite and contain no NAs", call. = FALSE)
    }
    if( ! i_is_increasing(new_bands) ){
        stop("`new_bands` must be strictly increasing (no duplicates, sorted ",
             "low to high). Try sort(unique(new_bands)).", call. = FALSE)
    }

    ## Broadcast or validate the destination FWHM
    if(length(fwhm) == 1){
        fwhm = rep(fwhm, length.out = length(new_bands))
    } else if (length(fwhm) != length(new_bands)){
        stop("provide a single fwhm value or one for each new_band")
    }

    ## Enforce increasing bands in spectra object
    if(! i_is_increasing(bands)){
        stop("resample requires strictly increasing band values.\nMatch sensor overlap before attempting to resample the spectra.")
    }

    ## Source-band widths (boxcar FWHM). Default: midpoint rule, which also acts
    ## as the wavelength-interval weight.
    if(is.null(src_fwhm)){
        src_fwhm = i_fwhm_midpoint(bands)
    } else if(length(src_fwhm) == 1){
        src_fwhm = rep(src_fwhm, length.out = length(bands))
    } else if(length(src_fwhm) != length(bands)){
        stop("provide a single src_fwhm value or one for each source band")
    }

    # Standard deviation from FWHM (destination Gaussian response)
    sigma = fwhm / (2 * sqrt(2 * log(2)))

    # Overlap integral of each destination Gaussian with each source boxcar:
    # w_ij = Phi((b_i + f_i/2 - t_j)/sigma_j) - Phi((b_i - f_i/2 - t_j)/sigma_j)
    box_lo = bands - src_fwhm / 2
    box_hi = bands + src_fwhm / 2

    upper = outer(box_hi, seq_along(new_bands),
                  function(x, j) stats::pnorm((x - new_bands[j]) / sigma[j]))
    lower = outer(box_lo, seq_along(new_bands),
                  function(x, j) stats::pnorm((x - new_bands[j]) / sigma[j]))
    gauss_kernel = upper - lower

    # Coverage = total unnormalized mass per destination band. A non-finite
    # coverage (e.g. a NaN/NA fwhm from make_fwhm at a far out-of-range band)
    # counts as uncovered rather than poisoning the logical index below.
    coverage  = colSums(gauss_kernel)
    uncovered = !is.finite(coverage) | coverage < coverage_min

    # Normalize the weights per destination band.
    gauss_kernel = sweep(gauss_kernel, 2, coverage, "/")

    # Compute the resampled reflectance
    resampled_reflectance = reflectance %*% gauss_kernel

    # Under-covered destination bands are honestly NA, not silently trimmed.
    if(any(uncovered)){
        resampled_reflectance[ , uncovered] = NA
        warning(sum(uncovered),
                " new band(s) fell outside the source coverage and were set to NA.")
    }

    # Create the resulting spectra object
    s = spectra(value = resampled_reflectance,
                bands = new_bands,
                names = names(spec),
                meta  = meta(spec))

    ## Resampling changes the band grid but not which sample came from which
    ## instrument, so the per-sample provenance still applies. Carrying it here
    ## also keeps it alive through smooth(method = "gaussian"), which routes
    ## through resample(). See R/sensor_info.R.
    attr(s, "sensor_info") = sensor_info(spec)

    s
}
