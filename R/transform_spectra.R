library("devtools")
devtools::use_package("devtools")
devtools::use_package("parallel")

################################################################################
# Combine spectral datasets
################################################################################

#' Combine spectral datasets
#'
#' \code{combine} binds two spectral datasets. Both spectra must have the
#' very same wavelength labels, but different metadata are acceptable
#'
#' @param s1 spectra object 1
#' @param s2 spectra object 2
#' @return combined spectra object
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library("spectrolab")
#'
#' # Create dummy spectra datasets. Pretend that these are all different...
#' s1 = as.spectra(spec_matrix_example)
#' s2 = as.spectra(spec_matrix_example)
#' s3 = as.spectra(spec_matrix_example)
#'
#' # combine 2 spectra objects
#' s_1and2 = combine(s1, s2)
#'
#' # combine n spectra objects using the `Reduce` function
#' s_n = Reduce(combine, list(s1, s2, s3))
combine = function(s1, s2){
   UseMethod("combine")
}


#' @describeIn combine Combines two spectral datasets
#' @export
combine.spectra = function(s1, s2){
    if( !is_spectra(s2) ){
        stop("Object `b` must be of class spectra")
    }

    if(any( suppressWarnings(wavelengths(s1) != wavelengths(s2)) )){
        stop("Spectra must have the same wavelengths. Consider using `resample()` first")
    }

    r = rbind(reflectance(s1), reflectance(s2))
    n = c(names(s1), names(s2))
    w = wavelengths(s1)               ## OK because I tested for equality before

    if(enforce01(s1) != enforce01(s2)){
        warning("Spectra objects have different enforce01 requirements.\n  Setting enforce01 to FALSE...")
        e = FALSE
    } else {
        e = enforce01(s1)             ## OK because I tested for inequality before
    }

    ## Merge metadata
    m1 = meta(s1)
    m2 = meta(s2)
    mn = union(names(m1), names(m2))

    m3 = data.frame(matrix(nrow     = nrow(m1) + nrow(m2),
                           ncol     = length(mn),
                           dimnames = list(NULL, mn)),
                    check.names = FALSE)

    m3[1 : nrow(m1), names(m1)] = m1
    m3[(1 + nrow(m1)) : nrow(m3), names(m2)] = m2

    spectra(r, w, n, m3, e)
}


################################################################################
# Aggregate
################################################################################

#' Aggregate spectra
#'
#' Applies FUN (and FUN_meta) over spectra aggregating by factor `by`.
#'
#' Argument FUN_meta is useful if you want to apply a different function to
#' metadata and reflectance. If you want to aggregate spectra and metadata
#' using `mean`, `sd`, `median` etc. but try to keep the text values, wrap your
#' function in \code{try_keep_txt(f)}.
#'
#' @param x spectra object
#' @param by vector of factors to guide the aggregation
#' @param FUN function to be applied to refl (and meta if FUN_meta is NULL)
#' @param FUN_meta function to be applied to metadata. If NULL (default), same
#'        FUN applied to reflectance is used.
#' @param ... extra args to FUN
#' @return spectra object
#'
#' @importFrom stats aggregate
#'
#' @author Jose Eduardo Meireles
#' @export
aggregate.spectra = function(x, by, FUN, FUN_meta = NULL, ...){

    if(!is.list(by)){
        by = list(by)
    }

    if(is.null(FUN_meta)){
        FUN_meta = FUN
    }

    r = stats::aggregate(as.matrix(x), by, FUN, ...)
    m = stats::aggregate(meta(x), by, FUN_meta, ...)
    s = as.spectra(r, 1)
    meta(s) = m[ , -1]

    enforce01(s) = enforce01(x)
    s
}

################################################################################
# Split spectral datasets
################################################################################


#' Split spectra
#'
#' \code{split} a spectra object into a list of spectra according to grouping f.
#'
#' @param x spectra object
#' @param f factor vector defining the grouping. Must have length nrow(x)
#' @param drop NOT used
#' @param ... NOT used
#' @return list of spectra
#'
#' @author Jose Eduardo Meireles
#' @export
split.spectra = function(x, f, drop = FALSE, ...){

    v = unlist(f)

    if(nrow(x) != length(v)){
        stop("f must have the same length as the number of samples in x")
    }

    l = split(seq.int(length(v)), v)

    lapply(l, function(y){ x[y, ] })
}

################################################################################
# Subset by
################################################################################

#' Subset spectra by factor
#'
#' \code{subset_by} subsets spectra ensuring that each factor `by` appears only
#' `max` times or less in the spectra dataset.
#'
#' @param x spectra object
#' @param by vector coercible to factor and of same length as nrow(x)
#' @param n_max integer. keep at most this number of spectra per unique `by`
#' @param random boolean. Sample randomly or keep first n_max? Defaults to TRUE
#' @return spectra
#'
#' @importFrom utils tail
#'
#' @author Jose Eduardo Meireles
#' @export
subset_by = function(x, by, n_max, random = TRUE){
    UseMethod("subset_by")
}

#' @describeIn subset_by Subset spectra by factor
#' @export
subset_by.spectra = function(x, by, n_max, random = TRUE){

    by = unlist(by)
    if( ! is.vector(by) || length(by) != nrow(x) ){
        stop("`by` must be a vector length equals the number of rows in x")
    }

    if( ! is.numeric(n_max) || n_max <= 0 ){
        stop("n_max must be a positive interger.")
    } else {
        n_max = ceiling( n_max[[1]] ) # in case max is a vector
    }

    excl_n_by = table(by) - n_max
    excl_n_by = excl_n_by[ excl_n_by > 0 ]

    # Nothing to subset
    if(length(excl_n_by) == 0){
        return(x)
    }

    # Compute indices to exclude
    excl_idx = sapply(names(excl_n_by), function(x){
        w = which(by == x)
        if(random){
            p = sample(w, excl_n_by[[x]])
        } else {
            p = utils::tail(w, n = excl_n_by[[x]])
        }
        p
    })

    excl_idx = unlist(excl_idx)

    x[ - excl_idx ,  ]
}


################################################################################
# Vector normalization
################################################################################

#' Vector normalize spectra
#'
#' \code{normalize} returns a spectra obj with vector normalized reflectances
#'
#' @param x spectra object. Wavelengths must be strictly increasing
#' @param quiet boolean. Warn about change in y value units? Defaults to FALSE
#' @param ... nothing
#' @return spectra object with normalized spectra
#'
#' @author Jose Eduardo Meireles
#' @export
normalize = function(x, quiet = FALSE, ...){
    UseMethod("normalize")
}


#' @describeIn normalize Vector normalize spectra
#' @export
normalize.spectra = function(x, quiet = FALSE, ...){

    i_is_increasing(wavelengths(x), stop = TRUE)

    if(!quiet){
        message("Vector nomalizing spectra...")
        message("Note that y values will not be true reflectances anymore!")

        if( "normalization_magnitude" %in% names(meta(x)) ){
            warning("spectra were apparently already vector normalized.\n  normalization magnitudes may not make sense.")
        }
    }

    refl            = reflectance(x)
    refl_squared    = refl * refl
    vec_ones        = rep.int(1L, ncol(refl_squared))
    spec_sq_rowsum  = refl_squared %*% vec_ones
    magnitudes      = as.vector(sqrt(spec_sq_rowsum))

    # normalize and construct a `spectra` object
    x[] = i_reflectance(refl / magnitudes)

    # add a magnitute attribute to the`spectra` object
    meta(x, "normalization_magnitude") = magnitudes

    x
}

################################################################################
# Smoothing functions
################################################################################

#' Smooth spline functions for spectra
#'
#' Gets spline functions for each spectrum in a spectra object.
#'
#' @param x spectra object. Wavelengths must be strictly increasing
#' @param parallel boolean. Do computation in parallel? Defaults to TRUE.
#'                 Unfortunately, the parallelization does not work on Windows.
#' @param ... additional parameters passed to smooth.spline except nknots, which
#'            is computed internally
#' @return a list of spline functions
#'
#' @importFrom parallel detectCores mclapply
#' @importFrom stats smooth.spline
#'
#' @keywords internal
#' @author Jose Eduardo Meireles
i_smooth_spline_spectra = function(x, parallel = TRUE, ...) {
    if( !is_spectra(x) ){
        stop("Object must be of class spectra")
    }

    scale   = c(0.1, 0.25, 0.5)
    cutres  = 100

    range   = diff(range( wavelengths(x) ))
    resol   = ceiling(range / ncol(x))
    fullres = floor(range / resol)
    propres = floor(range / resol * scale)
    nknots  = min( propres[propres >= cutres], fullres)

    d = reflectance(x)
    r = lapply( seq.int(nrow(x)), function(y){ d[y, ]})
    w = wavelengths(x)

    n = parallel::detectCores()
    l = length(r)

    if(parallel && l > 1) {

        if( .Platform$OS.type == "windows" ){
            message("Parallelization is not availiable for windows. Using 1 core...")
            n = 1
        }

        b = floor(seq.int(0, length(r), length.out = min(n, l) + 1L))

        c = cut(seq.int(length(r)), b, include.lowest = TRUE)

        s = split(r, c)

        s = parallel::mclapply(s, function(z){
            lapply(z, stats::smooth.spline, x = w, nknots = nknots, ...)},
            mc.cores = n)

        return(unlist(s, recursive = FALSE, use.names = FALSE))

    } else {
        return(lapply(r, stats::smooth.spline, x = w, nknots = nknots, ...))
    }
}


#' Smooth moving average for spectra
#'
#' @param x spectra object
#' @param n = NULL
#' @param save_wvls_to_meta boolean. keep lost ends of original wvls in metadata
#' @return spectra object
#'
#' @keywords internal
#' @author Jose Eduardo Meireles
i_smooth_mav_spectra = function(x, n = NULL, save_wvls_to_meta = TRUE){
    if( !is_spectra(x) ){
        stop("Object must be of class spectra")
    }

    if(is.null(n)){
        scale   = c(2, 3, 4, 5, 7, 10, 15, 20)
        cutres  = 150

        range   = diff(range( wavelengths(x) ))
        resol   = ceiling(range / ncol(x))
        fullres = floor(range / resol)
        propres = floor(range / resol / scale)
        n       = max(c(scale[propres >= cutres]), 1)
    }

    if(n == 1){
        stop("Not enough resolution to smooth using moving average. n param was 1.")
    }

    message("Simple moving average over n: ", n)

    r   = reflectance(x)
    s   = t(apply(r, 1, i_mav, n = n))
    w   = which(apply(is.na(s), 2, all))
    ww  = wavelengths(x)[w]
    x[] = s
    x   = x[ , setdiff(wavelengths(x), ww) ]

    if(length(w) != 0){

        message("Smoothing transformed some reflectances into NAs and those wavelengths were removed")

        if(save_wvls_to_meta){
            message("However, the original reflectance values for those wavelengths were kept as metadata")

            meta(x) = matrix(r[ , w],
                             nrow = nrow(x),
                             dimnames = list(NULL, paste("removed_wvl_", ww, sep = "")) )
        }
    }

    x
}

#' Default smoothing function
#'
#' @inherit stats::smooth
#' @importFrom stats smooth
smooth.default = stats::smooth

#' Smooth spectra
#'
#' \code{smooth} runs each spectrum by a smoothing and returns the spectra
#'
#' @param x spectra object. Wavelengths must be strictly increasing
#' @param method Choose smoothing method: "spline" (default) or "moving_average"
#' @param ... additional parameters passed to \code{smooth.spline} or parameters
#'            `n` and `save_wvls_to_meta` for the moving average smoothing.
#' @return a spectra object of with smoothed spectra
#'
#' @author Jose Eduardo Meireles
#' @export
smooth = function(x, method = "spline", ...){
    UseMethod("smooth")
}

#' @describeIn smooth Smooth spectra
#' @export
smooth.spectra = function(x, method = "spline", ...){

    i_is_increasing(wavelengths(x), stop = TRUE)

    if(method == "spline") {
        s   = i_smooth_spline_spectra(x, ...)
        x[] = do.call(rbind, sapply(s, `[`, "y"))
        return(x)
    } else if (method == "moving_average") {
        i_smooth_mav_spectra(x, ...)
    }
}

################################################################################
# Resampling spectra
################################################################################

#' Resample spectra
#'
#' \code{resample} returns spectra resampled to new wavelengths using smoothing.
#' Possible to increase or decrease the spectral resolution.
#'
#' The function runs a couple basic checks when resampling, but they are not
#' exhaustive, so look at the data before resampling. The implemented checks are:
#' 1. Stop if trying to predict wavelengths outside of the original range and,
#' 2. Warn if a gap is found in wavelengths. E.g. wvls are mostly at a 1nm
#'    resolution but go from 1530 to 1820 in the infrared. Does not check for NAs
#'
#' @param x spectra object. Wavelengths must be strictly increasing
#' @param new_wvls numeric vector of wavelengths to sample from spectra
#' @param ... additional parameters passed to the \code{smooth.spline} function.
#' @return spectra object with resampled spectra
#'
#' @importFrom stats predict
#'
#' @author Jose Eduardo Meireles
#' @export
resample = function(x, new_wvls, ...) {
    UseMethod("resample")
}


#' @describeIn resample Resample spectra
#' @export
resample.spectra = function(x, new_wvls, ...) {

    w = wavelengths(x)

    ## Simply subset the current spectra if all new_wvls are a in the set of
    ## current wavelengths

    if(all(new_wvls %in% w)){
        return(x[ , new_wvls ])
    }

    ## Enforce increasing wavelengths in spectra object
    i_is_increasing(w, stop = TRUE)


    ## Warn about long gaps in wavelengths
    ## Made up these thresholds, need to think harder
    d = diff(w)
    l = d > quantile(d, 0.5) * 6 |
        d > quantile(d, 0.9) * 3 |
        d > 20

    if(any(l)){
        for(i in which(l)){
            warning("Found long gap between wavelengths ",
                    w[i - 1], " and ", w[ i + 1], " (", d[i], ")", "\n",
                    "Reflectances resampled in this gap should probably be converted to NAs.")
        }
    }

    ## Do not predict points outside the original wavelength range
    r = range(wavelengths(x))

    if(min(new_wvls) < r[1] || max(new_wvls) > r[2]){
        stop("New wavelength values must be within the data's range: ", r[1], " to ", r[2])
    }

    ## Smooth and predict
    message("Using spline to predict reflectance at new wavelengths...")
    s = i_smooth_spline_spectra(x, ...)
    f = function(o, p){ stats::predict(o, p)[["y"]] }
    g = lapply(X = s, FUN = f, p = new_wvls)
    d = i_reflectance( do.call(rbind, g) )
    message("Beware the spectra are now partially smoothed.")


    ## Wavelength number may change, so using the "safe" setter will fail
    ## Instead of reaching inside the spectra object, I am using the "unsafe"
    ## version of the wavelength setter.
    wavelengths(x, unsafe = TRUE) = new_wvls

    ## THIS IS BAD. Figure out an "unsafe" version of the reflectance setter
    x[["reflectance"]] = d

    x
}
