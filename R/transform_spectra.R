#usethis::use_package("parallel", type = "Suggests")

################################################################################
# Apply by band
################################################################################

#' Apply numeric function by band
#'
#' \code{apply_by_band} is conceptually similar to apply(as.matrix(x), 2, fun),
#' but returns a spectra object while dealing with metadata and attributes.
#' Applying a function that does not act on numeric values may crash the function
#' or render all values NA.
#'
#' @param x spectra
#' @param fun numeric function to be applied to each band.
#' @param na.rm boolean. remove NAs?
#' @param keep_txt_meta boolean. try to keep text in the metadata?
#' @param name name for each sample in the output spectra. The default (NULL) will
#'             give samples sequential numeric names. Recycled if necessary.
#' @param ... extra arguments passed to fun
#' @return spectra
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' spec = as.spectra(spec_matrix_example, name_idx = 1)
#' spec_mean = apply_by_band(spec, mean)
apply_by_band = function(x, fun, na.rm = TRUE, keep_txt_meta = TRUE, name = NULL, ...){
    UseMethod("apply_by_band")
}

#' @describeIn apply_by_band Apply a numeric function by band
#'
#' @importFrom stats na.omit
#'
#' @author Jose Eduardo Meireles
#' @export
apply_by_band.spectra = function(x, fun, na.rm = TRUE, keep_txt_meta = TRUE, name = NULL, ...){

    # Wrap function fun to remove NAs from the computation
    # Works for functions that don't normally take a na.rm parameter,
    # such as sqrt or abs
    f_na_wrap = function(fun, na.rm){
        if(na.rm){
            function(x, ...){ fun( stats::na.omit(x), ...) }
        } else {
            function(x, ...){ fun(x, ...) }
        }
    }

    f  = f_na_wrap(fun, na.rm)
    fm = ifelse(keep_txt_meta, try_keep_txt(f), f)

    r  = apply(as.matrix(x), 2, f, ...)
    w  = bands(x)
    m0 = meta(x)
    m  = m0

    l = ifelse(is.vector(r), 1L, nrow(r))
    if(is.null(name)){
        n = seq(l)
    } else {
        n = rep(name, length.out = l)
    }

    if(ncol(m) != 0){
        m = lapply(m, fm, ...)  # Calling lapply because meta is always a data.frame
        m = do.call(cbind, m)
    }
    spectra(value = r, bands = w, names = n, meta = m)
}


################################################################################
# Aggregate
################################################################################

#' Aggregate spectra
#'
#' Applies FUN (and FUN_meta) over spectra aggregating by factor `by`.
#'
#' Argument FUN_meta is useful if you want to apply a different function to
#' metadata and value. If you want to aggregate spectra and metadata
#' using `mean`, `sd`, `median` etc. but try to keep the text values, wrap your
#' function in \code{try_keep_txt(f)}.
#'
#' @param x spectra object
#' @param by vector of factors to guide the aggregation
#' @param FUN function to be applied to refl (and meta if FUN_meta is NULL)
#' @param FUN_meta function to be applied to metadata. If NULL (default), same
#'        FUN applied to value is used.
#' @param ... extra args to FUN
#' @return spectra object
#'
#' @importFrom stats aggregate
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' spec = as.spectra(spec_matrix_example, name_idx = 1)
#' spec_mean = aggregate(spec, by = names(spec), mean, try_keep_txt(mean))
aggregate.spectra = function(x, by, FUN, FUN_meta = NULL, ...){

    if(!is.list(by)){
        by = list(by)
    }

    if(is.null(FUN_meta)){
        FUN_meta = FUN
    }

    r = stats::aggregate(as.matrix(x), by, FUN, ...)
    #m = stats::aggregate(meta(x), by, FUN_meta, ...)

    m = tryCatch({
        stats::aggregate(meta(x), by, FUN_meta, ...)
        },
        warning = function(w){
            message("Issues found when aggregating the metadata")
            message("This usualy happens when a mathematical function is applied to non-numeric data.")
            message("Here are the original warnings: ", w)
            suppressWarnings(stats::aggregate(meta(x), by, FUN_meta, ...))
        })

    s = as.spectra(r, 1)
    meta(s) = m[ , -1]

    s
}

################################################################################
# Combine spectral datasets
################################################################################

#' Combine spectral datasets
#'
#' \code{combine} binds two spectral datasets. Both spectra must have the
#' very same band labels, but different metadata are acceptable
#'
#' @param s1 spectra object 1
#' @param s2 spectra object 2
#' @return combined spectra object
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#'
#' # Create dummy spectra datasets. Pretend that these are all different...
#' s1 = as.spectra(spec_matrix_example, name_idx = 1)
#' s2 = as.spectra(spec_matrix_example, name_idx = 1)
#' s3 = as.spectra(spec_matrix_example, name_idx = 1)
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

    if(any( suppressWarnings(bands(s1) != bands(s2)) )){
        stop("Spectra must have the same bands. Consider using `resample()` first")
    }

    r = rbind(value(s1), value(s2))
    n = c(names(s1), names(s2))
    w = bands(s1)               ## OK because I tested for equality before

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

    spectra(r, w, n, m3)
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
#'
#' @examples
#' library(spectrolab)
#' spec = as.spectra(spec_matrix_example, name_idx = 1)
#' spec_list = split(spec, names(spec))
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
#' \code{subset_by} subsets spectra by a factor `by` ensuring that it appears at
#' most `n_max` times **and** at least `n_min` times in the dataset.
#'
#' Note that \code{subset_by} forces you to provide both a minimum and a maximum
#' number of spectra to be kept for each unique value of `by`. In case you're
#' interested in subsetting \emph{only} based on `n_min`, set `n_max` to `Inf`.
#'
#' @param x spectra object
#' @param by vector coercible to factor and of same length as nrow(x)
#' @param n_min int. only keep spectra with at least (inclusive) `n_min` number
#'              of samples per unique `by`.
#' @param n_max int. keep at most (incl) this number of spectra per unique `by`
#' @param random boolean. Sample randomly or keep first n_max? Defaults to TRUE
#' @return spectra
#'
#' @importFrom utils tail
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' spec = as.spectra(spec_matrix_example, name_idx = 1)
#'
#' # remove spec of species with less than 4 samples
#' spec = subset_by(spec, by = names(spec), n_min = 4, n_max = Inf)
subset_by = function(x, by, n_min, n_max, random = TRUE){
    UseMethod("subset_by")
}

#' @describeIn subset_by Subset spectra by factor
#' @export
subset_by.spectra = function(x, by, n_min, n_max, random = TRUE){

    by = unlist(by)

    if( ! is.vector(by) || length(by) != nrow(x) ){
        stop("`by` must be a vector length equals the number of rows in x")
    }

    if( ! is.numeric(n_min) || n_min <= 0 ){
        stop("n_min must be a positive interger, i.e. at least 1.")
    } else {
        n_min = ceiling( n_min[[1]] )
    }

    if( ! is.numeric(n_max) || n_max <= 0 ){
        stop("n_max must be a positive interger.")
    } else {
        n_max = ceiling( n_max[[1]] )
    }

    if(n_max < n_min){
        stop("`n_max` must be larger than `n_min`")
    }


    ########################################
    ## Subset based on n_min
    ########################################

    tbl_by   = table(by)

    keep_lbl = names(tbl_by[ tbl_by >= n_min ])
    keep_idx = which(by %in% keep_lbl)

    if(length(keep_idx) == 0){
        message("chosen `n_min` excluded all spectra. returning NULL.")
        return(NULL)
    } else {
        x  = x[ keep_idx, ]
        by = by[ keep_idx ]
    }

    ########################################
    ## Subset based on n_max
    ########################################

    excl_n_by = table(by) - n_max
    excl_n_by = excl_n_by[ excl_n_by > 0 ]

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

    # Exclude indices from `x` and `by` if there's something to exclude
    if(length(excl_idx) > 0){
        x  = x[ - excl_idx ,  ]
    }

    ########################################
    ## return
    ########################################
    x

}


################################################################################
# Vector normalization
################################################################################

#' Vector normalize spectra
#'
#' \code{normalize} returns a spectra obj with vector normalized values
#' Normalization value for each spectrum is computed as sqrt(sum(x_i^2))
#'
#' Normalization value for each spectrum computed as sqrt(sum(x^2))
#'
#' @param x spectra object. bands must be strictly increasing
#' @param quiet boolean. Warn about change in y value units? Defaults to FALSE
#' @param ... nothing
#' @return spectra object with normalized spectra
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' spec = as.spectra(spec_matrix_example, name_idx = 1)
#' spec = normalize(spec)
normalize = function(x, quiet = FALSE, ...){
    UseMethod("normalize")
}


#' @describeIn normalize Vector normalize spectra
#' @export
normalize.spectra = function(x, quiet = FALSE, ...){

    if(! i_is_increasing(bands(x))){
        stop("normalize requires strictly increasing band values.\nMatch sensor overlap before attempting to normalize the spectra.")
    }

    if(!quiet){
        message("Vector nomalizing spectra...")
        message("Note that y values will not be true values anymore!")

        if( "normalization_magnitude" %in% names(meta(x)) ){
            warning("spectra were apparently already vector normalized.\n normalization magnitudes may not make sense.")
        }
    }

    refl        = value(x)
    magnitudes  = sqrt(apply(refl^2, 1, sum))
    x[]         = refl / magnitudes

    # add a magnitute attribute to the`spectra` object
    meta(x, "normalization_magnitude") = magnitudes

    x
}

################################################################################
# Smoothing functions
################################################################################

#' Generic Smoothing function
#'
#' @param x data to smooth over
#' @param ... additional arguments
#' @return smoothed data
#'
#' @export
smooth = function(x, ...){
    UseMethod("smooth")
}

#' Default smoothing function
#'
#' @param x data to smooth over
#' @param ... additional arguments
#' @return smoothed data
#'
#' @importFrom stats smooth
#' @export
smooth.default = function(x, ...){
    stats::smooth(x, ...)
}

#' Smooth spectra
#'
#' \code{smooth} runs each spectrum by a smoothing and returns the spectra
#'
#' @param x spectra object. bands must be strictly increasing
#' @param method Choose smoothing method: "spline" (default) or "moving_average"
#' @param ... additional parameters passed to \code{smooth.spline} or parameters
#'            `n` and `save_wvls_to_meta` for the moving average smoothing.
#' @return a spectra object of with smoothed spectra
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#'
#' spec = as.spectra(spec_matrix_example, name_idx = 1)
#' spec = smooth(spec, parallel = FALSE)
smooth.spectra = function(x, method = "spline", ...){

    if(! i_is_increasing(bands(x))){
        stop("smooth requires strictly increasing band values.\nMatch sensor overlap before attempting to smooth the spectra.")
        }


    if(method == "spline") {
        s   = i_smooth_spline_spectra(x, ...)
        x[] = do.call(rbind, sapply(s, `[`, "y"))
        return(x)
    } else if (method == "moving_average") {
        i_smooth_mav_spectra(x, ...)
    }
}

#' Smooth spline functions for spectra
#'
#' Gets spline functions for each spectrum in a spectra object.
#'
#' @param x spectra object. bands must be strictly increasing
#' @param parallel boolean. Do computation in parallel? Defaults to TRUE.
#'                 Unfortunately, the parallelization does not work on Windows.
#' @param ... additional parameters passed to smooth.spline except nknots, which
#'            is computed internally
#' @return a list of spline functions
#'
#' @importFrom stats smooth.spline
#' @importFrom parallel detectCores mclapply
#'
#' @keywords internal
#' @author Jose Eduardo Meireles
i_smooth_spline_spectra = function(x, parallel = TRUE, ...) {

    if( !is_spectra(x) ){
        stop("Object must be of class spectra")
    }

    scale   = c(0.1, 0.25, 0.5)
    cutres  = 100

    range   = diff(range( bands(x) ))
    resol   = ceiling(range / ncol(x))
    fullres = floor(range / resol)
    propres = floor(range / resol * scale)
    nknots  = min( propres[propres >= cutres], fullres)

    d = value(x)
    r = lapply( seq.int(nrow(x)), function(y){ d[y, ]})
    l = length(r)
    w = bands(x)

    # parallel?
    p = requireNamespace("parallel", quietly = TRUE)
    p = p && parallel && l > 1 && .Platform$OS.type != "windows"

    if(p){
        n = parallel::detectCores() - 1L
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

        range   = diff(range( bands(x) ))
        resol   = ceiling(range / ncol(x))
        propres = floor(range / resol / scale)
        n       = max(c(scale[propres >= cutres]), 1)
    }

    if(n == 1){
        stop("Not enough resolution to smooth using moving average. n param was 1.")
    }

    message("Simple moving average over n: ", n)

    r   = value(x)
    s   = t(apply(r, 1, i_mav, n = n))
    w   = which(apply(is.na(s), 2, all))
    ww  = bands(x)[w]
    x[] = s
    x   = x[ , setdiff(bands(x), ww) ]

    if(length(w) != 0){

        message("Smoothing transformed some values into NAs and those bands were removed")

        if(save_wvls_to_meta){
            message("However, the original value values for those bands were kept as metadata")

            meta(x) = matrix(r[ , w],
                             nrow = nrow(x),
                             dimnames = list(NULL, paste("removed_wvl_", ww, sep = "")) )
        }
    }

    x
}

################################################################################
# Resampling spectra
################################################################################

#' Resample spectra
#'
#' \code{resample} returns spectra resampled to new bands using spline smoothing.
#' Possible to increase or decrease the spectral resolution.
#'
#' \code{resample} doesn't predict values for bands outside of the original range.
#'
#' @param x spectra object. bands must be strictly increasing
#' @param new_wvls numeric vector of bands to sample from spectra
#' @param ... additional parameters passed to the \code{smooth.spline} function.
#' @return spectra object with resampled spectra
#'
#' @importFrom stats predict
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' spec = as.spectra(spec_matrix_example, name_idx = 1)
#' spec = resample(spec, new_wvls = seq(400, 2400, 0.5), parallel = FALSE)
resample = function(x, new_wvls, ...) {
    UseMethod("resample")
}


#' @describeIn resample Resample spectra
#' @export
resample.spectra = function(x, new_wvls, ...) {

    w = bands(x)

    ## Simply subset the current spectra if all new_wvls are a in the set of
    ## current bands
    if(all(new_wvls %in% w)){
        return(x[ , new_wvls ])
    }

    ## Enforce increasing bands in spectra object
    if(! i_is_increasing(bands(x))){
        stop("resample requires strictly increasing band values.\nMatch sensor overlap before attempting to resample the spectra.")
    }

    ## Do not predict points outside the original band range
    r = range(bands(x))

    if(min(new_wvls) < r[1] || max(new_wvls) > r[2]){
        stop("New band values must be within the data's range: ", r[1], " to ", r[2])
    }

    ## Smooth and predict
    message("Using spline to predict value at new bands...")
    s = i_smooth_spline_spectra(x, ...)
    f = function(o, p){ stats::predict(o, p)[["y"]] }
    g = lapply(X = s, FUN = f, p = new_wvls)
    message("Beware the spectra are now partially smoothed.")

    ## Construct new spectra object and return
    spectra(value = do.call(rbind, g),
            bands = new_wvls,
            names = names(x),
            meta  = meta(x))

}
