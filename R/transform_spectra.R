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
#' spec = as_spectra(spec_matrix_example, name_idx = 1)
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
#' @param FUN function to be applied to value (and meta if FUN_meta is NULL)
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
#' spec = as_spectra(spec_matrix_example, name_idx = 1)
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

    s = as_spectra(r, 1)
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
#' s1 = as_spectra(spec_matrix_example, name_idx = 1)
#' s2 = as_spectra(spec_matrix_example, name_idx = 1)
#' s3 = as_spectra(spec_matrix_example, name_idx = 1)
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
#' spec = as_spectra(spec_matrix_example, name_idx = 1)
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
#' spec = as_spectra(spec_matrix_example, name_idx = 1)
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
#' \code{normalize} returns a spectra obj with vector normalized values.
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
#' spec = as_spectra(spec_matrix_example, name_idx = 1)
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
    magnitudes  = sqrt(apply(refl^2, 1, sum, na.rm = TRUE))
    x[]         = refl / magnitudes

    # add a magnitude attribute to the`spectra` object
    meta(x, "normalization_magnitude") = magnitudes

    x
}
