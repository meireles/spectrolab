#' Find sensor overlap bounds
#'
#' \code{i_find_sensor_overlap_bounds} finds the overlap bounds between sensors
#'
#' @param x wavelength vector
#' @param idx boolean. return indices? defaults to TRUE
#' @return data.frame with sensor bounds
#'
#' @keywords internal
#' @author Jose Eduardo Meireles
i_find_sensor_overlap_bounds = function(x, idx = TRUE){
    decrease    = which(diff(x) < 0.0)
    n_decreases = length(decrease) + 1
    dimnames    = list(c("begin", "end"),
                       paste("sensor", seq(n_decreases), sep = "_"))

    bounds = matrix(data     = c( c(1, decrease + 1), c(decrease, length(x)) ),
                    ncol     = n_decreases,
                    byrow    = TRUE,
                    dimnames = dimnames)
    if(!idx){
        bounds["begin", ] = x[ bounds["begin", ] ]
        bounds["end", ]   = x[ bounds["end", ] ]
    }
    as.data.frame(bounds)
}


#' Guess splice bands (bounds between sensors)
#'
#' @param x spectra object
#' @return vector of band values
#'
#' @author Jose Eduardo Meireles
#' @export
guess_splice_at = function(x){
    UseMethod("guess_splice_at")
}


#' @describeIn guess_splice_at Guess splice bands (bounds between sensors)
#' @export
guess_splice_at.spectra = function(x){

    w         = bands(x)
    b         = i_find_sensor_overlap_bounds(w, idx = FALSE)
    nb        = ncol(b)

    if(nb > 1){

        # If there is an overlap (i.e. SVC or PSR)

        splice_at = rep(NA, nb - 1)

        for(i in seq(nb - 1)){
            scalar       = 2 * i # Use most of the right sensor instead of the left one
            splice_at[i] = (b[1, i + 1] * scalar + b[2, i]) / (scalar + 1)
        }

    } else {

        message("Guessing sensor bounds is unreliable so please visually inspect your spectra.")

        # If there is no overlap (e.g. ASD), you may see a "jump" between adjacent
        # bands.

        # Trim ends
        p = 0.10             # percent to trim
        r = range(bands(x))
        t = diff(r) * p
        y = x[ , bands(x, r[1] + t, r[2] - t)]

        # Take the third derivative of the spectra and calculate its average
        # by band
        z = apply(y, 1, diff, difference = 3)
        e = rowMeans(abs(z))

        # Find the band names that exceeds x SDs from the mean
        s = 6
        m = mean(e) + s * sd(e)
        e = e[ e >= m ]
        g = sort(as.numeric(names(e)))

        # Now there are "clusters" of band names that meed that the mean + x SD
        # criterion. I will assume that sensors are not more than j units (e.g. nm)
        # apart so I can return the smallest band name from each cluster

        j         = 100
        splice_at = g[ c(TRUE, diff(g) >= j) ]
    }
    return(splice_at)
}



#' Trim sensor overlap
#'
#' @param x spectra object
#' @param splice_at bands where to splice sensors. suggests where the
#'                  beginning of sensors 2 and 3 should be.
#' @return spectra object
#'
#' @keywords internal
#' @author Jose Eduardo Meireles
i_trim_sensor_overlap = function(x, splice_at){

    w         = bands(x)
    b         = i_find_sensor_overlap_bounds(w)
    bb        = b
    no_over   = ncol(b) == 1
    splice_at = sort(splice_at)

    if(no_over){

        # If no overlap is found, break by splice_at since match_spectra
        # assumes that the data will be split

        b = matrix(NA, nrow = 2, ncol = length(splice_at) + 1)
        b[[ 1 ]]            = 1
        b[[ prod(dim(b)) ]] = length(w)

        rownames(b) = c("begin", "end")
        colnames(b) = paste0("sensor_", seq(ncol(b)))


        for(i in 1:length(splice_at)){
            m = max(which(w <= splice_at[i]))
            b[1, i + 1] = m
            b[2 , i]    = m - 1
        }

        b = data.frame(b)

    }
    if(length(splice_at) != ncol(b) - 1){
        stop("number of cut_points must be equal to the number of overlaps.")
    }

    ## Work with COLUMN INDICES, not band values. A duplicated wavelength in a
    ## sensor overlap (e.g. 975.6 sampled by both detectors) would otherwise make
    ## label-based selection `x[, values]` match columns in BOTH detectors and
    ## corrupt the join. Indices are unambiguous. See
    ## ai_reviews/DUPLICATE_BANDS_ANALYSIS.md.
    idx = lapply(b, function(y){ seq.int(y[[1]], y[[2]]) })

    ## Trim the overlap at splice_at itself: the right sensor keeps what is at or
    ## above it, the left sensor keeps what is strictly below it. splice_at is
    ## documented as the point where one detector takes over from the other, so
    ## it -- not the right sensor's first wavelength -- is the cut.
    ##
    ## Trimming the left sensor at `min(kept right)` instead used to let left-hand
    ## bands lying between splice_at and the right sensor's start survive: on the
    ## SVC reference data that kept 970.8 nm, one band the vendor deletes, so a
    ## "Remove @ 970" file came back with 983 bands against the vendor's 982.
    for(i in 1:length(splice_at) ){
        idx[[i + 1]] = idx[[i + 1]][ w[idx[[i + 1]]] >= splice_at[i] ]
        idx[[i]]     = idx[[i]][     w[idx[[i]]]     <  splice_at[i] ]
    }

    keep0   = unlist(idx, use.names = FALSE)
    sensor0 = rep(names(idx), vapply(idx, length, integer(1)))
    ord     = order(w[keep0])                 # guarantee strictly increasing output
    keep    = keep0[ord]
    sensor  = sensor0[ord]

    ## Rebuild positionally, preserving samples, metadata and sensor provenance.
    out = new_spectra(value = value(x)[ , keep, drop = FALSE],
                      bands = w[keep],
                      names = names(x),
                      meta  = meta(x))
    si = sensor_info(x)
    if( !is.null(si) ){
        attr(out, "sensor_info") = si
    }

    list("spectra" = out,
         "sensor"  = sensor,
         "overlap" = if(no_over){ NA } else { bb })
}


#' Adopt the vendor's recorded matching window into a splice_config
#'
#' When the instrument file says which window it computed its detector-matching
#' factor over (SVC's "Matching Type: Radiance @ 976 - 1010"), prefer it over the
#' auto-detected one --- it is the vendor's own statement about where the two
#' detectors are comparable. An explicit \code{config$window} always wins.
#'
#' A recorded window describes ONE junction (the one the vendor matched), so it
#' is only adopted when the config gains at exactly one junction. Otherwise the
#' same window would be imposed on crossovers hundreds of nm away, where it
#' selects no bands at all.
#'
#' @param config a splice_config
#' @param si a sensor_info data.frame, or NULL
#' @return the config, possibly with \code{window} filled in
#'
#' @keywords internal
#' @author Jose Eduardo Meireles
i_config_window_from_provenance = function(config, si){

    if( !is.null(config$window) || config$gain_type == "none" ){
        return(config)
    }
    if( !(is.numeric(config$gain_at) && length(config$gain_at) == 1) &&
        !identical(config$gain_at, "first") ){
        return(config)
    }

    win = i_match_window_from_provenance(si)
    if( is.null(win) ){
        return(config)
    }

    config$window = win
    config
}


#' Match spectra at sensor transitions
#'
#' \code{match_sensors} joins the detector segments of a full-range spectrum by
#' rescaling the sensors on either side of each splice so they line up, and
#' returns a spectrum with strictly increasing bands.
#'
#' \strong{Splice points.} \code{splice_at} gives the wavelength(s) where sensors
#' meet (the beginning of the rightmost sensor); length 1 or 2 (up to 3 sensors).
#' When \code{splice_at = NULL} (the default), \code{match_sensors} uses splice
#' points captured from the instrument file at read time (see
#' \code{\link{sensor_info}}). Two guard rails make this safe rather than
#' guess-y:
#' \itemize{
#'   \item If the bands are already strictly increasing, the vendor already
#'         spliced the data (e.g. an SVC file saved in "Remove" mode, or a
#'         standard ASD spectrum), so the spectra are returned \emph{unchanged}.
#'   \item If no splice points are available and none were supplied, an error is
#'         raised asking for them rather than silently inventing values.
#' }
#' \code{\link{guess_splice_at}} can suggest values, but visually inspect your
#' spectra before trusting them. Typical values on our own instruments were
#' SVC ~ c(990, 1900), ASD ~ c(1001, 1801).
#'
#' \strong{What this does (and does not) do.} This is a general, instrument-
#' agnostic join: one scaling factor per junction spread with a linear ramp.
#' Which junctions get that factor depends on the data. When the detectors still
#' physically overlap (raw SVC, PSR), only the \emph{first} junction is matched:
#' the far crossover of a 3-detector instrument sits near 1900 nm, inside the deep
#' water band where both detectors are at the edge of their sensitivity, and a
#' factor estimated there is noise rather than a gain. When the data carries no
#' overlap (an already-joined spectrum split by \code{splice_at}), every junction
#' is matched.
#'
#' It is \emph{not} a faithful reproduction of any single vendor's stitching
#' algorithm (SVC deletes the overlap, Spectral Evolution ramp-blends it, ASD's
#' algorithm is not recoverable). Vendor-specific splice presets that consume the
#' captured \code{\link{sensor_info}} provenance are available via \code{method}
#' (see below); on SVC data the \code{"svc"} preset lands considerably closer to
#' the vendor's own overlap-matched output than this legacy path does.
#'
#' \strong{Methods / presets.} Passing \code{method} (or a \code{config} built
#' with \code{\link{splice_config}}) selects a vendor-aware splice via the splice
#' engine (see \code{\link{splice_config}}): \code{"svc"} (graded scalar match +
#' cut), \code{"naturaspec"} (ramp blend), \code{"asd"} (additive step correction
#' on already-joined data), or the generic \code{"cut"}/\code{"ramp"}/
#' \code{"concatenate"}. \code{method = NULL} (default) or \code{"scale"} uses the
#' legacy algorithm documented above.
#'
#' @param x spectra object
#' @param splice_at splice-point wavelengths (length 1 or 2), or \code{NULL}
#'                  (default) to use the splice points recorded in
#'                  \code{\link{sensor_info}} provenance.
#' @param fixed_sensor sensor to keep fixed (legacy algorithm only). Can be 1 or 2
#'                     when matching 2 sensors. When matching 3 sensors, must be 2.
#' @param interpolate_wvl extent around each \code{splice_at} over which the
#'                        legacy splicing factors are computed. Defaults to
#'                        \code{c(5, 2)}.
#' @param method optional preset name selecting the splice engine: one of
#'               \code{"svc"}, \code{"naturaspec"}, \code{"asd"}, \code{"cut"},
#'               \code{"ramp"}, \code{"concatenate"}, or \code{"scale"} (legacy).
#' @param config optional \code{\link{splice_config}} for full control; overrides
#'               \code{method}.
#' @return spectra object with matched sensors (strictly increasing bands)
#'
#' @importFrom stats approx
#'
#' @seealso \code{\link{splice_config}}, \code{\link{sensor_info}}
#' @author Jose Eduardo Meireles and Anna Schweiger
#' @export
#'
match_sensors = function(x,
                         splice_at       = NULL,
                         fixed_sensor    = 2,
                         interpolate_wvl = c(5, 2),
                         method          = NULL,
                         config          = NULL){
    UseMethod("match_sensors")
}


#' @describeIn match_sensors Match sensor overlap regions
#' @export
match_sensors.spectra = function(x,
                                 splice_at       = NULL,
                                 fixed_sensor    = 2,
                                 interpolate_wvl = c(5, 2),
                                 method          = NULL,
                                 config          = NULL){

    ## Resolve a splice_config: an explicit `config` wins; otherwise a `method`
    ## preset (except "scale", which means the legacy algorithm below).
    if( is.null(config) && !is.null(method) && !identical(method, "scale") ){
        config = i_splice_preset(method)
    }
    use_engine = !is.null(config)

    ## Resolve splice points. When not passed explicitly, fall back to what the
    ## instrument file recorded (sensor_info provenance) rather than guessing.
    ## For the legacy path, already-increasing bands with nothing to resolve means
    ## the data is already spliced -> return unchanged. (The engine's asd/
    ## concatenate presets DO operate on already-increasing data, so we only
    ## short-circuit for the legacy path here.)
    if( is.null(splice_at) ){
        if( i_is_increasing(bands(x)) && !use_engine ){
            message("Bands are already strictly increasing; the sensor overlap ",
                    "appears to have been spliced already. Returning spectra unchanged.")
            return(x)
        }

        splice_at = i_splice_from_provenance(sensor_info(x))

        if( is.null(splice_at) ){
            stop("No `splice_at` supplied and no splice points found in the sensor ",
                 "provenance (see sensor_info(x)). Provide `splice_at` explicitly, ",
                 "e.g. from guess_splice_at(x).", call. = FALSE)
        }

        message("Using splice point(s) from sensor provenance: ",
                paste(splice_at, collapse = ", "))
    }

    ## Engine path (vendor presets / custom config)
    if( use_engine ){
        config = i_config_window_from_provenance(config, sensor_info(x))
        return(i_splice(x, splice_at, config))
    }

    ## ----------------------------------------------------------------------
    ## Legacy "scale" algorithm (unchanged): whole-sensor scalar + linear ramp.
    ## ----------------------------------------------------------------------
    splice_at = unlist(splice_at)

    ## Guard the fixed sensor: an out-of-range value used to fall through to
    ## `names(y[match(fixed_sensor, y)])` returning NULL and dying with the
    ## opaque "argument is of length zero".
    if( length(fixed_sensor) != 1 || ! fixed_sensor %in% seq_len(length(splice_at) + 1L) ){
        stop("`fixed_sensor` must be one of 1:", length(splice_at) + 1L,
             " for ", length(splice_at) + 1L, " sensors.", call. = FALSE)
    }

    ## Three sensors: the legacy ramp only makes sense with the middle detector
    ## held fixed. That was already forced, but silently -- say so instead of
    ## quietly discarding what the caller asked for.
    if( length(splice_at) == 2 && fixed_sensor != 2 ){
        warning("`fixed_sensor` must be 2 when matching 3 sensors with the legacy ",
                "algorithm; ignoring fixed_sensor = ", fixed_sensor, ".",
                call. = FALSE)
        fixed_sensor = 2
    }

    y = i_trim_sensor_overlap(x = x, splice_at = splice_at)
    x = y$spectra              # trimmed spectra, strictly increasing bands
    w = bands(x)
    s = split(w, y$sensor)

    interpolate_wvl = rep(interpolate_wvl, length.out = length(splice_at))

    ## Pick bands by sensor to computer factors
    wl_picks = lapply(seq_along(splice_at), function(z){
        low   = splice_at[z] - interpolate_wvl[z]
        high  = splice_at[z] + interpolate_wvl[z]
        left  = s[[ z ]][ s[[ z ]]         >= low ]
        right = s[[z + 1L ]][ s[[z + 1L ]] <= high ]

        # solve issues if any of the picks are empty
        if(length(left)  == 0){
            left = max(s[[ z ]])
        }
        if(length(right) == 0){
            right = min(s[[ z + 1L ]])
        }

        list("left"  = left, "right" = right)
    })
    names(wl_picks) = splice_at

    ## compute splicing factors
    splice_factors = lapply(seq_along(wl_picks), function(z){
        y = setNames(c(z, z + 1), c("left", "right"))
        m = names(y[match(fixed_sensor, y)])

        if(m == "right"){
            fixed  = wl_picks[[z]]$right
            scaled = wl_picks[[z]]$left
        } else {
            fixed  = wl_picks[[z]]$left
            scaled = wl_picks[[z]]$right
        }

        rowMeans(value(x[ ,  fixed, simplify = FALSE])) /
        rowMeans(value(x[ , scaled, simplify = FALSE]))

    })

    ## Compute the factor matrices
    ## These functions need to be empirically derived. Current implementation
    ## is just a hack and should not be used in production code

    s[fixed_sensor] = NULL

    factor_mat = lapply(seq_along(splice_factors), function(z){
        bds = s[[z]]
        fac = splice_factors[[z]]

        y   = setNames(c(z, z + 1), c("left", "right"))
        m   = names(y[match(fixed_sensor, y)])

        if(m == "right"){
            r =  sapply(fac, function(q){
                approx(x = range(bds), y = c(1, q), xout = bds)$y
            })
        } else {
            r =  sapply(fac, function(q){
                approx(x = range(bds), y = c(q, 1), xout = bds)$y
            })
        }
        r
    })


    ## Which junctions actually get their factor applied.
    ##
    ## Data with a REAL detector overlap gets only the first junction matched.
    ## This looks like a bug -- the far sensor is left alone -- and 0.0.20 briefly
    ## "fixed" it by looping over every junction. That was the wrong call: on
    ## overlapping instruments the far crossover sits at ~1900 nm, inside the deep
    ## water band at the edge of both detectors' sensitivity, and the factor
    ## estimated there is noise (0.63-1.50 across the SVC reference set). Applying
    ## it ramped a 50% error across a whole detector. Validated against the vendor
    ## overlap-matched files, matching only the first junction is right: SVC
    ## itself removes both overlaps but matches only the VNIR/SWIR1 one.
    ##
    ## Data with NO overlap (a joined ASD spectrum, split by splice_at) has no
    ## such window problem, so every junction is matched, as before.
    no_overlap = length(y$overlap) == 1L && all(is.na(y$overlap))

    if(no_overlap || length(factor_mat) == 1){
        iter = seq_along(factor_mat)
    } else {
        iter = 1
    }

    for(i in iter){
        x[ , s[[i]]] = value(x[ , s[[i]] ] ) * t( factor_mat[[i]] )
    }

    x
}
