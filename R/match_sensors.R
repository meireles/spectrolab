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
#' Joins the detector segments with no magnitude correction at all: each sensor
#' keeps the bands between its neighbouring splice points. This is the bare cut
#' that \code{\link{match_sensors}} builds on, and it is useful on its own to see
#' what the gain match and the crossfade actually change.
#'
#' @param x spectra object
#' @param splice_at bands where to splice sensors. suggests where the
#'                  beginning of sensors 2 and 3 should be.
#' @return list with the joined \code{spectra}, the \code{sensor} each output band
#'         came from, and the detected \code{overlap} bounds (NA when the data was
#'         already joined)
#'
#' @keywords internal
#' @author Jose Eduardo Meireles
i_trim_sensor_overlap = function(x, splice_at){

    w         = bands(x)
    splice_at = sort(splice_at)
    seg       = i_sensor_segments(w, splice_at)

    if(length(splice_at) != length(seg$cols) - 1){
        stop("number of cut_points must be equal to the number of overlaps.")
    }

    kept    = i_splice_keep(w, seg$cols, splice_at)
    keep0   = unlist(kept, use.names = FALSE)
    sensor0 = rep(names(kept), vapply(kept, length, integer(1)))

    ord    = order(w[keep0])                  # guarantee strictly increasing output
    keep   = keep0[ord]
    sensor = sensor0[ord]

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
         "overlap" = if(seg$overlap){ i_find_sensor_overlap_bounds(w) } else { NA })
}



#' Match spectra at sensor transitions
#'
#' \code{match_sensors} joins the detector segments of a full-range spectrum into
#' one continuous, strictly increasing spectrum: it magnitude-matches the outer
#' detector(s) to the inner one, deletes the overlapping measurements, and
#' crossfades the seam so no step is left where the detectors meet.
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
#' \strong{What it does.} One algorithm, in three steps:
#' \enumerate{
#'   \item \emph{Gain match.} A single factor per junction is estimated over the
#'         crossover window --- the detectors' physical overlap, inset off both
#'         ends where their response rolls off, or a small interval around
#'         \code{splice_at} (\code{interpolate_wvl}) when the data carries no
#'         overlap. The factor is applied \emph{tapered}: full at the junction,
#'         fading to no correction at the far end of the corrected detector, so a
#'         detector's well-calibrated end is left alone. Its amplitude is solved
#'         for, not read off as a ratio, because the taper is already below full
#'         strength inside the window.
#'   \item \emph{Cut.} The overlapping measurements are deleted at
#'         \code{splice_at}, exactly as the instrument does.
#'   \item \emph{Crossfade.} Across the crossover window the output is a weighted
#'         mix of both detectors, so the seam carries no step.
#' }
#'
#' Which junctions get matched depends on the data. When the detectors still
#' physically overlap (raw SVC, PSR), only the \emph{first} junction is: the far
#' crossover of a 3-detector instrument sits near 1900 nm, inside the deep water
#' band where both detectors are at the edge of their sensitivity, and a factor
#' estimated there is noise rather than a gain --- applying it ramps a large error
#' across a whole detector. When the data carries no overlap (an already-joined
#' spectrum split by \code{splice_at}), every junction is matched.
#'
#' \strong{How close is it?} \code{inst/extdata/svc_raw_and_overlap_matched_serbin}
#' ships 14 scans twice, raw and reprocessed by SVC's own software. Against that
#' reference the RMSE of \code{match_sensors} is 0.00013 reflectance units, versus
#' 0.00771 for the bare join with no matching. The taper shape and the crossfade
#' were both derived from that comparison. It is still \emph{not} a replay of
#' vendor firmware: SVC applies its match to the reference and target radiance
#' with slightly different factors, so it does not cancel out of the single
#' reflectance column spectrolab reads, and no comparable reference exists for ASD
#' or Spectral Evolution --- for those instruments this is spectrolab's own,
#' documented correction.
#'
#' @param x spectra object
#' @param splice_at splice-point wavelengths (length 1 or 2), or \code{NULL}
#'                  (default) to use the splice points recorded in
#'                  \code{\link{sensor_info}} provenance.
#' @param fixed_sensor sensor held fixed; the others are corrected toward it. Can
#'                     be 1 or 2 when matching 2 sensors. When matching 3 sensors,
#'                     must be 2.
#' @param interpolate_wvl half width (nm) of the window used to estimate the gain
#'                        \emph{when the detectors do not overlap}. Recycled over
#'                        junctions; defaults to \code{c(5, 2)}. Ignored when there
#'                        is a physical overlap, which is measured directly.
#' @return spectra object with matched sensors (strictly increasing bands)
#'
#' @seealso \code{\link{guess_splice_at}}, \code{\link{sensor_info}}
#' @author Jose Eduardo Meireles and Anna Schweiger
#' @export
#'
#' @examples
#' library(spectrolab)
#' dir_path = system.file("extdata/svc_raw_and_overlap_matched_serbin/SVC_Files",
#'                        package = "spectrolab")
#' raw = read_spectra(dir_path, format = "sig")
#' matched = match_sensors(raw, splice_at = c(970, 1901))
match_sensors = function(x,
                         splice_at       = NULL,
                         fixed_sensor    = 2,
                         interpolate_wvl = c(5, 2)){
    UseMethod("match_sensors")
}


#' @describeIn match_sensors Match sensor overlap regions
#' @export
match_sensors.spectra = function(x,
                                 splice_at       = NULL,
                                 fixed_sensor    = 2,
                                 interpolate_wvl = c(5, 2)){

    ## Resolve splice points. When not passed explicitly, fall back to what the
    ## instrument file recorded (sensor_info provenance) rather than guessing.
    ## Already-increasing bands with nothing to resolve means the data is already
    ## spliced -> return it unchanged.
    if( is.null(splice_at) ){
        if( i_is_increasing(bands(x)) ){
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

    splice_at = sort(unlist(splice_at))
    n_sensors = length(splice_at) + 1L

    ## Guard the fixed sensor: an out-of-range value used to die deep inside with
    ## the opaque "argument is of length zero".
    if( length(fixed_sensor) != 1 || ! fixed_sensor %in% seq_len(n_sensors) ){
        stop("`fixed_sensor` must be one of 1:", n_sensors,
             " for ", n_sensors, " sensors.", call. = FALSE)
    }

    ## Three sensors: only the middle one can be held fixed. Correcting detector 2
    ## toward detector 1 would leave detector 3 behind, opening a step at the far
    ## junction that was not there to begin with. That was already forced, but
    ## silently -- say so instead of quietly discarding what the caller asked for.
    if( n_sensors == 3 && fixed_sensor != 2 ){
        warning("`fixed_sensor` must be 2 when matching 3 sensors; ignoring ",
                "fixed_sensor = ", fixed_sensor, ".", call. = FALSE)
        fixed_sensor = 2
    }

    ## When the instrument recorded the window it matched over (SVC's "Matching
    ## Type: Radiance @ 976 - 1010"), use it -- but only if we cannot see an
    ## overlap ourselves. A visible overlap is the better estimate of where the
    ## two detectors are comparable, and measurably so: on the vendor reference
    ## set the detected window beats the recorded one (RMSE 0.00013 vs 0.00032),
    ## because the recorded zone was chosen in radiance, not reflectance.
    i_splice(x,
             splice_at       = splice_at,
             fixed_sensor    = fixed_sensor,
             interpolate_wvl = interpolate_wvl,
             window          = i_match_window_from_provenance(sensor_info(x)))
}
