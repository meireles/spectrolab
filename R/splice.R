################################################################################
# Detector splice engine  --- the private implementation of match_sensors()
#
# ONE ALGORITHM, NOT A MENU
# -------------------------
# A full-range spectroradiometer stitches two or three detector segments into one
# spectrum, and the segments never agree perfectly where they meet: the outer
# detector reads a slightly different magnitude than the inner one, and the
# disagreement grows toward the far end of the outer detector, where its response
# is rolling off. Joining them therefore takes three steps, and there is one
# defensible way to do each:
#
#   1. DETECT the segments and the junction(s) between them.
#   2. GAIN-MATCH the outer detector to the inner one, tapered so the correction
#      is full at the junction and vanishes at the far end of the detector.
#   3. JOIN: cut the overlap at the splice point, and crossfade the two detectors
#      across the crossover window so the seam carries no step.
#
# WHY THESE CHOICES (evidence, not taste)
# ---------------------------------------
# inst/extdata/svc_raw_and_overlap_matched_serbin/ ships the same 14 scans twice:
# raw ("[Overlap: Preserve, Matching Type: None]") and reprocessed by SVC's own
# software ("[Overlap: Remove @ 970,1901, Matching Type: Radiance @ 976 - 1010]").
# Comparing the two, band by band, says exactly what a good splice looks like:
#
#   * The correction to detector 1 is TAPERED, not flat: the ratio of the vendor's
#     output to its input runs smoothly from 1.000 at 338 nm to ~0.94-0.97 at the
#     junction.
#   * The taper is CONVEX in wavelength, not linear. Fitting (ratio - 1) to
#     u^power, with u the fraction of the detector's own wavelength span, gives
#     power = 1.308 +/- 0.005 across all 14 scans (estimated on the vendor's
#     reference-radiance column, which is free of reflectance rounding). Hence
#     i_splice()'s taper_power = 1.3.
#   * The taper spans the DETECTOR, ending at its last band, not at the splice
#     point --- which is why the gain amplitude has to be solved for rather than
#     read off as a ratio of window means: in the matching window the taper is
#     already at ~96% of full, so a raw ratio is biased low by ~0.2%. See
#     i_solve_gain().
#   * The seam is CROSSFADED, not cut: the vendor's output between the splice
#     point and the top of its matching zone is a mix of both detectors, weighted
#     toward the (matched) inner detector at the low end and reaching the outer
#     detector alone at the top of the zone. That is what makes a vendor splice
#     look seamless where a bare cut leaves a visible step.
#   * Only the FIRST junction is gain-matched when the detectors physically
#     overlap. See i_splice() for why the far junction must be left alone.
#
# Against those 14 scans this algorithm lands at RMSE 0.00013 in reflectance
# units (bare cut: 0.00771), with the detector-2 and detector-3 residual at the
# level of the files' own two-decimal rounding.
#
# HONESTY
# -------
# This is not a replay of vendor firmware and does not claim to be. SVC applies
# its match to the reference AND target radiance with slightly different factors,
# so the correction does not cancel out of the single reflectance column
# spectrolab reads; we reproduce the removal structure exactly and apply a
# general, documented gain. ASD's own algorithm is not recoverable at all --- for
# already-joined data this engine applies the same tapered gain, which is
# spectrolab's choice, not ASD's.
################################################################################


#' Sensor segments of a spectrum
#'
#' \code{i_sensor_segments} returns the column indices of each detector segment.
#' When the bands are non-monotonic the segments are read off the wavelength
#' reversals (a physical detector overlap: the same wavelengths are measured
#' twice). When they are monotonic the data is already joined, so
#' \code{splice_at} itself defines the segment boundaries.
#'
#' @param w band vector
#' @param splice_at junction wavelength(s), sorted
#' @return list with \code{cols} (list of column-index vectors, one per sensor)
#'         and \code{overlap} (TRUE when a physical overlap was detected)
#'
#' @keywords internal
#' @author Jose Eduardo Meireles
i_sensor_segments = function(w, splice_at){

    bounds  = i_find_sensor_overlap_bounds(w)
    overlap = ncol(bounds) > 1L

    if(overlap){
        cols = lapply(bounds, function(y){ seq.int(y[[1]], y[[2]]) })
    } else {
        ## Already joined: cut the single run of bands at each splice point. Each
        ## junction must fall strictly inside the data, or the "segment" it is
        ## supposed to create is empty.
        inside = vapply(splice_at, function(s){ any(w >= s) && any(w < s) }, logical(1))
        if( !all(inside) ){
            stop("splice_at value(s) ", paste(splice_at[!inside], collapse = ", "),
                 " fall outside the band range (", min(w), " to ", max(w),
                 "), so they do not split the spectrum.", call. = FALSE)
        }
        begin = c(1L, vapply(splice_at, function(s){ min(which(w >= s)) }, integer(1)))
        end   = c(begin[-1] - 1L, length(w))
        cols  = Map(seq.int, begin, end)
    }

    names(cols) = paste0("sensor_", seq_along(cols))
    list(cols = cols, overlap = overlap)
}


#' Inset a window by a fraction of its width on each side
#'
#' The outermost wavelengths of a detector overlap are where both sensors'
#' response is rolling off, so a gain factor estimated across the full overlap is
#' biased by its own worst data. Insetting reproduces what vendors do when they
#' record a matching zone narrower than the overlap it sits in: SVC overlaps over
#' 971.8--1016.6 nm but matches over 976--1010 nm, which a 10\% inset recovers
#' almost exactly.
#'
#' Degenerate cases (zero-width, or an inset that would empty the window) return
#' the window unchanged rather than an invalid one.
#'
#' @param win numeric c(lo, hi)
#' @param inset fraction to drop from each end
#' @return numeric c(lo, hi)
#' @keywords internal
i_inset_window = function(win, inset){
    if(is.null(inset) || !is.finite(inset) || inset <= 0){
        return(win)
    }
    pad = (win[2] - win[1]) * inset
    if(!is.finite(pad) || pad <= 0){
        return(win)
    }
    c(win[1] + pad, win[2] - pad)
}


#' Crossover window for one junction
#'
#' With a physical overlap the window is the overlap itself, inset off both ends.
#' Without one there is nothing to measure, so the window is a small interval
#' around the splice point (\code{half_width}, i.e. \code{interpolate_wvl}), or an
#' explicit window recorded by the instrument.
#'
#' @param w band vector
#' @param left,right column indices of the two segments
#' @param splice_at this junction's wavelength
#' @param overlap logical, is there a physical overlap?
#' @param half_width half width used when there is no overlap
#' @param inset fraction of the overlap dropped from each end
#' @param window explicit window, or NULL
#' @return numeric c(lo, hi)
#' @keywords internal
i_splice_window = function(w, left, right, splice_at, overlap,
                           half_width, inset, window = NULL){
    if(overlap){
        lo = max(min(w[left]), min(w[right]))
        hi = min(max(w[left]), max(w[right]))
        return(i_inset_window(c(lo, hi), inset))
    }
    ## A recorded window is only usable if it actually brackets this junction --
    ## it describes one crossover, and imposing it on another one hundreds of nm
    ## away would select no bands at all and silently skip the match.
    if( !is.null(window) && window[1] <= splice_at && window[2] >= splice_at ){
        return(window)
    }
    c(splice_at - half_width, splice_at + half_width)
}


#' Taper weights across a detector segment
#'
#' The correction is full (weight 1) at the junction end of the segment and
#' vanishes (weight 0) at the detector's far end, following \code{u^power} where
#' \code{u} is the fraction of the segment's own wavelength span. \code{power} is
#' 1 for a straight ramp; the SVC reference set puts it at 1.31 (see the file
#' header).
#'
#' The span is the segment's \emph{whole} extent, including the part that a cut
#' will later discard --- the taper is a property of the detector, not of where we
#' happen to cut it.
#'
#' @param wl wavelengths of the scaled segment
#' @param junction_end "high" when the junction is at the segment's long-wavelength
#'                     end (the segment is left of the junction), "low" otherwise
#' @param power taper exponent
#' @return numeric vector of weights in [0, 1], same length as \code{wl}
#' @keywords internal
i_taper_weights = function(wl, junction_end, power){
    rng  = range(wl)
    span = rng[2] - rng[1]
    if( !is.finite(span) || span <= 0 ){
        return(rep(1, length(wl)))            # single-band segment: no taper to do
    }
    u = if(junction_end == "high"){ (wl - rng[1]) / span } else { (rng[2] - wl) / span }
    pmin(pmax(u, 0), 1) ^ power
}


#' Solve the gain amplitude that makes a tapered correction match the window
#'
#' The correction applied to the scaled segment is \code{1 + (F - 1) * t(w)}, with
#' \code{t} the taper weights. Estimating \code{F} as the plain ratio of window
#' means would be wrong, because inside the window the taper is already below 1
#' (the window sits near, but not at, the junction end of the detector): the
#' correction actually delivered there would fall short of the ratio it was meant
#' to reproduce. Solving
#'
#'   \code{mean(scaled) + (F - 1) * mean(scaled * t) = mean(fixed)}
#'
#' removes that bias. On the SVC reference set it accounts for a systematic
#' -0.2\% error in the factor, and cuts the detector-1 residual by ~4x.
#'
#' @param fixed n x k matrix of fixed-side values in the window
#' @param scaled n x m matrix of scaled-side values in the window
#' @param t_window taper weights at the scaled side's window bands (length m)
#' @return numeric vector of length n --- the gain at the junction, per sample
#' @keywords internal
i_solve_gain = function(fixed, scaled, t_window){
    m_fixed  = rowMeans(fixed,  na.rm = TRUE)
    m_scaled = rowMeans(scaled, na.rm = TRUE)
    m_taper  = rowMeans(sweep(scaled, 2, t_window, "*"), na.rm = TRUE)
    1 + (m_fixed - m_scaled) / m_taper
}


#' Is an estimated gain plausible?
#'
#' \code{range} states what a detector-matching gain can possibly be. A factor
#' outside it does not mean "correct by the bound" --- it means the window the
#' factor was estimated from carries no usable signal (the classic case is a
#' junction sitting inside a deep water band). Callers skip the junction instead:
#' applying a bound would apply a known-bad correction silently.
#'
#' @param fac estimated factor (may be a vector)
#' @param range numeric c(min, max), or NULL for no check
#' @return logical, same length as \code{fac}
#' @keywords internal
i_gain_is_plausible = function(fac, range){
    ok = is.finite(fac) & fac > 0
    if(is.null(range)){
        return(ok)
    }
    ok & fac >= range[1] & fac <= range[2]
}


#' Warn that a junction was left uncorrected because its gain was implausible
#'
#' @param n_rejected number of samples whose factor failed the check
#' @param n_total number of samples
#' @param w the junction wavelength
#' @param range the plausible range
#' @return invisible NULL
#' @keywords internal
i_warn_rejected_gain = function(n_rejected, n_total, w, range){
    if(n_rejected == 0){
        return(invisible(NULL))
    }
    why = if(is.null(range)){
        "the estimated factor was not a usable number"
    } else {
        paste0("the estimated factor fell outside the plausible range [",
               range[1], ", ", range[2], "]")
    }

    warning("Detector matching skipped at ", w, " nm for ", n_rejected, " of ",
            n_total, " sample(s): ", why, ", which means the matching window ",
            "carries too little signal to estimate a gain. Those sensors were ",
            "joined without a magnitude correction.", call. = FALSE)
    invisible(NULL)
}


#' Splice detector segments into one strictly increasing spectrum
#'
#' \code{i_splice} is the engine behind \code{\link{match_sensors}}: detect the
#' segments, gain-match the outer detector(s) with a tapered factor, then cut the
#' overlap and crossfade the seam. See the header of \code{R/splice.R} for the
#' evidence behind every default.
#'
#' \strong{Which junctions get matched.} When the detectors physically overlap,
#' only the \emph{first} junction is. The far crossover of a 3-detector
#' instrument sits near 1900 nm, inside the deep water band where both detectors
#' are at the edge of their sensitivity: on the SVC reference set the ratio
#' measured there is 0.63--0.84 over a handful of bands averaging 0.04
#' reflectance, i.e. noise, and applying it ramps a large error across a whole
#' detector (RMSE against the vendor goes from 0.00013 to 0.02263). The vendor
#' agrees --- it removes both overlaps but matches only the near one. When there
#' is no overlap (already-joined data split by \code{splice_at}) there is no such
#' window problem and every junction is matched.
#'
#' @param x spectra object
#' @param splice_at junction wavelength(s), sorted
#' @param fixed_sensor index of the sensor held fixed; the others are corrected
#'                     toward it
#' @param interpolate_wvl half width (nm) of the window used to estimate the gain
#'                        when the detectors do not overlap. Recycled over
#'                        junctions
#' @param window explicit crossover window \code{c(lo, hi)}, used only when there
#'               is no overlap to measure one from
#' @param taper_power exponent of the taper (1 = straight ramp)
#' @param window_inset fraction of a detected overlap dropped from each end before
#'                     the gain is estimated
#' @param gain_range plausible range for the gain; outside it the junction is left
#'                   uncorrected, with a warning
#' @return spectra object with strictly increasing bands
#'
#' @keywords internal
#' @importFrom stats approx
#' @author Jose Eduardo Meireles
i_splice = function(x,
                    splice_at,
                    fixed_sensor    = 2,
                    interpolate_wvl = c(5, 2),
                    window          = NULL,
                    taper_power     = 1.3,
                    window_inset    = 0.10,
                    gain_range      = c(1/3, 3)){

    w    = bands(x)
    v    = value(x)
    seg  = i_sensor_segments(w, splice_at)
    cols = seg$cols
    nj   = length(cols) - 1L

    if(length(splice_at) != nj){
        stop("number of splice points must be one less than the number of sensors.",
             call. = FALSE)
    }

    ## Junctions that get a gain match -- see the roxygen block above.
    do_gain = if(seg$overlap && nj > 1L){ 1L } else { seq_len(nj) }

    half_width = rep(interpolate_wvl, length.out = nj)
    win_of     = vector("list", nj)
    matched    = matrix(FALSE, nrow = nrow(v), ncol = nj)

    for(j in do_gain){

        left  = cols[[j]]
        right = cols[[j + 1L]]
        ## A recorded window describes the ONE crossover the instrument matched,
        ## so it is only ever offered to the first junction.
        win   = i_splice_window(w, left, right, splice_at[j], seg$overlap,
                                half_width[j], window_inset,
                                if(j == 1L){ window } else { NULL })
        win_of[[j]] = win

        in_win = function(k){ k[ w[k] >= win[1] & w[k] <= win[2] ] }
        lw     = in_win(left)
        rw     = in_win(right)
        if(length(lw) == 0 || length(rw) == 0){
            next                       # nothing to compare across this junction
        }

        ## The side nearer `fixed_sensor` is held fixed; the other is corrected.
        fixed_is_left = fixed_sensor <= j
        if(fixed_is_left){
            fixed_win = lw; scaled_win = rw; scaled = right; junction_end = "low"
        } else {
            fixed_win = rw; scaled_win = lw; scaled = left;  junction_end = "high"
        }

        t_seg = i_taper_weights(w[scaled], junction_end, taper_power)
        t_win = t_seg[ match(scaled_win, scaled) ]

        fac = i_solve_gain(fixed  = v[ , fixed_win,  drop = FALSE],
                           scaled = v[ , scaled_win, drop = FALSE],
                           t_window = t_win)
        ok  = i_gain_is_plausible(fac, gain_range)

        adj       = 1 + outer(fac - 1, t_seg)
        adj[!ok, ] = 1
        v[ , scaled] = v[ , scaled, drop = FALSE] * adj

        matched[ , j] = ok
        i_warn_rejected_gain(sum(!ok), length(ok), splice_at[j], gain_range)
    }

    ## --- join: keep each segment between its neighbouring splice points -------
    keep   = unlist(i_splice_keep(w, cols, splice_at), use.names = FALSE)
    ord    = order(w[keep])
    keep   = keep[ord]
    w_out  = w[keep]
    v_out  = v[ , keep, drop = FALSE]

    ## --- crossfade the seam ---------------------------------------------------
    ## Only where both detectors measured the same wavelengths AND the gain match
    ## succeeded: blending detectors that were never made comparable would mix two
    ## different magnitudes into a smooth-looking but wrong curve.
    if(seg$overlap){
        for(j in do_gain){
            win = win_of[[j]]
            if(is.null(win) || !any(matched[ , j])){
                next
            }
            v_out = i_splice_crossfade(w, v, cols[[j]], cols[[j + 1L]],
                                       w_out, v_out, win, matched[ , j])
        }
    }

    out = new_spectra(value = v_out,
                      bands = w_out,
                      names = names(x),
                      meta  = meta(x))

    si = sensor_info(x)
    if( !is.null(si) ){
        attr(out, "sensor_info") = si
    }
    out
}


#' Column indices kept by the join
#'
#' Each sensor keeps the bands between its neighbouring splice points: the left
#' sensor of a junction keeps \code{w < splice_at}, the right one \code{w >=
#' splice_at}. \code{splice_at} is \emph{the} cut --- trimming the left sensor at
#' the right sensor's first band instead would keep bands the vendor deletes (on
#' the SVC reference that is 970.8 nm, one band, and an output grid of 983 bands
#' against the vendor's 982).
#'
#' Selection is by COLUMN INDEX, never by band label: a duplicated wavelength in
#' an overlap would make label selection match columns in both detectors and
#' corrupt the join.
#'
#' @param w band vector
#' @param cols list of per-sensor column indices
#' @param splice_at junction wavelengths
#' @return list of kept column indices, one element per sensor, in sensor order
#' @keywords internal
i_splice_keep = function(w, cols, splice_at){
    n = length(cols)
    out = lapply(seq_len(n), function(k){
        idx = cols[[k]]
        if(k > 1L){ idx = idx[ w[idx] >= splice_at[k - 1L] ] }
        if(k < n) { idx = idx[ w[idx] <  splice_at[k] ] }
        idx
    })
    names(out) = names(cols)
    out
}


#' Crossfade two detectors across their crossover window
#'
#' Everywhere the two detectors both measured, the output becomes a weighted mix
#' of the two, each interpolated onto the output wavelengths: the (matched) left
#' detector alone below the window, a linear crossfade across it, the right
#' detector alone above it. Outside the physical overlap nothing is touched, and
#' at either edge the mix reduces to the detector that owns those bands, so the
#' result is continuous by construction.
#'
#' This is what removes the seam. A bare cut leaves the last band of one detector
#' next to the first band of the other, and even after a gain match those two
#' readings differ --- on the SVC reference set the cut leaves a 10\% step at the
#' junction and this leaves 0.2\%, which is what the vendor's own output has.
#'
#' Giving the left detector full weight \emph{below} the window is deliberate and
#' is what the vendor does: the bottom edge of the right detector is where its
#' response is still rolling on, and is exactly the data the window was inset to
#' exclude from the gain estimate. Trusting it for the output while refusing it
#' for the factor would be incoherent.
#'
#' @param w original band vector (may be non-monotonic)
#' @param v original value matrix, gain-corrected
#' @param left,right column indices of the two segments
#' @param w_out output band vector
#' @param v_out output value matrix (modified and returned)
#' @param win crossover window c(lo, hi)
#' @param samples logical vector: which samples to blend
#' @return \code{v_out} with the overlapped columns replaced
#' @keywords internal
#' @importFrom stats approx
i_splice_crossfade = function(w, v, left, right, w_out, v_out, win, samples){

    ## Only where BOTH detectors have data: beyond the physical overlap an
    ## interpolation would be a flat extrapolation of a detector's last reading.
    span = c(max(min(w[left]), min(w[right])),
             min(max(w[left]), max(w[right])))

    sel = which(w_out >= span[1] & w_out <= span[2])
    if(length(sel) == 0 || win[2] <= win[1]){
        return(v_out)
    }

    wl = w_out[sel]
    a  = pmin(pmax((win[2] - wl) / (win[2] - win[1]), 0), 1)   # weight on the left

    for(i in which(samples)){
        l = stats::approx(w[left],  v[i, left],  xout = wl, rule = 2)$y
        r = stats::approx(w[right], v[i, right], xout = wl, rule = 2)$y
        v_out[i, sel] = a * l + (1 - a) * r
    }
    v_out
}
