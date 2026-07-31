################################################################################
# Detector-splice engine  (Phase 2 of the match_sensors redesign)
#
# WHY A GENERAL ENGINE
# --------------------
# The three instrument families spectrolab supports join their detector segments
# in structurally different ways (see ai_reviews/MATCH_SENSORS_PLAN.md and
# ai_reviews/DETECTOR_SPLICE_REFERENCE.md):
#
#   * SVC  : DELETES the overlapping points (a clean cut), optionally after a
#            single graded scalar match of one detector to its neighbour.
#   * Spectral Evolution / NaturaSpec : linear-RAMP blend across an overlap
#            window, with a per-junction additive-vs-multiplicative gain choice.
#   * ASD  : ships an already-joined spectrum; the honest options are to leave it
#            alone (concatenate) or apply an optional magnitude match across the
#            stored splice wavelengths (no vendor algorithm is recoverable).
#
# Rather than three ad-hoc code paths, these are all points in ONE small
# parameter space. This engine decomposes a splice into three orthogonal,
# independently-parameterized stages -- exactly how SVC's own software models it
# (detect / match / remove as separate switches):
#
#     detect junctions  ->  (optional) GAIN-match one side  ->  JOIN
#
# Vendor behaviours are then just named presets (see splice_config / the presets
# below) over this one core.
#
# HONESTY (carried from the reference's confidence discipline):
#   * The SVC "Matching" is applied to reference AND target *radiance* with
#     slightly different factors, so it does NOT cancel out of the reflectance
#     ratio; spectrolab reads a single value column, so we can reproduce the
#     REMOVAL exactly but we do NOT claim bit-exact reproduction of vendor
#     reflectance. The gain here is a general, documented correction, not a
#     byte-for-byte replay of SVC firmware.
#   * ASD's splice algorithm is unknown; the ASD gain option is spectrolab's own
#     choice (an additive offset, inspired by -- not copied from -- the GPL-3
#     package prospectr's spliceCorrection), NOT an ASD vendor algorithm.
#   * The Spectral Evolution ramp is a single-source (DERIVED) reconstruction.
#
# The legacy match_sensors behaviour (a whole-sensor scalar with a linear approx
# ramp) is preserved unchanged as method = "scale" in R/match_sensors.R; this
# engine provides the newer cut / ramp / concatenate joins and the gain
# estimators.
################################################################################


########################################
# Configuration
########################################

#' Configure a detector-splice operation
#'
#' \code{splice_config} builds the parameter set that drives the splice engine
#' used by \code{\link{match_sensors}}. Most users never call it directly --- they
#' pass a preset name to \code{match_sensors(x, method = ...)} --- but it is the
#' full-control interface for non-standard instruments.
#'
#' A splice is decomposed into three stages: detect the junctions, optionally
#' \emph{gain}-match one side to the other over a crossover window, then
#' \emph{join} the segments into a single strictly-increasing spectrum.
#'
#' @param gain_type How to magnitude-match the two sides of a junction:
#'   \describe{
#'     \item{"none"}{no matching (default).}
#'     \item{"multiplicative"}{scale one side by a single factor
#'       \code{mean(fixed window) / mean(scaled window)} (SVC-style).}
#'     \item{"additive"}{shift one side by an offset (see \code{gain_estimator}).}
#'     \item{"ssd"}{choose additive vs multiplicative per junction, keeping
#'       whichever makes the smaller adjustment (Spectral-Evolution-style).}
#'   }
#' @param gain_estimator For \code{gain_type = "additive"}, how to estimate the
#'   offset: \code{"mean_diff"} (difference of window means) or
#'   \code{"linear_extrap"} (fit a short local line on the fixed side, extrapolate
#'   to the splice wavelength, offset the other side to meet it --- the default,
#'   inspired by prospectr's spliceCorrection; slope-aware).
#' @param reference Which side of each junction is held fixed while the other is
#'   adjusted: \code{"right"}, \code{"left"}, or \code{"middle"} (fix the central
#'   detector and correct the outer ones --- resolves per junction for 3 sensors).
#' @param window Crossover window \code{c(lo, hi)} (nm) over which factors are
#'   computed and, for a ramp, the blend runs. \code{NULL} (default) auto-detects
#'   it from the sensor overlap, inset by \code{window_inset}.
#' @param window_inset Fraction of the detected overlap to drop from \emph{each}
#'   end before computing a gain factor (default 0.10). The extreme edges of an
#'   overlap are where both detectors' response is rolling off, so including them
#'   biases the factor; vendors inset their own matching zones for the same
#'   reason (SVC overlaps at 971.8--1016.6 nm but matches over 976--1010 nm,
#'   which a 10\% inset reproduces almost exactly). Ignored when \code{window} is
#'   given explicitly, and never applied to a ramp blend.
#' @param gain_at Which junctions get a gain match: \code{"all"} (default),
#'   \code{"first"}, or an integer vector of junction indices. Not every junction
#'   should be matched --- SVC, for one, matches only the VNIR/SWIR1 crossover and
#'   simply removes the overlap at the SWIR1/SWIR2 one, where both detectors sit
#'   in the 1900 nm water band at the edge of their sensitivity and any ratio
#'   between them is noise. See \code{\link{match_sensors}}.
#' @param clamp For a multiplicative or \code{"ssd"} gain, the plausible range
#'   \code{c(min, max)} for the factor (e.g. SVC's \code{c(0.8, 1.2)}).
#'   \code{NULL} = no check. A factor outside the range means the estimate itself
#'   is untrustworthy, so the junction is left \strong{uncorrected} with a
#'   warning; the bound is deliberately not used to floor the factor, which would
#'   apply a known-bad correction silently.
#' @param graded For a multiplicative gain, taper the factor across the scaled
#'   segment (full at the junction, fading to 1 at the far end) rather than
#'   applying it flat. SVC does this; defaults to FALSE.
#' @param join How to combine the segments: \code{"cut"} (delete the overlap,
#'   SVC-style), \code{"ramp"} (weighted blend across the window,
#'   Spectral-Evolution-style), or \code{"concatenate"} (leave already-joined data
#'   in place --- used when only a gain correction across a splice is wanted, e.g.
#'   ASD).
#' @param ramp_shape For \code{join = "ramp"}, \code{"linear"} (default) or
#'   \code{"cosine"}.
#' @param n_fit Number of bands used for the \code{"linear_extrap"} local fit.
#' @return an object of class \code{splice_config}
#'
#' @seealso \code{\link{match_sensors}}
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' # SVC-like: multiplicative match of the VIS side, clamped, then cut the overlap
#' cfg = splice_config(gain_type = "multiplicative", reference = "right",
#'                     clamp = c(0.8, 1.2), graded = TRUE, join = "cut")
#' cfg
splice_config = function(gain_type      = c("none", "multiplicative", "additive", "ssd"),
                         gain_estimator = c("linear_extrap", "mean_diff"),
                         reference      = c("right", "left", "middle"),
                         window         = NULL,
                         window_inset   = 0.10,
                         gain_at        = "all",
                         clamp          = NULL,
                         graded         = FALSE,
                         join           = c("cut", "ramp", "concatenate"),
                         ramp_shape     = c("linear", "cosine"),
                         n_fit          = 10L){

    gain_type      = match.arg(gain_type)
    gain_estimator = match.arg(gain_estimator)
    reference      = match.arg(reference)
    join           = match.arg(join)
    ramp_shape     = match.arg(ramp_shape)

    if( !is.null(window) && (length(window) != 2 || !is.numeric(window)) ){
        stop("`window` must be NULL or a numeric c(lo, hi)")
    }
    if( !is.null(clamp) && (length(clamp) != 2 || !is.numeric(clamp)) ){
        stop("`clamp` must be NULL or a numeric c(min, max)")
    }
    if( !is.numeric(window_inset) || length(window_inset) != 1 ||
        is.na(window_inset) || window_inset < 0 || window_inset >= 0.5 ){
        stop("`window_inset` must be a single number in [0, 0.5)")
    }
    if( !( identical(gain_at, "all") || identical(gain_at, "first") ||
           (is.numeric(gain_at) && length(gain_at) > 0 && all(gain_at >= 1)) ) ){
        stop("`gain_at` must be \"all\", \"first\", or a vector of junction indices")
    }
    if( !is.numeric(n_fit) || n_fit < 2 ){
        stop("`n_fit` must be an integer >= 2")
    }

    structure(list(gain_type      = gain_type,
                   gain_estimator = gain_estimator,
                   reference      = reference,
                   window         = window,
                   window_inset   = as.numeric(window_inset),
                   gain_at        = if(is.numeric(gain_at)) as.integer(gain_at) else gain_at,
                   clamp          = clamp,
                   graded         = isTRUE(graded),
                   join           = join,
                   ramp_shape     = ramp_shape,
                   n_fit          = as.integer(n_fit)),
              class = "splice_config")
}


#' Does this junction get a gain match?
#'
#' Resolves \code{config$gain_at} against a junction index.
#'
#' @param config a splice_config
#' @param j junction index (1-based)
#' @return TRUE/FALSE
#' @keywords internal
i_gain_at_junction = function(config, j){
    ga = config$gain_at
    if(is.null(ga) || identical(ga, "all")){ return(TRUE) }
    if(identical(ga, "first")){ return(j == 1L) }
    j %in% ga
}


#' @export
print.splice_config = function(x, ...){
    cat("<splice_config>\n")
    cat("  gain : ", x$gain_type,
        if(x$gain_type == "additive") paste0(" (", x$gain_estimator, ")") else "",
        if(x$gain_type == "multiplicative" && x$graded) " (graded)" else "",
        "\n", sep = "")
    cat("  fixed side  : ", x$reference, "\n", sep = "")
    if(x$gain_type != "none"){
        cat("  gain at     : ",
            if(is.numeric(x$gain_at)) paste0("junction(s) ", paste(x$gain_at, collapse = ", "))
            else x$gain_at,
            "\n", sep = "")
    }
    if(!is.null(x$clamp))  cat("  plausible   : [", x$clamp[1], ", ", x$clamp[2], "]\n", sep = "")
    if(!is.null(x$window)){
        cat("  window      : [", x$window[1], ", ", x$window[2], "]\n", sep = "")
    } else {
        cat("  window      : auto (overlap inset ", 100 * x$window_inset, "%)\n", sep = "")
    }
    cat("  join        : ", x$join,
        if(x$join == "ramp") paste0(" (", x$ramp_shape, ")") else "",
        "\n", sep = "")
    invisible(x)
}


#' Built-in splice presets
#'
#' \code{i_splice_preset} maps a preset name to a \code{\link{splice_config}}.
#' These encode each vendor's documented behaviour (see the package's splice
#' reference). \code{"scale"} is handled separately (the legacy algorithm) and is
#' not produced here.
#'
#' @param name one of "svc", "naturaspec", "asd", "cut", "ramp", "concatenate"
#' @return a splice_config
#'
#' @keywords internal
#' @author Jose Eduardo Meireles
i_splice_preset = function(name){
    switch(name,
        ## SVC: match the VIS side to the (temperature-stabilised) SWIR1 with a
        ## graded scalar, then delete the overlap.
        ##
        ## gain_at = 1 is not a detail -- it is the behaviour. SVC's own header
        ## records TWO removals but only ONE matching zone
        ## ("Overlap: Remove @ 970,1901, Matching Type: Radiance @ 976 - 1010"),
        ## and its output leaves detectors 2 and 3 bit-identical to the raw file.
        ## The SWIR1/SWIR2 crossover sits at ~1900 nm, in the deep water band at
        ## the edge of both detectors' sensitivity (mean reflectance ~0.04 over a
        ## handful of bands), so a ratio estimated there is noise: on the
        ## reference data it lands at 0.63-0.84, outside any plausible range.
        ## Matching there corrupts half the spectrum. spectrolab 0.0.19 avoided
        ## this with an `iter = 1` guard in the legacy path; this is that rule,
        ## stated rather than implied.
        "svc" = splice_config(gain_type = "multiplicative", reference = "right",
                              clamp = c(0.8, 1.2), graded = TRUE, join = "cut",
                              gain_at = 1L),

        ## Spectral Evolution / NaturaSpec: SSD-winner gain anchored to the low
        ## (Si) side, linear-ramp blend across the overlap. DERIVED; default
        ## anchors are approximate and per-unit -- override `window` if known.
        "naturaspec" = splice_config(gain_type = "ssd", reference = "left",
                                     join = "ramp", ramp_shape = "linear"),

        ## ASD: data is already joined; optionally correct a residual step across
        ## the stored splice wavelengths with an additive, slope-aware offset,
        ## keeping the central detector fixed. NOT an ASD vendor algorithm.
        "asd" = splice_config(gain_type = "additive", gain_estimator = "linear_extrap",
                             reference = "middle", join = "concatenate"),

        ## Generic building blocks:
        "cut"         = splice_config(gain_type = "none", join = "cut"),
        "ramp"        = splice_config(gain_type = "none", join = "ramp"),
        "concatenate" = splice_config(gain_type = "none", join = "concatenate"),

        stop("unknown splice preset: '", name, "'. Known presets: ",
             "svc, naturaspec, asd, cut, ramp, concatenate (and 'scale' for the ",
             "legacy algorithm).", call. = FALSE)
    )
}


########################################
# Small helpers
########################################

#' Resolve the "middle" reference side to left/right for a given junction
#'
#' @param reference "left", "right", or "middle"
#' @param j junction index (1-based)
#' @param n_junctions total number of junctions
#' @return "left" or "right" (which side is held fixed at this junction)
#' @keywords internal
i_resolve_reference = function(reference, j, n_junctions){
    if(reference != "middle"){
        return(reference)
    }
    ## "middle": fix the central detector. With 2 junctions, junction 1
    ## (outer-left | middle) fixes the right side; junction 2 (middle |
    ## outer-right) fixes the left side.
    if(n_junctions == 1){
        return("right")
    }
    if(j <= n_junctions / 2) "right" else "left"
}


#' Clamp a numeric to a [min, max] range
#' @keywords internal
i_clamp = function(x, lo, hi){
    pmin(pmax(x, lo), hi)
}


#' Inset a window by a fraction of its width on each side
#'
#' The outermost wavelengths of a detector overlap are where both sensors'
#' response is rolling off, so a gain factor estimated across the full overlap is
#' biased by its own worst data. Insetting reproduces what vendors do when they
#' record a matching zone narrower than the overlap it sits in.
#'
#' Degenerate cases (zero-width or an inset that would empty the window) return
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


#' Is an estimated multiplicative gain plausible?
#'
#' \code{clamp} states the range a real detector-matching factor can fall in. A
#' factor outside it does not mean "correct by the bound" --- it means the window
#' the factor was estimated from carries no usable signal. Callers skip the
#' junction instead.
#'
#' @param fac estimated factor
#' @param clamp numeric c(min, max), or NULL for no check
#' @return TRUE if the factor should be applied
#' @keywords internal
i_gain_is_plausible = function(fac, clamp){
    if(!is.finite(fac) || fac <= 0){
        return(FALSE)
    }
    if(is.null(clamp)){
        return(TRUE)
    }
    fac >= clamp[1] && fac <= clamp[2]
}


#' Warn that a junction was left uncorrected because its factor was implausible
#'
#' @param n_rejected number of samples whose factor failed the check
#' @param n_total number of samples
#' @param w the junction wavelength
#' @param clamp the plausible range
#' @return invisible NULL
#' @keywords internal
i_warn_rejected_gain = function(n_rejected, n_total, w, clamp){
    if(n_rejected == 0){
        return(invisible(NULL))
    }
    why = if(is.null(clamp)){
        "the estimated factor was not a usable number"
    } else {
        paste0("the estimated factor fell outside the plausible range [",
               clamp[1], ", ", clamp[2], "]")
    }

    warning("Detector matching skipped at ", w, " nm for ", n_rejected, " of ",
            n_total, " sample(s): ", why, ", which means the matching window ",
            "carries too little signal to estimate a gain (a junction inside a ",
            "deep water band, typically). Those sensors were joined without a ",
            "magnitude correction.", call. = FALSE)
    invisible(NULL)
}


########################################
# The engine
########################################

#' Splice detector segments of a spectra object
#'
#' \code{i_splice} is the internal engine behind the vendor-aware
#' \code{\link{match_sensors}} presets. It routes to the right handling depending
#' on whether the data still carries a physical detector overlap (non-monotonic
#' bands, e.g. raw SVC) or has already been joined (monotonic bands, e.g. ASD).
#'
#' @param x spectra object
#' @param splice_at numeric junction wavelength(s)
#' @param config a \code{\link{splice_config}}
#' @return spectra object with strictly increasing bands
#'
#' @keywords internal
#' @author Jose Eduardo Meireles
i_splice = function(x, splice_at, config){

    splice_at = sort(unlist(splice_at))

    if( i_is_increasing(bands(x)) || config$join == "concatenate" ){
        ## Already-joined data: the only meaningful operation is a gain
        ## correction across each splice wavelength (the ASD / prospectr case).
        return(i_splice_gain_across(x, splice_at, config))
    }

    ## Physical overlap present (raw SVC / SE): optional gain match, then join.
    i_splice_overlap(x, splice_at, config)
}


#' Gain-correct an already-joined (monotonic) spectrum across splice points
#'
#' Used for ASD-style data that is already one continuous spectrum but may show a
#' small step at a detector boundary. For each splice wavelength the segment on
#' the non-fixed side is shifted (additive) or scaled (multiplicative) to meet
#' the fixed side. The \code{"linear_extrap"} estimator fits a short local line on
#' the fixed side and extrapolates it to the splice wavelength, so a sloped
#' spectrum is handled better than a plain mean difference.
#'
#' @param x spectra object (bands strictly increasing)
#' @param splice_at sorted junction wavelengths
#' @param config splice_config
#' @return spectra object
#' @keywords internal
i_splice_gain_across = function(x, splice_at, config){

    if(config$gain_type == "none"){
        return(x)                       # concatenate with no correction
    }

    b   = bands(x)
    v   = value(x)                      # samples x bands
    nj  = length(splice_at)
    win = config$window

    for(j in seq_len(nj)){

        if( !i_gain_at_junction(config, j) ){
            next
        }

        w     = splice_at[j]
        fixed = i_resolve_reference(config$reference, j, nj)

        ## Segments on either side of this splice wavelength
        left_cols  = which(b <  w)
        right_cols = which(b >= w)
        if(length(left_cols) == 0 || length(right_cols) == 0){
            next
        }

        ## Choose which side is adjusted
        if(fixed == "right"){
            fixed_cols = right_cols; adj_cols = left_cols
        } else {
            fixed_cols = left_cols;  adj_cols = right_cols
        }

        ## Per-sample correction
        n_rejected = 0L
        for(i in seq_len(nrow(v))){
            corr = i_gain_correction(bands_fixed = b[fixed_cols],
                                     vals_fixed  = v[i, fixed_cols],
                                     bands_adj   = b[adj_cols],
                                     vals_adj    = v[i, adj_cols],
                                     w           = w,
                                     config      = config,
                                     fixed_side  = fixed)
            if( isTRUE(attr(corr, "rejected")) ){
                n_rejected = n_rejected + 1L
            }
            v[i, adj_cols] = as.numeric(corr)
        }

        i_warn_rejected_gain(n_rejected, nrow(v), w, config$clamp)
    }

    x[] = v
    x
}


#' Compute the corrected values for the adjusted side of one junction (one sample)
#'
#' @param bands_fixed,vals_fixed fixed-side bands and values
#' @param bands_adj,vals_adj     adjusted-side bands and values
#' @param w splice wavelength
#' @param config splice_config
#' @param fixed_side "left" or "right"
#' @return corrected \code{vals_adj}
#' @keywords internal
#' @importFrom stats lm median predict
i_gain_correction = function(bands_fixed, vals_fixed, bands_adj, vals_adj,
                             w, config, fixed_side){

    ## Window near the splice on each side (for mean/ssd estimators)
    win = config$window
    if(is.null(win)){
        ## Default: ~10 band steps on each side of the splice. The step must be
        ## measured WITHIN a segment. Concatenating the two segments first
        ## (`diff(c(bands_fixed, bands_adj))`) picks up the jump *between* them,
        ## which on real data blew the half-window up from ~22 nm to ~22000 nm --
        ## i.e. the "local" window became the whole spectrum.
        steps     = c(diff(bands_fixed), diff(bands_adj))
        steps     = steps[is.finite(steps) & steps > 0]
        span      = if(length(steps) == 0){ 1 } else { stats::median(steps) }
        half      = 10 * span
        win       = c(w - half, w + half)
    }
    f_in = bands_fixed >= win[1] & bands_fixed <= win[2]
    a_in = bands_adj   >= win[1] & bands_adj   <= win[2]
    if(!any(f_in)) f_in = seq_along(bands_fixed) %in% utils::tail(seq_along(bands_fixed),
                                                                 min(config$n_fit, length(bands_fixed)))
    if(!any(a_in)) a_in = seq_along(bands_adj)   %in% utils::head(seq_along(bands_adj),
                                                                 min(config$n_fit, length(bands_adj)))

    mean_f = mean(vals_fixed[f_in], na.rm = TRUE)
    mean_a = mean(vals_adj[a_in],   na.rm = TRUE)

    if(config$gain_type == "multiplicative"){
        fac = mean_f / mean_a
        ## Implausible factor -> the window has no usable signal; leave it alone.
        ## The "rejected" flag lets the caller report it instead of silently
        ## returning an uncorrected segment.
        if( !i_gain_is_plausible(fac, config$clamp) ){
            return(structure(vals_adj, rejected = TRUE))
        }
        return(vals_adj * fac)
    }

    if(config$gain_type == "additive"){
        if(config$gain_estimator == "linear_extrap"){
            ## Fit a short local line on the FIXED side nearest the splice and
            ## extrapolate it to w; offset the adjusted side to meet that value.
            ord   = order(abs(bands_fixed - w))
            k     = min(config$n_fit, length(bands_fixed))
            idx   = ord[seq_len(k)]
            fit   = stats::lm(y ~ x, data = data.frame(x = bands_fixed[idx], y = vals_fixed[idx]))
            pred  = as.numeric(stats::predict(fit, newdata = data.frame(x = w)))
            ## adjusted side value nearest the splice
            near  = which.min(abs(bands_adj - w))
            off   = pred - vals_adj[near]
            return(vals_adj + off)
        } else {                                   # mean_diff
            return(vals_adj + (mean_f - mean_a))
        }
    }

    if(config$gain_type == "ssd"){
        ## SSD-winner: pick additive vs multiplicative by whichever perturbs the
        ## adjusted side least (sum of squared changes).
        fac  = mean_f / mean_a
        if( !i_gain_is_plausible(fac, config$clamp) ){
            return(structure(vals_adj, rejected = TRUE))
        }
        mult = vals_adj * fac
        add  = vals_adj + (mean_f - mean_a)
        ssd_mult = sum((mult - vals_adj)^2, na.rm = TRUE)
        ssd_add  = sum((add  - vals_adj)^2, na.rm = TRUE)
        return(if(ssd_add < ssd_mult) add else mult)
    }

    vals_adj
}


#' Splice a spectrum that still carries a physical detector overlap
#'
#' Handles the raw SVC ("cut") and Spectral Evolution ("ramp") situations: detect
#' the sensor segments, optionally gain-match the scaled side over the overlap,
#' then either delete the overlap (cut) or blend it (ramp).
#'
#' @param x spectra object (bands non-monotonic across sensor overlaps)
#' @param splice_at sorted junction wavelengths
#' @param config splice_config
#' @return spectra object with strictly increasing bands
#' @keywords internal
i_splice_overlap = function(x, splice_at, config){

    ## --- optional gain match applied to the scaled sensor(s) before joining ---
    if(config$gain_type != "none"){
        x = i_splice_overlap_gain(x, splice_at, config)
    }

    ## --- join ---
    if(config$join == "cut"){
        ## Delete the overlapping points: reuse the existing, tested overlap
        ## trimmer, which keeps the left sensor below each splice and the right
        ## sensor at/above it, yielding strictly increasing bands.
        return(i_trim_sensor_overlap(x, splice_at)[["spectra"]])
    }

    if(config$join == "ramp"){
        return(i_splice_ramp(x, splice_at, config))
    }

    ## concatenate on overlapping data is ambiguous; fall back to a cut.
    i_trim_sensor_overlap(x, splice_at)[["spectra"]]
}


#' Apply the gain match to the scaled sensor over each overlap (before a join)
#'
#' Computes, per junction and per sample, a factor (or offset) from the two
#' sensors' values in the overlap window and applies it to the scaled sensor's
#' whole segment --- graded (fading to no correction at the far end) when
#' \code{config$graded} is TRUE, otherwise flat.
#'
#' @keywords internal
i_splice_overlap_gain = function(x, splice_at, config){

    b        = bands(x)
    v        = value(x)
    bounds   = i_find_sensor_overlap_bounds(b)          # column ranges per sensor
    n_sensor = ncol(bounds)
    nj       = length(splice_at)

    seg_cols = lapply(seq_len(n_sensor), function(k){
        seq.int(bounds[["begin", k]], bounds[["end", k]])
    })

    for(j in seq_len(min(nj, n_sensor - 1L))){

        ## Not every junction is matchable -- see splice_config(gain_at).
        if( !i_gain_at_junction(config, j) ){
            next
        }

        w     = splice_at[j]
        fixed = i_resolve_reference(config$reference, j, nj)

        left_cols  = seg_cols[[j]]
        right_cols = seg_cols[[j + 1L]]

        ## Matching window: an explicit one wins, otherwise the physical overlap
        ## inset off both ends (where detector response is rolling off).
        win = config$window
        if(is.null(win)){
            lo  = max(min(b[left_cols]),  min(b[right_cols]))
            hi  = min(max(b[left_cols]),  max(b[right_cols]))
            win = i_inset_window(c(lo, hi), config$window_inset)
        }

        lf = left_cols[  b[left_cols]  >= win[1] & b[left_cols]  <= win[2] ]
        rf = right_cols[ b[right_cols] >= win[1] & b[right_cols] <= win[2] ]
        if(length(lf) == 0 || length(rf) == 0){
            next
        }

        if(fixed == "right"){ fixed_cols = rf; scaled_cols = lf; scaled_seg = left_cols }
        else               { fixed_cols = lf; scaled_cols = rf; scaled_seg = right_cols }

        n_rejected = 0L

        for(i in seq_len(nrow(v))){
            mean_f = mean(v[i, fixed_cols],  na.rm = TRUE)
            mean_s = mean(v[i, scaled_cols], na.rm = TRUE)
            if(!is.finite(mean_f) || !is.finite(mean_s) || mean_s == 0){
                next
            }

            if(config$gain_type == "additive"){
                off              = mean_f - mean_s
                v[i, scaled_seg] = v[i, scaled_seg] + off
            } else {
                fac = mean_f / mean_s                       # multiplicative / ssd default

                ## An implausible factor means the estimate is bad, not that the
                ## correction should be shrunk to the bound: leave the sensor
                ## alone rather than applying a known-bad gain.
                if( !i_gain_is_plausible(fac, config$clamp) ){
                    n_rejected = n_rejected + 1L
                    next
                }

                if(config$graded){
                    ## Taper the factor across the scaled segment: full at the
                    ## junction end, 1 (no correction) at the far end.
                    bs   = b[scaled_seg]
                    if(fixed == "right"){        # scaled = left segment, junction at its high end
                        t = (bs - min(bs)) / max(1e-9, (max(bs) - min(bs)))
                    } else {                     # scaled = right segment, junction at its low end
                        t = (max(bs) - bs) / max(1e-9, (max(bs) - min(bs)))
                    }
                    graded_fac       = 1 + t * (fac - 1)
                    v[i, scaled_seg] = v[i, scaled_seg] * graded_fac
                } else {
                    v[i, scaled_seg] = v[i, scaled_seg] * fac
                }
            }
        }

        i_warn_rejected_gain(n_rejected, nrow(v), w, config$clamp)
    }

    x[] = v
    x
}


#' Linear/cosine ramp blend of overlapping sensors
#'
#' Within each overlap window, produce one output value per 1-band step as a
#' weighted average of the left and right sensors (the right side is interpolated
#' onto the left side's wavelengths in the window). Below/above the window the
#' respective sensor is copied through. Outside all overlaps the spectrum is
#' unchanged. This is the Spectral-Evolution-style join.
#'
#' @keywords internal
#' @importFrom stats approx
i_splice_ramp = function(x, splice_at, config){

    b        = bands(x)
    v        = value(x)
    bounds   = i_find_sensor_overlap_bounds(b)
    n_sensor = ncol(bounds)
    nj       = length(splice_at)

    seg_cols = lapply(seq_len(n_sensor), function(k){
        seq.int(bounds[["begin", k]], bounds[["end", k]])
    })

    ## Build the output sensor-by-sensor. For a junction, the left sensor keeps
    ## everything below the window, the overlap window is blended, and the right
    ## sensor keeps everything above the window.
    out_bands = list()
    out_vals  = list()

    for(k in seq_len(n_sensor)){
        cols = seg_cols[[k]]
        bk   = b[cols]
        vk   = v[, cols, drop = FALSE]

        ## If this sensor is the RIGHT side of a junction, keep only what lies
        ## ABOVE that junction's blend window (the window itself is blended below).
        if(k >= 2 && (k - 1L) <= nj){
            whi  = i_ramp_window(b, seg_cols, splice_at[k - 1L], k - 1L, config)[2]
            keep = bk > whi
            bk = bk[keep]; vk = vk[, keep, drop = FALSE]
        }
        ## If this sensor is the LEFT side of a junction, keep only what lies
        ## BELOW that junction's blend window.
        if(k <= nj){
            wlo  = i_ramp_window(b, seg_cols, splice_at[k], k, config)[1]
            keep = bk < wlo
            bk = bk[keep]; vk = vk[, keep, drop = FALSE]
        }

        out_bands[[k]] = bk
        out_vals[[k]]  = vk
    }

    ## Blended windows
    blend_bands = list()
    blend_vals  = list()
    for(j in seq_len(min(nj, n_sensor - 1L))){
        win  = i_ramp_window(b, seg_cols, splice_at[j], j, config)
        Lc   = seg_cols[[j]];   Rc = seg_cols[[j + 1L]]
        grid = b[Lc][ b[Lc] >= win[1] & b[Lc] <= win[2] ]      # blend on the left grid
        if(length(grid) == 0){ next }

        t = i_ramp_weight(grid, win[1], win[2], config$ramp_shape)   # 0 at lo -> 1 at hi
        bl = matrix(NA_real_, nrow = nrow(v), ncol = length(grid))
        for(i in seq_len(nrow(v))){
            Lval = stats::approx(b[Lc], v[i, Lc], xout = grid, rule = 2)$y
            Rval = stats::approx(b[Rc], v[i, Rc], xout = grid, rule = 2)$y
            bl[i, ] = (1 - t) * Lval + t * Rval
        }
        blend_bands[[j]] = grid
        blend_vals[[j]]  = bl
    }

    ## Assemble everything in wavelength order
    all_bands = c(unlist(out_bands), unlist(blend_bands))
    all_vals  = do.call(cbind, c(out_vals, blend_vals))
    ord       = order(all_bands)

    out = new_spectra(value = i_value(all_vals[, ord, drop = FALSE]),
                      bands = i_bands(all_bands[ord]),
                      names = names(x),
                      meta  = meta(x))

    out
}


#' The blend window for a ramp junction
#' @keywords internal
i_ramp_window = function(b, seg_cols, w, j, config){
    if(!is.null(config$window)){
        return(config$window)
    }
    Lc = seg_cols[[j]]; Rc = seg_cols[[j + 1L]]
    lo = max(min(b[Lc]), min(b[Rc]))
    hi = min(max(b[Lc]), max(b[Rc]))
    if(lo >= hi){                     # no real overlap: a hairline window at w
        lo = w - 1; hi = w + 1
    }
    c(lo, hi)
}


#' Ramp weight: 0 at the low edge, 1 at the high edge of the window
#' @keywords internal
i_ramp_weight = function(xx, x0, x1, shape){
    if(x0 >= x1){
        return(rep(0.5, length(xx)))
    }
    t = i_clamp((xx - x0) / (x1 - x0), 0, 1)
    if(shape == "cosine"){
        t = 0.5 * (1 - cos(pi * t))
    }
    t
}
