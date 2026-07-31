library("testthat")
library("spectrolab")

################################################################################
# The private splice engine behind match_sensors(): segment -> tapered gain match
# -> cut -> crossfade. match_sensors() is the only public entry point; everything
# tested here is internal machinery reached through it (or through :::).
################################################################################

band_mean = function(x, lo, hi){
    b = bands(x)
    mean(value(x)[ , b >= lo & b <= hi ])
}

svc_raw = function(){
    d = system.file("extdata", "svc_raw_and_overlap_matched_serbin", "SVC_Files",
                    package = "spectrolab")
    if(d == ""){ return(NULL) }
    suppressWarnings(suppressMessages(read_spectra(d, format = "sig")))
}

## the bare join, with no gain match and no crossfade -- the reference every
## "did the algorithm actually do something" assertion is made against
cut_only = function(x, splice_at){
    spectrolab:::i_trim_sensor_overlap(x, splice_at)[["spectra"]]
}


## ---- segmentation ---------------------------------------------------------

test_that("i_sensor_segments finds overlapping detectors from band reversals", {
    w = c(seq(400, 1000, 2), seq(950, 1800, 2))
    s = spectrolab:::i_sensor_segments(w, 975)

    expect_true(s$overlap)
    expect_length(s$cols, 2)
    expect_equal(range(w[s$cols[[1]]]), c(400, 1000))
    expect_equal(range(w[s$cols[[2]]]), c(950, 1800))
})

test_that("i_sensor_segments splits already-joined data at the splice points", {
    w = 350:2000
    s = spectrolab:::i_sensor_segments(w, c(1000, 1800))

    expect_false(s$overlap)
    expect_length(s$cols, 3)
    ## no band is lost or duplicated by the split
    expect_equal(sort(unlist(s$cols, use.names = FALSE)), seq_along(w))
    expect_equal(min(w[s$cols[[2]]]), 1000)
    expect_equal(max(w[s$cols[[1]]]), 999)
})

test_that("i_sensor_segments keeps every band when a splice falls between bands", {
    ## Was: the segment boundary was computed as max(which(w <= splice_at)) and
    ## the trim then dropped that column from both sides, so a splice point that
    ## did not land exactly on a band silently deleted one.
    w = seq(400, 1000, by = 2.5)
    s = spectrolab:::i_sensor_segments(w, 701)

    expect_equal(sort(unlist(s$cols, use.names = FALSE)), seq_along(w))
    expect_lt(max(w[s$cols[[1]]]), 701)
    expect_gt(min(w[s$cols[[2]]]), 701)
})

test_that("i_sensor_segments rejects splice points outside the data", {
    expect_error(spectrolab:::i_sensor_segments(350:2000, 3000), "outside the band range")
    expect_error(spectrolab:::i_sensor_segments(350:2000, 100),  "outside the band range")
})


## ---- window and taper -----------------------------------------------------

test_that("i_inset_window trims both ends and degrades gracefully", {
    expect_equal(spectrolab:::i_inset_window(c(0, 100), 0.10), c(10, 90))
    expect_equal(spectrolab:::i_inset_window(c(0, 100), 0),    c(0, 100))
    ## degenerate window is returned unchanged rather than inverted
    expect_equal(spectrolab:::i_inset_window(c(5, 5), 0.10), c(5, 5))

    ## the SVC overlap inset by 10% lands on the vendor's own matching zone
    w = spectrolab:::i_inset_window(c(971.8, 1016.6), 0.10)
    expect_true(w[1] > 975 && w[1] < 978)      # vendor says 976
    expect_true(w[2] > 1008 && w[2] < 1014)    # vendor says 1010
})

test_that("i_taper_weights are full at the junction and zero at the far end", {
    wl = seq(400, 1000, 10)

    hi = spectrolab:::i_taper_weights(wl, "high", 1)   # junction at the high end
    expect_equal(hi[length(hi)], 1)
    expect_equal(hi[1], 0)
    expect_equal(hi[wl == 700], 0.5)                   # power 1 == straight ramp

    lo = spectrolab:::i_taper_weights(wl, "low", 1)    # junction at the low end
    expect_equal(lo[1], 1)
    expect_equal(lo[length(lo)], 0)

    ## a convex taper puts less correction in the middle than a straight ramp
    expect_lt(spectrolab:::i_taper_weights(wl, "high", 1.3)[wl == 700], 0.5)

    ## single-band segment: nothing to taper across
    expect_equal(spectrolab:::i_taper_weights(500, "high", 1.3), 1)
})

test_that("i_solve_gain accounts for the taper instead of taking a plain ratio", {
    ## Fixed side reads 1.10, scaled side 1.00, so the naive factor is 1.10. The
    ## taper is only at 0.5 in the window, so the amplitude has to be twice as
    ## large for the applied correction to actually reach 1.10 there.
    fixed  = matrix(1.10, nrow = 1, ncol = 4)
    scaled = matrix(1.00, nrow = 1, ncol = 4)

    expect_equal(spectrolab:::i_solve_gain(fixed, scaled, rep(1.0, 4)), 1.10)
    expect_equal(spectrolab:::i_solve_gain(fixed, scaled, rep(0.5, 4)), 1.20)
})


## ---- plausibility gate ----------------------------------------------------

test_that("i_gain_is_plausible rejects rather than floors", {
    expect_true(spectrolab:::i_gain_is_plausible(0.95, c(1/3, 3)))
    expect_false(spectrolab:::i_gain_is_plausible(0.10, c(1/3, 3)))
    expect_false(spectrolab:::i_gain_is_plausible(NaN,  c(1/3, 3)))
    expect_false(spectrolab:::i_gain_is_plausible(-1,   NULL))
    expect_true(spectrolab:::i_gain_is_plausible(0.01,  NULL))   # no range, no check
    ## vectorised, one verdict per sample
    expect_equal(spectrolab:::i_gain_is_plausible(c(0.9, 10), c(1/3, 3)),
                 c(TRUE, FALSE))
})

test_that("an implausible gain warns and leaves the data uncorrected", {
    ## Two sensors overlapping over 950-1000 nm, where the right one reads a tenth
    ## of the left: no plausible detector gain, so the junction must be refused
    ## rather than corrected by the bound.
    l_wl = seq(400, 1000, 5)
    r_wl = seq(950, 1500, 5)
    syn  = spectra(matrix(c(rep(0.5, length(l_wl)), rep(0.05, length(r_wl))), nrow = 1),
                   c(l_wl, r_wl), "a")

    expect_warning(out <- suppressMessages(match_sensors(syn, splice_at = 975)),
                   "outside the plausible range")

    ## Refused, not floored: the join is exactly the bare cut, with no crossfade
    ## either (blending detectors that were never made comparable would look
    ## smooth and be wrong).
    expect_equal(value(out), value(cut_only(syn, 975)))
})


## ---- the join -------------------------------------------------------------

test_that("the join removes the overlap and yields increasing bands", {
    raw = svc_raw()
    skip_if(is.null(raw), "serbin raw extdata not installed")

    m = suppressWarnings(suppressMessages(match_sensors(raw, splice_at = c(970, 1901))))
    expect_true(spectrolab:::i_is_increasing(bands(m)))
    expect_lt(ncol(m), ncol(raw))                 # overlap points removed
    expect_equal(bands(m), bands(cut_only(raw, c(970, 1901))))
})

test_that("the gain match and crossfade change values, not the band grid", {
    raw = svc_raw()
    skip_if(is.null(raw), "serbin raw extdata not installed")

    pure = cut_only(raw, c(970, 1901))
    m    = suppressWarnings(suppressMessages(match_sensors(raw, splice_at = c(970, 1901))))

    expect_equal(bands(m), bands(pure))
    expect_false(isTRUE(all.equal(value(m), value(pure))))
})

test_that("the crossfade closes the seam a bare cut leaves open", {
    raw = svc_raw()
    skip_if(is.null(raw), "serbin raw extdata not installed")

    pure = cut_only(raw, c(970, 1901))
    m    = suppressWarnings(suppressMessages(match_sensors(raw, splice_at = c(970, 1901))))

    step = function(x){
        b = bands(x)
        i = max(which(b <  970))
        j = min(which(b >= 970))
        mean(abs(value(x)[ , j] / value(x)[ , i] - 1))
    }

    expect_gt(step(pure), 0.05)      # ~10% step across the raw junction
    expect_lt(step(m),    0.01)      # closed to well under a percent
})

test_that("the crossfade only touches the overlap region", {
    raw = svc_raw()
    skip_if(is.null(raw), "serbin raw extdata not installed")

    pure = cut_only(raw, c(970, 1901))
    m    = suppressWarnings(suppressMessages(match_sensors(raw, splice_at = c(970, 1901))))
    b    = bands(m)

    ## The physical overlap tops out at 1016.6 nm; above it detector 2 comes
    ## through the join untouched, and so does detector 3.
    above = b > 1016.6
    expect_equal(value(m)[ , above], value(pure)[ , above])
})


## ---- already-joined (ASD-style) data --------------------------------------

test_that("a step on already-joined data is corrected without an overlap", {
    ## Monotonic spectrum with a clean multiplicative step above 1000 nm.
    wl   = 350:2000
    v    = 0.2 + 0.00005 * (wl - 350)
    v[wl >= 1000] = v[wl >= 1000] * 1.25
    spec = spectra(matrix(v, nrow = 1), wl, "s")

    before = band_mean(spec, 1000, 1002) - band_mean(spec, 997, 999)
    expect_gt(before, 0.04)

    out = suppressMessages(match_sensors(spec, splice_at = 1000))

    expect_true(spectrolab:::i_is_increasing(bands(out)))
    expect_equal(unname(dim(out)), unname(dim(spec)))     # nothing to cut away
    after = band_mean(out, 1000, 1002) - band_mean(out, 997, 999)
    expect_lt(abs(after), 0.01)
})

test_that("match_sensors corrects a residual step on monotonic ASD data", {
    asd_dir = "data_for_tests/asd"
    skip_if(length(list.files(asd_dir, pattern = "[.]asd$")) == 0, "no .asd fixtures")
    asd = suppressWarnings(suppressMessages(read_spectra(asd_dir, format = "asd")))

    sp  = spectrolab:::i_splice_from_provenance(sensor_info(asd))
    skip_if(is.null(sp), "no splice provenance on these fixtures")

    out = suppressWarnings(suppressMessages(match_sensors(asd, splice_at = sp)))
    expect_true(spectrolab:::i_is_increasing(bands(out)))
    expect_equal(unname(dim(out)), unname(dim(asd)))
    expect_true(all(is.finite(value(out))))
    expect_lt(max(abs(value(out) - value(asd))), 0.5)
})


## ---- there is exactly one public entry point ------------------------------

test_that("the splice engine is private", {
    exported = getNamespaceExports("spectrolab")
    expect_true("match_sensors" %in% exported)
    expect_false(any(c("splice_config", "i_splice", "i_splice_preset") %in% exported))
    ## and match_sensors takes no algorithm-selection arguments
    expect_equal(names(formals(match_sensors)),
                 c("x", "splice_at", "fixed_sensor", "interpolate_wvl"))
})
