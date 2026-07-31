################################################################################
# Regression tests for the 0.0.20 CRAN-prep review fixes.
# Each block names the defect it locks down.
################################################################################

library(spectrolab)

spec = as_spectra(spec_matrix_example, name_idx = 1)

sig_path = system.file("extdata", "Acer_example", package = "spectrolab")
sig      = suppressMessages(read_spectra(sig_path, format = "sig"))

## Column indices of the last band of each sensor after trimming, i.e. the
## columns the detector junctions actually sit between. Deliberately derived from
## the sensor vector rather than from the splice wavelength: the trimmed sensors
## do not start exactly at splice_at (here sensor 3 begins at 1908.2, not 1900),
## so comparing at `splice_at` measures a step INSIDE one detector.
junction_cols = function(x, splice_at){
    sen = spectrolab:::i_trim_sensor_overlap(x, splice_at)[["sensor"]]
    which(diff(as.integer(factor(sen, levels = unique(sen)))) != 0)
}

## Mean absolute jump across the junction sitting between columns k and k + 1.
step_idx = function(x, k){
    mean(abs(value(x)[ , k + 1] - value(x)[ , k]))
}


test_that("match_sensors() corrects the first junction only when sensors overlap", {
    ## This block used to assert the opposite -- that EVERY junction is gain
    ## matched -- on the reasoning that the first-junction-only guard silently
    ## left the far detector unmatched. Validating against the vendor's own
    ## overlap-matched files
    ## (test_svc_vendor_match.R) showed that reasoning was wrong: SVC removes both
    ## overlaps but magnitude-matches only the VNIR/SWIR1 one, and its output
    ## leaves detectors 2 and 3 identical to the raw file.
    ##
    ## The far junction of a 3-detector instrument sits near 1900 nm, in the deep
    ## water band at the edge of both detectors' sensitivity. The factor
    ## estimated there is noise, so "correcting" it ramped a large error across a
    ## whole detector. The guard was right; it is back, and stated outright in
    ## R/splice.R.
    skip_if(!nzchar(sig_path))

    expect_equal(ncol(spectrolab:::i_find_sensor_overlap_bounds(bands(sig))), 3)

    jx       = junction_cols(sig, c(990, 1900))
    expect_equal(length(jx), 2)

    matched  = suppressMessages(match_sensors(sig, splice_at = c(990, 1900)))
    cut_only = spectrolab:::i_trim_sensor_overlap(sig, c(990, 1900))[["spectra"]]

    ## The first junction is tightened relative to a plain cut with no gain.
    expect_lt(step_idx(matched, jx[1]), step_idx(cut_only, jx[1]))

    ## Everything above the detectors' physical overlap (which tops out at
    ## 1016.6 nm, above the last crossfaded band) comes through the join as cut,
    ## unmodified -- including the whole of detector 3.
    above = which(bands(matched) > 1016.6)
    expect_equal(value(matched)[ , above], value(cut_only)[ , above])
})


test_that("match_sensors() corrects every junction when there is no overlap", {
    ## The counterpart to the rule above: a spectrum that is already joined has
    ## no overlap window to estimate from, so splice_at splits it and each
    ## junction is matched. This is the ASD-shaped case.
    wl = 350:2000
    v  = 0.2 + 0.00005 * (wl - 350)
    v[wl >= 1000] = v[wl >= 1000] * 1.15         # step up at the first junction
    v[wl >= 1800] = v[wl >= 1800] * 0.85         # step down at the second
    joined = spectra(rbind(v, v), wl, c("a", "b"))

    expect_equal(ncol(spectrolab:::i_find_sensor_overlap_bounds(bands(joined))), 1)

    m = suppressMessages(match_sensors(joined, splice_at = c(1000, 1800)))
    b = bands(m)

    ## both steps shrink
    step = function(x, w){
        bb = bands(x)
        abs(mean(value(x)[, min(which(bb >= w))]) - mean(value(x)[, max(which(bb < w))]))
    }
    expect_lt(step(m, 1000), step(joined, 1000))
    expect_lt(step(m, 1800), step(joined, 1800))

    ## and the far detector really was touched
    expect_false(isTRUE(all.equal(value(m)[, b >= 1800],
                                  value(joined)[, bands(joined) >= 1800])))
})


test_that("match_sensors() does not silently ignore fixed_sensor", {
    ## With 2 splice points the middle detector has to be the fixed one --
    ## correcting detector 2 toward detector 1 would leave detector 3 behind. It
    ## used to overwrite the user's choice without a word.
    expect_warning(suppressMessages(match_sensors(sig, splice_at = c(990, 1900),
                                                  fixed_sensor = 1)),
                   "fixed_sensor")

    ## Asking for the value it would pick anyway stays quiet.
    expect_silent(suppressMessages(match_sensors(sig, splice_at = c(990, 1900),
                                                 fixed_sensor = 2)))
})


test_that("match_sensors() rejects an out-of-range fixed_sensor", {
    ## Two sensors -> fixed_sensor must be 1 or 2; 9 used to die deep inside
    ## with "argument is of length zero".
    two = sig[ , bands(sig) <= 1906.2 ]
    expect_error(suppressMessages(match_sensors(two, splice_at = 990,
                                                fixed_sensor = 9)),
                 "fixed_sensor")
})


test_that("resample() rejects a destination grid that is not strictly increasing", {
    ## Was: unsorted/duplicated new_bands were accepted and produced a spectra
    ## that violated the increasing-band invariant, failing much later with a
    ## misleading "match sensor overlap first" message.
    expect_error(resample(spec, c(500, 400, 600), fwhm = 10), "strictly increasing")
    expect_error(resample(spec, c(500, 500, 600), fwhm = 10), "strictly increasing")
    expect_error(resample(spec, c(500, NA, 600),  fwhm = 10), "finite")
    expect_error(resample(spec, numeric(0),       fwhm = 10), "non-empty")
    expect_error(resample(spec, "600",            fwhm = 10), "numeric")

    ## The valid case still works.
    ok = suppressWarnings(resample(spec, seq(500, 600, 10), fwhm = 10))
    expect_true(spectrolab:::i_is_increasing(bands(ok)))
})


test_that("resample() carries sensor_info provenance through", {
    matched = suppressMessages(match_sensors(sig, splice_at = c(990, 1900)))
    expect_false(is.null(sensor_info(matched)))

    rs = suppressWarnings(resample(matched, seq(400, 2400, 10), fwhm = 10))
    expect_false(is.null(sensor_info(rs)))
    expect_equal(nrow(sensor_info(rs)), unname(nrow(rs)))

    ## smooth(method = "gaussian") routes through resample(), so it inherits this.
    sm = suppressWarnings(suppressMessages(smooth(matched)))
    expect_false(is.null(sensor_info(sm)))
})


test_that("meta() never misses a label silently, even with zero metadata columns", {
    ## Was: an object with no metadata columns returned quietly for ANY label,
    ## including under quiet = FALSE which is documented as a hard error.
    expect_warning(meta(spec, "nope"), "not found")
    expect_error(meta(spec, "nope", quiet = FALSE), "not found")

    with_meta = spec
    meta(with_meta, "grp") = rep("a", nrow(spec))
    expect_warning(meta(with_meta, "nope"), "not found")
    expect_error(meta(with_meta, "nope", quiet = FALSE), "not found")

    ## A label that does exist stays quiet.
    expect_silent(meta(with_meta, "grp"))
})


test_that("the band-gap warning lists bands readably", {
    ## Was: paste(..., sep = ",") glued 898, 599, 8 into "8985998".
    gappy = spec
    b     = bands(gappy)
    b[1000:length(b)] = b[1000:length(b)] + 5000
    bands(gappy) = b

    w = tryCatch(spectrolab:::i_mind_the_gap_smoothing(gappy),
                 warning = function(x) conditionMessage(x))
    expect_true(grepl("band\\(s\\): [0-9]", w))
})


test_that("combine() warns when mixing normalized and un-normalized spectra", {
    n = suppressMessages(normalize(spec))
    expect_warning(combine(n, spec), "normalized")
    expect_warning(combine(spec, n), "normalized")

    ## Two objects of the same kind stay quiet on this axis.
    expect_silent(combine(spec[1:2, ], spec[3:4, ]))
})


test_that("Ops and combine() agree on what 'the same bands' means", {
    ## Was: Ops used exact !=, combine() used all.equal(), so a 1e-12 difference
    ## errored in one and passed in the other.
    jittered = spec
    bands(jittered) = bands(spec) + 1e-12

    expect_s3_class(spec + jittered, "spectra")
    expect_equal(nrow(combine(spec, jittered)), 2 * nrow(spec))

    ## A genuinely different grid still errors.
    shifted = spec
    bands(shifted) = bands(spec) + 1
    expect_error(spec + shifted, "band labels")
})


test_that("the value matrix rejects non-numeric input instead of NA-filling it", {
    ## Was: mode(x) <- "numeric" turned text into NAs behind a coercion warning
    ## and the constructor returned an all-NA spectra.
    bad = data.frame(a = c("x", "y"), b = c(3, 4), stringsAsFactors = FALSE)
    expect_error(spectrolab:::i_value(bad), "could not be coerced")
    expect_error(spectra(value = bad, bands = c(1, 2), names = c("s1", "s2")),
                 "could not be coerced")

    ## Genuine NAs in numeric input are preserved, not treated as a failure.
    with_na = matrix(c(1, NA, 3, 4), nrow = 2)   # column-major: [2, 1] is the NA
    expect_true(is.na(spectrolab:::i_value(with_na)[2, 1]))
    expect_equal(sum(is.na(spectrolab:::i_value(with_na))), 1L)
})


test_that("quantile() no longer tags the result with a dead marker class", {
    q = quantile(spec, probs = c(0.25, 0.75))
    expect_identical(class(q), "spectra")
    expect_equal(unname(nrow(q)), 2L)
    expect_equal(names(q), c("0.25", "0.75"))
})


test_that("smooth_spline caps the number of forked workers", {
    ## Was: hard-wired detectCores() - 1L, which forks e.g. 127 processes on a
    ## big host and breaks CRAN's two-core ceiling.
    expect_equal(formals(smooth_spline)$cores,
                 quote(getOption("mc.cores", 2L)))

    sm = smooth(spec[1:4, ], method = "spline", parallel = FALSE)
    expect_equal(dim(sm), dim(spec[1:4, ]))
})
