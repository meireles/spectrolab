library("testthat")
library("spectrolab")

## mean of the raw values in a wavelength window (avoids Ops.spectra on subsets)
band_mean = function(z, lo, hi) mean(value(z)[, bands(z) >= lo & bands(z) <= hi])

## Phase 2 of the match_sensors redesign: the general detect -> gain -> join
## splice engine, its config/presets, and the match_sensors dispatch.

## ---- splice_config & presets ----------------------------------------------

test_that("splice_config validates arguments", {
    cfg = splice_config(gain_type = "multiplicative", reference = "right",
                        clamp = c(0.8, 1.2), graded = TRUE, join = "cut")
    expect_s3_class(cfg, "splice_config")
    expect_equal(cfg$gain_type, "multiplicative")
    expect_true(cfg$graded)

    expect_error(splice_config(gain_type = "bogus"))          # match.arg
    expect_error(splice_config(clamp = 1))                     # bad clamp
    expect_error(splice_config(window = c(1, 2, 3)))          # bad window
    expect_error(splice_config(n_fit = 1))                     # too few
})

test_that("presets expand to sensible configs; unknown errors", {
    svc = spectrolab:::i_splice_preset("svc")
    expect_equal(svc$gain_type, "multiplicative")
    expect_equal(svc$join, "cut")
    expect_equal(svc$reference, "right")
    expect_equal(svc$clamp, c(0.8, 1.2))

    ns = spectrolab:::i_splice_preset("naturaspec")
    expect_equal(ns$join, "ramp")
    expect_equal(ns$gain_type, "ssd")

    asd = spectrolab:::i_splice_preset("asd")
    expect_equal(asd$gain_type, "additive")
    expect_equal(asd$gain_estimator, "linear_extrap")
    expect_equal(asd$join, "concatenate")

    expect_error(spectrolab:::i_splice_preset("nope"))
})

test_that("i_resolve_reference resolves 'middle' per junction", {
    expect_equal(spectrolab:::i_resolve_reference("right", 1, 2), "right")
    expect_equal(spectrolab:::i_resolve_reference("middle", 1, 2), "right") # outer-left | middle
    expect_equal(spectrolab:::i_resolve_reference("middle", 2, 2), "left")  # middle | outer-right
})

## ---- cut join (SVC removal) on real overlapping data ----------------------

test_that("method = 'cut' removes the overlap and yields increasing bands", {
    d = system.file("extdata", "svc_raw_and_overlap_matched_serbin", "SVC_Files",
                    package = "spectrolab")
    skip_if(d == "", "serbin raw extdata not installed")
    raw = suppressWarnings(suppressMessages(read_spectra(d, format = "sig")))

    m = suppressWarnings(suppressMessages(
        match_sensors(raw, method = "cut", splice_at = c(970, 1901))))
    expect_true(spectrolab:::i_is_increasing(bands(m)))
    expect_lt(ncol(m), ncol(raw))                 # overlap points removed
})

test_that("method = 'svc' applies a graded match and still cuts", {
    d = system.file("extdata", "svc_raw_and_overlap_matched_serbin", "SVC_Files",
                    package = "spectrolab")
    skip_if(d == "", "serbin raw extdata not installed")
    raw = suppressWarnings(suppressMessages(read_spectra(d, format = "sig")))

    pure = suppressWarnings(suppressMessages(
        match_sensors(raw, method = "cut", splice_at = c(970, 1901))))
    svc  = suppressWarnings(suppressMessages(
        match_sensors(raw, method = "svc", splice_at = c(970, 1901))))

    expect_true(spectrolab:::i_is_increasing(bands(svc)))
    expect_equal(bands(svc), bands(pure))         # same join, different values
    expect_false(isTRUE(all.equal(value(svc), value(pure))))
})

## ---- additive step correction on already-joined (ASD-style) data ----------

test_that("additive linear_extrap removes a known step at a splice", {
    ## Monotonic spectrum with a clean +0.05 step above 1000 nm.
    wl   = 350:2000
    base = 0.2 + 0.00005 * (wl - 350)            # gently sloped
    v    = base
    v[wl >= 1000] = v[wl >= 1000] + 0.05
    spec = spectra(matrix(v, nrow = 1), wl, "s")

    step_before = band_mean(spec, 1000, 1002) - band_mean(spec, 997, 999)
    expect_gt(step_before, 0.045)

    cfg = splice_config(gain_type = "additive", gain_estimator = "linear_extrap",
                        reference = "left", join = "concatenate")
    out = suppressMessages(match_sensors(spec, splice_at = 1000, config = cfg))

    expect_true(spectrolab:::i_is_increasing(bands(out)))
    step_after = band_mean(out, 1000, 1002) - band_mean(out, 997, 999)
    expect_lt(abs(step_after), 0.01)             # step largely removed
})

test_that("method = 'asd' corrects a residual step on monotonic ASD data", {
    asd_dir = "data_for_tests/asd"
    skip_if(length(list.files(asd_dir, pattern = "[.]asd$")) == 0, "no .asd fixtures")
    asd = suppressWarnings(suppressMessages(read_spectra(asd_dir, format = "asd")))

    ## splice points come from sensor_info provenance; result stays monotonic and
    ## finite, and the correction near reflectance ~1 is small.
    out = suppressWarnings(suppressMessages(match_sensors(asd, method = "asd")))
    expect_true(spectrolab:::i_is_increasing(bands(out)))
    expect_equal(unname(dim(out)), unname(dim(asd)))
    expect_true(all(is.finite(value(out))))
    expect_lt(max(abs(value(out) - value(asd))), 0.5)
})

## ---- ramp join (Spectral Evolution style) ---------------------------------

test_that("ramp blends a synthetic two-sensor overlap into increasing bands", {
    ## VIS 400-1000 and SWIR 950-1800 overlap over 950-1000; SWIR offset up.
    v_wl = seq(400, 1000, 2); s_wl = seq(950, 1800, 2)
    v_v  = seq(0.10, 0.50, length.out = length(v_wl))
    s_v  = seq(0.50, 0.30, length.out = length(s_wl)) + 0.1
    syn  = spectra(matrix(c(v_v, s_v), nrow = 1), c(v_wl, s_wl), "a")

    expect_false(spectrolab:::i_is_increasing(bands(syn)))
    r = suppressWarnings(suppressMessages(match_sensors(syn, method = "ramp", splice_at = 975)))
    expect_true(spectrolab:::i_is_increasing(bands(r)))
    expect_true(all(is.finite(value(r))))
})

test_that("ramp weight is 0 at the low edge and 1 at the high edge", {
    w = spectrolab:::i_ramp_weight(c(950, 975, 1000), 950, 1000, "linear")
    expect_equal(w[1], 0)
    expect_equal(w[3], 1)
    expect_equal(w[2], 0.5)
    ## degenerate window -> constant 0.5 (guards the flat-average bug)
    expect_equal(spectrolab:::i_ramp_weight(c(1, 2), 5, 5, "linear"), c(0.5, 0.5))
})

## ---- dispatch: engine vs legacy -------------------------------------------

test_that("config overrides method and routes to the engine", {
    wl   = 350:1500
    v    = 0.2 + numeric(length(wl))
    v[wl >= 900] = v[wl >= 900] + 0.03
    spec = spectra(matrix(v, nrow = 1), wl, "s")

    cfg = splice_config(gain_type = "additive", gain_estimator = "mean_diff",
                        reference = "left", join = "concatenate")
    out = suppressMessages(match_sensors(spec, splice_at = 900, method = "svc", config = cfg))
    ## additive mean_diff correction was applied (config won over method="svc")
    expect_true(spectrolab:::i_is_increasing(bands(out)))
    expect_lt(abs(band_mean(out, 900, 902) - band_mean(out, 897, 899)), 0.01)
})

test_that("legacy scale path is unchanged (method NULL/'scale')", {
    ## a synthetic increasing spectrum with an explicit splice still processes via
    ## the legacy algorithm exactly as before the engine existed.
    wl = 400:900
    base = seq(0.1, 0.5, length.out = length(wl))
    v = rbind(base, base); v[, wl > 700] = v[, wl > 700] * 1.5
    syn = spectra(v, wl, c("a", "b"))

    a = suppressWarnings(suppressMessages(match_sensors(syn, splice_at = 701, fixed_sensor = 1)))
    b = suppressWarnings(suppressMessages(match_sensors(syn, splice_at = 701, fixed_sensor = 1,
                                                        method = "scale")))
    expect_equal(value(a), value(b))
})
