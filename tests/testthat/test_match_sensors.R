library("testthat")
library("spectrolab")

## match_sensors() and guess_splice_at() previously had zero tests.

test_that("match_sensors reduces a known multiplicative step at the splice", {
    ## Synthetic 2-sample spectrum with a 50% jump at 700 nm.
    wl = 400:900
    base = seq(0.1, 0.5, length.out = length(wl))
    v = rbind(base, base)
    v[, wl > 700] = v[, wl > 700] * 1.5
    syn = spectra(v, wl, c("a", "b"))

    step_before = mean(syn[, 701]) / mean(syn[, 700])
    expect_gt(step_before, 1.4)     # ~1.5 discontinuity present

    fixed = suppressWarnings(suppressMessages(
        match_sensors(syn, splice_at = 701, fixed_sensor = 1)))

    bf  = bands(fixed)
    lo  = max(bf[bf <  701])
    hi  = min(bf[bf >= 701])
    step_after = mean(fixed[, hi]) / mean(fixed[, lo])

    ## The step should be largely removed (close to 1) and much smaller than before
    expect_lt(abs(step_after - 1), 0.05)
    expect_lt(abs(step_after - 1), abs(step_before - 1))
})

test_that("match_sensors on raw SVC data yields strictly increasing bands", {
    raw = system.file("extdata", "svc_raw_and_overlap_matched_serbin",
                       "SVC_Files", package = "spectrolab")
    skip_if(raw == "", "serbin raw extdata not installed")

    s = suppressWarnings(suppressMessages(read_spectra(raw, format = "sig")))
    expect_false(spectrolab:::i_is_increasing(bands(s)))

    sp = guess_splice_at(s)
    expect_length(sp, 2)

    m = suppressWarnings(suppressMessages(match_sensors(s, splice_at = sp)))
    expect_true(spectrolab:::i_is_increasing(bands(m)))
    ## overlap is trimmed, so the matched object has fewer bands than the raw one
    expect_lt(ncol(m), ncol(s))
    expect_equal(unname(nrow(m)), unname(nrow(s)))
})

test_that("guess_splice_at finds two SVC sensor transitions near 1000 and 1900 nm", {
    raw = system.file("extdata", "svc_raw_and_overlap_matched_serbin",
                       "SVC_Files", package = "spectrolab")
    skip_if(raw == "", "serbin raw extdata not installed")

    s  = suppressWarnings(suppressMessages(read_spectra(raw, format = "sig")))
    sp = guess_splice_at(s)

    expect_true(sp[1] > 950  && sp[1] < 1050)
    expect_true(sp[2] > 1850 && sp[2] < 1950)
})
