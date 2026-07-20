library("testthat")
library("spectrolab")

## B1: duplicate band labels are preserved (not nudged), handled like duplicate
## sample names, and selecting them tells the user.

test_that("the constructor preserves exact duplicate wavelengths (no nudge)", {
    v = matrix(1:6, nrow = 1)
    b = c(500, 500, 600, 600, 600, 700)     # 2x 500, 3x 600
    s = spectra(v, b, "a")
    expect_equal(bands(s), b)                # unchanged, byte-for-byte
    expect_true(anyDuplicated(bands(s)) > 0) # duplicates survive
})

test_that("real raw SVC overlap keeps its duplicate wavelengths", {
    d = system.file("extdata", "svc_raw_and_overlap_matched_serbin", "SVC_Files",
                    package = "spectrolab")
    skip_if(d == "", "serbin raw extdata not installed")
    raw = suppressWarnings(suppressMessages(read_spectra(d, format = "sig")))
    b   = bands(raw)
    expect_true(anyDuplicated(b) > 0)
    ## no fractional "nudge signature" (a physical wavelength stays physical)
    nudged = any(abs(b - round(b, 1)) > 1e-6 & abs(b - round(b, 1)) < 1e-2)
    expect_false(nudged)
})

test_that("selecting a duplicated band returns all matches and messages", {
    v = matrix(1:6, nrow = 1)
    s = spectra(v, c(500, 500, 600, 700, 800, 900), "a")

    expect_message(out <- s[, 500], "duplicated")
    expect_true(is_spectra(out))
    expect_equal(unname(ncol(out)), 2L)        # both 500 columns
    expect_equal(bands(out), c(500, 500))
})

test_that("selecting a non-duplicated band is silent and simplifies", {
    v = matrix(1:6, nrow = 1)
    s = spectra(v, c(500, 500, 600, 700, 800, 900), "a")

    expect_silent(res <- s[, 600])
    expect_true(is.numeric(res) && !is_spectra(res))   # simplified to a vector
})

test_that("subsetting all bands (j missing) does not message about duplicates", {
    v = matrix(1:6, nrow = 1)
    s = spectra(v, c(500, 500, 600, 700, 800, 900), "a")
    expect_silent(s[1, ])
})

## duplicate handling is consistent with duplicate SAMPLE names (already allowed)
test_that("duplicate bands behave like duplicate sample names", {
    dup_names = spectra(matrix(1:8, nrow = 2), c(400, 500, 600, 700), c("a", "a"))
    expect_equal(unname(nrow(dup_names["a", ])), 2L)  # both rows (pre-existing)

    dup_bands = spectra(matrix(1:8, nrow = 2), c(400, 500, 500, 700), c("a", "b"))
    expect_equal(unname(ncol(suppressMessages(dup_bands[, 500]))), 2L)  # both cols
})

## the internal trim is positional, so duplicates cannot corrupt the join
test_that("match_sensors cut yields strictly increasing bands on duplicate raw data", {
    d = system.file("extdata", "svc_raw_and_overlap_matched_serbin", "SVC_Files",
                    package = "spectrolab")
    skip_if(d == "", "serbin raw extdata not installed")
    raw = suppressWarnings(suppressMessages(read_spectra(d, format = "sig")))
    expect_true(anyDuplicated(bands(raw)) > 0)         # duplicates present pre-splice

    m = suppressWarnings(suppressMessages(
        match_sensors(raw, method = "cut", splice_at = c(970, 1901))))
    expect_true(spectrolab:::i_is_increasing(bands(m)))
    expect_false(anyDuplicated(bands(m)) > 0)          # unique after splice
})
