library("testthat")
library("spectrolab")

## Value-asserting golden tests for read_spectra. The pre-existing read tests
## only checked that an object of class `spectra` came back; these assert on
## concrete band ranges, dimensions and values so that a regression in the
## parsers (e.g. the ASD band-grid or byte-size handling) is actually caught.

## ---- SVC (.sig) -----------------------------------------------------------

test_that("SVC .sig reads with expected structure and reflectance range", {
    d = system.file("extdata", "Acer_example", package = "spectrolab")
    skip_if(d == "", "Acer_example extdata not installed")

    s = suppressWarnings(suppressMessages(
        read_spectra(d, format = "sig", exclude_if_matches = c("BAD", "WR"))))

    expect_s3_class(s, "spectra")
    expect_equal(unname(nrow(s)), 7L)
    expect_equal(unname(ncol(s)), 1024L)

    ## SVC covers roughly 340-2520 nm
    expect_gt(min(bands(s)), 300)
    expect_lt(max(bands(s)), 2600)

    ## target reflectance: non-negative and physically plausible
    expect_true(all(value(s) >= 0))
    expect_lt(max(value(s)), 1.5)
})

## ---- PSR (.sed) -----------------------------------------------------------

test_that("PSR .sed reads with expected band grid", {
    d = system.file("extdata", "psr_DN_brett", package = "spectrolab")
    skip_if(d == "", "psr_DN_brett extdata not installed")

    s = suppressWarnings(suppressMessages(
        read_spectra(d, format = "sed", exclude_if_matches = "not_working")))

    expect_s3_class(s, "spectra")
    expect_equal(unname(ncol(s)), 2151L)
    expect_equal(range(bands(s)), c(350, 2500))
})

## ---- ASD (.asd) golden values --------------------------------------------
## Relies on testthat setting "tests/testthat" as the working directory.

test_that("ASD reader returns the expected band grid and concrete values", {
    asd_dir = "data_for_tests/asd"
    skip_if(length(list.files(asd_dir, pattern = "[.]asd$")) == 0,
            "no .asd fixtures available")

    s = suppressWarnings(suppressMessages(
        read_spectra(asd_dir, format = "asd", type = "target_reflectance")))

    ## Version "as7", 1 nm grid from 350 to 2500 -> exactly 2151 bands.
    ## A band-grid off-by (B3) or wrong byte width (B4) would move these.
    expect_equal(unname(ncol(s)), 2151L)
    expect_equal(range(bands(s)), c(350, 2500))
    expect_equal(bands(s)[1], 350)
    expect_equal(diff(bands(s))[1], 1)

    ## Concrete relative-reflectance value (spectrum / white reference).
    expect_equal(round(value(s)[1, 1], 4), 1.0057, tolerance = 1e-3)
})
