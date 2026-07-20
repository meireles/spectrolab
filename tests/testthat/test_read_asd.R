library("testthat")
library("spectrolab")

## Relies on testthat setting "tests/testthat" as the working directory!!!

asd_dir   = "data_for_tests/asd"
asd_files = sort(list.files(asd_dir, pattern = "[.]asd$", full.names = TRUE))

context("Read ASD binary files")

## Regression tests for the ASD reader, in particular the radiance types, which
## were broken by an argument-order bug in spectra() (bands/value/name swapped)
## and by building a spectra object where the downstream code expects a matrix.

test_that("ASD fixtures are present", {
    skip_if(length(asd_files) == 0, "no .asd fixtures available")
    expect_gte(length(asd_files), 1)
})

test_that("read_spectra reads ASD reflectance, target and reference radiance", {
    skip_if(length(asd_files) == 0, "no .asd fixtures available")

    for(ty in c("target_reflectance", "target_radiance", "reference_radiance")){
        s = suppressWarnings(suppressMessages(
            read_spectra(asd_files, format = "asd", type = ty)))

        expect_s3_class(s, "spectra")
        expect_equal(unname(nrow(s)), length(asd_files))
        expect_true(all(is.finite(value(s))))
        expect_true(all(diff(bands(s)) > 0))
    }
})

test_that("ASD sample names come from the file names", {
    skip_if(length(asd_files) == 0, "no .asd fixtures available")

    s        = suppressWarnings(suppressMessages(
        read_spectra(asd_files, format = "asd", type = "target_radiance")))
    expected = gsub("[.]asd$", "", basename(asd_files), ignore.case = TRUE)
    expect_identical(names(s), expected)
})

test_that("ASD reflectance equals target radiance / reference radiance", {
    skip_if(length(asd_files) == 0, "no .asd fixtures available")

    rfl = suppressWarnings(suppressMessages(
        read_spectra(asd_files, format = "asd", type = "target_reflectance")))
    rd  = suppressWarnings(suppressMessages(
        read_spectra(asd_files, format = "asd", type = "target_radiance")))
    ref = suppressWarnings(suppressMessages(
        read_spectra(asd_files, format = "asd", type = "reference_radiance")))

    ## relative reflectance is defined as spectrum / white_reference; the three
    ## read paths are independent, so this cross-check would fail if any of them
    ## built its value matrix incorrectly.
    expect_equal(value(rfl), value(rd) / value(ref))
})

test_that("ASD radiance and reflectance are actually different data", {
    skip_if(length(asd_files) == 0, "no .asd fixtures available")

    rfl = suppressWarnings(suppressMessages(
        read_spectra(asd_files, format = "asd", type = "target_reflectance")))
    rd  = suppressWarnings(suppressMessages(
        read_spectra(asd_files, format = "asd", type = "target_radiance")))
    expect_false(identical(value(rfl), value(rd)))
})

test_that("read_spectra rejects an unknown ASD type", {
    skip_if(length(asd_files) == 0, "no .asd fixtures available")
    expect_error(
        suppressWarnings(suppressMessages(
            read_spectra(asd_files, format = "asd", type = "bogus"))))
})
