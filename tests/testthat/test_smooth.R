library("testthat")
library("spectrolab")

spec = as_spectra(spec_matrix_example, name_idx = 1)

context("Smoothing spectra")

## Regression test for the bug where smooth(method = "spline") computed the
## smoothed spectra but returned the *original* unchanged object.
test_that("spline smoothing actually changes the values", {
    s = suppressWarnings(suppressMessages(
        smooth(spec, method = "spline", parallel = FALSE)))
    expect_s3_class(s, "spectra")
    expect_equal(dim(s), dim(spec))
    expect_false(identical(value(s), value(spec)))
})

test_that("gaussian smoothing returns a spectra of the same dimensions", {
    s = suppressWarnings(suppressMessages(smooth(spec, method = "gaussian")))
    expect_s3_class(s, "spectra")
    expect_equal(dim(s), dim(spec))
    expect_false(identical(value(s), value(spec)))
})

test_that("moving average smoothing returns a spectra", {
    s = suppressWarnings(suppressMessages(smooth(spec, method = "moving_average")))
    expect_s3_class(s, "spectra")
})

## Regression test for the bug where an unknown/misspelled method silently
## returned NULL instead of erroring.
test_that("an unknown smoothing method errors", {
    expect_error(smooth(spec, method = "gausian"))       # misspelled
    expect_error(smooth(spec, method = "not_a_method"))
})
