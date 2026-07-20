library("testthat")
library("spectrolab")

spec = as_spectra(spec_matrix_example, name_idx = 1)

context("Savitzky-Golay smoothing and derivatives")

test_that("smooth_sgolay returns a spectra of the same dimensions", {
    skip_if_not_installed("signal")
    s = smooth_sgolay(spec)
    expect_s3_class(s, "spectra")
    expect_equal(dim(s), dim(spec))
    expect_false(identical(value(s), value(spec)))
})

test_that("smooth(method = 'sgolay') dispatches to smooth_sgolay", {
    skip_if_not_installed("signal")
    s1 = smooth(spec, method = "sgolay")
    s2 = smooth_sgolay(spec)
    expect_equal(value(s1), value(s2))
})

test_that("smooth_sgolay errors on an even or too-small filter length", {
    skip_if_not_installed("signal")
    expect_error(smooth_sgolay(spec, p = 3, n = 4))
    expect_error(smooth_sgolay(spec, p = 3, n = 3))
})

test_that("deriv_spectra returns a spectra of the same dimensions", {
    skip_if_not_installed("signal")
    d = suppressMessages(deriv_spectra(spec, order = 1))
    expect_s3_class(d, "spectra")
    expect_equal(dim(d), dim(spec))
    expect_false(identical(value(d), value(spec)))
})

test_that("higher order derivatives differ from the first order derivative", {
    skip_if_not_installed("signal")
    d1 = suppressMessages(deriv_spectra(spec, order = 1))
    d2 = suppressMessages(deriv_spectra(spec, order = 2))
    expect_false(identical(value(d1), value(d2)))
})

test_that("deriv_spectra requires a positive integer order", {
    skip_if_not_installed("signal")
    expect_error(deriv_spectra(spec, order = 0))
    expect_error(deriv_spectra(spec, order = 1.5))
})

test_that("deriv_spectra emits a message unless quiet = TRUE", {
    skip_if_not_installed("signal")
    expect_message(deriv_spectra(spec, order = 1), "derivative")
    expect_silent(deriv_spectra(spec, order = 1, quiet = TRUE))
})
