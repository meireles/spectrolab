library("testthat")
library("spectrolab")

spec = as_spectra(spec_matrix_example, name_idx = 1)

context("Continuum removal")

test_that("continuum_removal returns a spectra of the same dimensions", {
    s = continuum_removal(spec)
    expect_s3_class(s, "spectra")
    expect_equal(dim(s), dim(spec))
    expect_false(identical(value(s), value(spec)))
})

test_that("continuum-removed values are at most 1 (values sit at or below their continuum)", {
    s = continuum_removal(spec)
    expect_true(all(value(s) <= 1 + sqrt(.Machine$double.eps)))
})

test_that("continuum_removal errors when bands are not strictly increasing", {
    bad = spec
    bands(bad)[2] = bands(bad)[1]
    expect_error(continuum_removal(bad))
})

test_that("the upper hull always includes the first and last band", {
    w = bands(spec)
    r = value(spec)[1, ]
    h = spectrolab:::i_upper_hull_idx(w, r)
    expect_true(1 %in% h)
    expect_true(length(w) %in% h)
})
