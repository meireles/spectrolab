library("testthat")
library("spectrolab")

context("Spectra combine")

spec = as.spectra(spec_matrix_example)
s1   = spec
wavelengths(s1) = seq(ncol(s1))
s2   = spec[ , 400:2000]

test_that("combining wrong class throws", {
    expect_error( combine(spec, as.matrix(spec)) )
})

test_that("combining incompatible wavelength values throws", {
    expect_error( combine(spec, s1) )
})

test_that("combining different band number throws", {
    expect_error( combine(spec, s2) )
})

test_that("combining identical obj doubles the number of samples", {
    expect_true( nrow(combine(spec, spec)) == nrow(spec) * 2 )
})
