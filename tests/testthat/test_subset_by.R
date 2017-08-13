library("testthat")
library("spectrolab")

context("Spectra subsetting by")

spec = as.spectra(spec_matrix_example)

test_that("by of wrong length throws", {
    expect_error( subset_by(spec, by = names(spec)[ 1 : floor(nrow(spec) / 2)], n_max = 5))
})

test_that("by of wrong length throws", {
    expect_error( subset_by(spec, by = NULL, n_max = 5))
})

test_that("zero n_max throws an error", {
    expect_error( subset_by(spec, by = names(spec), n_max = 0))
})

test_that("n_max = 1 will return a subetted obj", {
    expect_lt( nrow(subset_by(spec, by = names(spec), n_max = 1)), nrow(spec) )
})


test_that("spectra is converted to matrix", {
    expect_true( nrow(subset_by(spec, by = names(spec), n_max = Inf)) == nrow(spec))
})

