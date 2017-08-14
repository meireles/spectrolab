library("testthat")
library("spectrolab")

context("Spectra subsetting by")

spec   = as.spectra(spec_matrix_example)
by_def = names(spec)

test_that("by of wrong length throws", {
    expect_error( subset_by(spec, by = by_def[ 1 : floor(nrow(spec) / 2)],
                            n_min = 1, n_max = 5) )
})

test_that("by of wrong length throws", {
    expect_error( subset_by(spec, by = NULL, n_min = 1, n_max = 5))
})

test_that("zero n_max throws an error", {
    expect_error( subset_by(spec, by = by_def, n_min = 1, n_max = 0))
})


test_that("n_max is larger than n_min", {
    expect_error( subset_by(spec, by = by_def, n_min = 5, n_max = 2))
})

test_that("n_max = 1 will return a subetted obj", {
    expect_lt( nrow(subset_by(spec, by = by_def, n_min = 1, n_max = 1)), nrow(spec) )
})

test_that("n_min = 5 will return a subetted obj", {
    expect_lt( nrow(subset_by(spec, by = by_def, n_min = 5, n_max = Inf)), nrow(spec) )
})


test_that("null if n_min is too large", {
    expect_null( subset_by(spec, by = by_def, n_min = 100, n_max = Inf) )
})


test_that("same spec obj is returned if n_min and n_max are wide enough", {
    expect_true( nrow(subset_by(spec, by = by_def,
                                n_min = 1, n_max = Inf)) == nrow(spec))
})

