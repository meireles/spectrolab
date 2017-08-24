library("testthat")
library("spectrolab")

context("Spectra transpose")

spec = as.spectra(spec_matrix_example)

test_that("transposing spec throws", {
    expect_error( t(spec) )
})

test_that("transposing spec matrix", {
    expect_is(t(as.matrix(spec)), "matrix")
})

test_that("transposing spec data frame", {
    expect_is(t(as.data.frame(spec)), "matrix")
})
