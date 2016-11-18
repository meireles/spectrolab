library("testthat")
library("spectrolab")

context("Spectra subset")

spec = as.spectra(spec_matrix_example)
ones = rep(1, dim(spec)[[2]])

test_that("spectra wl are subset with indexes", {
    expect_equal( reflectance(spec) %*% ones, spec %*% ones)
})
