library("testthat")
library("spectrolab")

context("Spectra conversion")

test_that("matrix is converted to spectra", {
    expect_s3_class( as.spectra(spec_matrix_example, name_idx = 1), "spectra" )
})

test_that("data frame is converted to spectra", {
    expect_s3_class( as.spectra( as.data.frame(spec_matrix_example), name_idx = 1), "spectra" )
})

test_that("spectra is converted to matrix", {
    expect_true( is.matrix(as.matrix( as.spectra(spec_matrix_example, name_idx = 1) )))
})

