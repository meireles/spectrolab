library("testthat")
library("spectrolab")

context("Spectra subset")

spec = as_spectra(spec_matrix_example, name_idx = 1)

test_that("spectra wl are subset with indexes", {
    expect_error(spec[ , 1:100])
    expect_error(spec[ , 1:600])
    expect_error(spec[ , 600:2800])
})

