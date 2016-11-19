library("testthat")
library("spectrolab")

context("Constraints in spectra reflectance values")

n    = 10
nm   = LETTERS[seq(n)]
wl   = seq(n) + 399
rf   = matrix(runif(n * n, -10, 10), ncol = n)
rf01 = matrix(rbeta(n * n, 1, 1), ncol = n)

test_that("default ctor", {
    expect_s3_class(spectra(rf, wl, nm), "spectra")
    expect_s3_class(spectra(rf01, wl, nm), "spectra")
})


test_that("enforce01 in ctor == T", {
    expect_s3_class(spectra(rf01, wl, nm, enforce01 = T), "spectra")
    expect_error(spectra(rf, wl, nm, enforce01 = T))
})

