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

test_that("enforce01 getter", {
    expect_equal(enforce01(spectra(rf, wl, nm)), FALSE)
    expect_equal(enforce01(spectra(rf01, wl, nm, enforce01 = TRUE)), TRUE)
})

test_that("enforce01 setter", {
    s   = spectra(rf, wl, nm)
    expect_error(enforce01(s) <- TRUE)

    s01 = spectra(rf01, wl, nm)
    expect_silent(enforce01(s01) <- TRUE)
    enforce01(s01) = TRUE
    expect_true(enforce01(s01) == TRUE)
})
