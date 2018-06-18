library("testthat")
library("spectrolab")

context("Spectra stat overloads")

spec = as.spectra(spec_matrix_example, name_idx = 1)

test_that("mean(spec)[ , 400 equals mean(spec[ , 400])", {
    expect_equivalent(mean(spec)[ , 400], mean(spec[ , 400]))
})

test_that("median(spec)[ , 400 equals median(spec[ , 400])", {
    expect_equivalent(median(spec)[ , 400], median(spec[ , 400]))
})

test_that("sd(spec)[ , 400 equals sd(spec[ , 400])", {
    expect_equivalent(sd(spec)[ , 400], sd(spec[ , 400]))
})

test_that("var(spec)[ , 400 equals var(spec[ , 400])", {
    expect_equivalent(var(spec)[ , 400], var(spec[ , 400]))
})
