library("testthat")
library("spectrolab")

context("Spectra vector normalization")

spec            = as_spectra(spec_matrix_example, name_idx = 1)

spec_na_few_samples = spec_na_all_col = spec

spec_na_all_col[ , 400:410] = NA
spec_na_few_samples[1:3, 400:410] = NA

test_that("normalization on spec with a few NA samples does not return all NA", {
    expect_true( ! all(is.na(value(normalize(spec_na_few_samples)))))
})

test_that("normalization on spec with a col of all NAs does not return all NA matrix", {
    expect_true(! all(is.na(value(normalize(spec_na_all_col)))))
})
