library("testthat")
library("spectrolab")

context("Quantile naming scheme")

s  = as_spectra(spec_matrix_example, name_idx = 1)


q  = quantile(s, probs = c(0, 0.5, 1), names = NULL)
test_that("quantile name is NULL", {
    expect_equal( names(q), c("spec_0", "spec_0.5", "spec_1"))
})

q  = quantile(s, probs = c(0, 0.5, 1), names = NA)
test_that("quantile name is NA", {
    expect_equal(names(q), c("spec_0", "spec_0.5", "spec_1"))
})

q  = quantile(s, probs = c(0, 0.5, 1), names = "bogus")
test_that("quantile name is single char", {
    expect_equal( names(q), c("bogus", "bogus", "bogus"))
})

q  = quantile(s, probs = c(0, 0.5, 1), names = "")
test_that("quantile name is single an empty char", {
    expect_equal( names(q), c("", "", ""))
})

