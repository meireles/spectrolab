library("testthat")
library("spectrolab")

context("Quantile naming scheme")

s  = as_spectra(spec_matrix_example, name_idx = 1)


q  = quantile(s, probs = c(0, 0.5, 1), sample_names = NULL)
test_that("quantile name is NULL", {
    expect_equal( names(q), c("0", "0.5", "1"))
})

q  = quantile(s, probs = c(0, 0.5, 1), sample_names = NA)
test_that("quantile name is NA", {
    expect_equal(names(q), c("0", "0.5", "1"))
})

q  = quantile(s, probs = c(0, 0.5, 1), sample_names = "bogus")
test_that("quantile name is single char", {
    expect_equal( names(q), c("bogus", "bogus", "bogus"))
})

q  = quantile(s, probs = c(0, 0.5, 1), sample_names = "")
test_that("quantile name is single an empty char", {
    expect_equal( names(q), c("", "", ""))
})

## base quantile's `names` is a logical; it must now flow through `...` to
## stats::quantile without hijacking the sample naming (regression guard for
## the rename of the old `names` argument to `sample_names`).
test_that("base `names` logical no longer sets sample names", {
    q = quantile(s, probs = c(0, 0.5, 1), names = FALSE)
    expect_equal(names(q), c("0", "0.5", "1"))
})

