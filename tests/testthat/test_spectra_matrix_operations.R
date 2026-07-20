library("testthat")
library("spectrolab")

context("Spectra matrix operations")

spec   = as_spectra(spec_matrix_example, name_idx = 1)
ones_c = rep.int(1, ncol(spec))
ones_r = rep.int(1, nrow(spec))

test_that("spectra matrix multiplication matches the underlying value matrix", {
    expect_equivalent( value(spec) %*% ones_c, spec %*% ones_c )
    expect_equivalent( ones_r %*% value(spec), ones_r %*% spec )
})

test_that("%*% dispatches with spectra on either side", {
    m = matrix(1, nrow = 5, ncol = nrow(spec))

    left  = spec %*% t(as.matrix(spec))
    right = m %*% spec

    expect_true(is.matrix(left))
    expect_false(is_spectra(left))
    expect_equal(dim(left), c(unname(nrow(spec)), unname(nrow(spec))))

    expect_true(is.matrix(right))
    expect_equal(dim(right), c(5, unname(ncol(spec))))
})

test_that("%*% errors on incompatible dimensions, same as plain matrices", {
    bad = matrix(1, nrow = 3, ncol = 3)
    expect_error(spec %*% bad)
})
