library("testthat")
library("spectrolab")

context("Spectra subsetting by")

mat    = matrix( c(10, 4, 2, 2,  10,
                   8,  5, 2, 3,  2,
                   9,  6, 2, NA, 20),
                 ncol = 5, byrow = TRUE, dimnames = list(NULL, c(1, 2, 3, 4, 5)))

spec   = as_spectra(mat, name_idx = NULL, meta_idxs = NULL)


test_that("mean from spec equals the colMeans equiv.", {
    expect_equivalent( as.vector(value(mean(spec))),
                       colMeans(mat, na.rm = TRUE) )
})


test_that("median from spec equals the apply mat equiv.", {
    expect_equivalent( as.vector(value(median(spec))),
                       apply(mat, 2, median, na.rm = TRUE) )
})


test_that("var from spec equals the apply mat equiv.", {
    expect_equivalent( as.vector(value(var(spec))),
                       apply(mat, 2, var, na.rm = TRUE) )
})

test_that("sd from spec equals the apply mat equiv.", {
    expect_equivalent( as.vector(value(sd(spec))),
                       apply(mat, 2, sd, na.rm = TRUE) )
})
