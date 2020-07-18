library("testthat")
library("spectrolab")

context("Spectra setter")

s0 = as_spectra(spec_matrix_example[1:3, ], name_idx = 1)
meta(s0, "bogus") = c("a", "b", "c")

s1 = s2 = s3 = s0


s1[1, ] = s1[2, ]

meta(s2) = data.frame("crap" = c(1,2,3))

s3 = s3[ , 400:700]

test_that("Set spectrum to another spectrum replaces names and meta too", {
    expect_false( all(names(s0) == names(s1)) )
    expect_true( all(names(s1[1, ]) == names(s1[2, ])) )

    expect_false( all(meta(s0) == meta(s1)) )
    expect_true(all(meta(s1[1, ]) == meta(s1[2, ])))
})


test_that("Setting fails if bands or meta are incompatible", {
    expect_error( s1[1:2, ] <- s2[1:2, ] )
    expect_error( s1[1, ] <- s3[1, ] )
})
