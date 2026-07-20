library("testthat")
library("spectrolab")

context("length/is.na/Math/c/rbind overloads")

spec = as_spectra(spec_matrix_example, name_idx = 1)

test_that("length() returns the number of samples, not the internal slot count", {
    expect_equal(length(spec), nrow(spec))
    expect_equal(unname(length(spec)), 50L)
})

test_that("is.na() checks the value matrix, not the internal slots", {
    expect_false(any(is.na(spec)))

    spec_na = spec
    value(spec_na)[1, 1] = NA
    expect_true(is.na(spec_na)[1, 1])
    expect_true(any(is.na(spec_na)))
})

test_that("Math group generic works for common functions", {
    a = abs(spec)
    expect_s3_class(a, "spectra")
    expect_equal(value(a), abs(value(spec)))

    sq = sqrt(spec)
    expect_s3_class(sq, "spectra")
    expect_equal(value(sq), sqrt(value(spec)))

    rnd = round(spec, 2)
    expect_equal(value(rnd), round(value(spec), 2))
})

test_that("Math group generic handles cumulative functions row-wise", {
    cs = cumsum(spec)
    expect_s3_class(cs, "spectra")
    expect_equal(dim(cs), dim(spec))
    expect_equal(value(cs)[1, ], cumsum(value(spec)[1, ]))
})

test_that("c() and rbind() combine spectra instead of degrading to list/matrix", {
    s1 = spec[1:2, ]
    s2 = spec[3:4, ]

    cc = c(s1, s2)
    expect_s3_class(cc, "spectra")
    expect_equal(unname(nrow(cc)), 4)

    rb = rbind(s1, s2)
    expect_s3_class(rb, "spectra")
    expect_equal(unname(nrow(rb)), 4)
})
