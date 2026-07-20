library("testthat")
library("spectrolab")

spec = as_spectra(spec_matrix_example, name_idx = 1)

test_that("combine binds rows and keeps the shared band grid", {
    s = suppressWarnings(suppressMessages(combine(spec, spec)))
    expect_s3_class(s, "spectra")
    expect_equal(unname(nrow(s)), 2L * unname(nrow(spec)))
    expect_equal(bands(s), bands(spec))
})

test_that("combine unions metadata columns, filling gaps with NA", {
    s1 = spec
    s2 = spec
    meta(s1, "only_in_1") = seq_len(nrow(s1))
    meta(s2, "only_in_2") = seq_len(nrow(s2)) * 10

    s = suppressWarnings(suppressMessages(combine(s1, s2)))

    expect_true(all(c("only_in_1", "only_in_2") %in% names(meta(s))))
    ## rows from s1 have NA in the s2-only column and vice versa
    expect_true(all(is.na(meta(s, "only_in_2")[seq_len(nrow(s1)), ])))
    expect_true(all(is.na(meta(s, "only_in_1")[nrow(s1) + seq_len(nrow(s2)), ])))
})

test_that("combine errors when band vectors differ in length (B2 regression)", {
    ## Before the fix, `bands(s1) != bands(s2)` recycled the shorter vector and
    ## could pass silently. Different band counts must be a hard error.
    s2 = spec[, bands(spec)[1:100]]
    expect_error(suppressWarnings(suppressMessages(combine(spec, s2))),
                 "same bands")
})

test_that("combine errors when bands differ in value", {
    s2 = spec
    bands(s2) = bands(s2) + 1
    expect_error(suppressWarnings(suppressMessages(combine(spec, s2))),
                 "same bands")
})
