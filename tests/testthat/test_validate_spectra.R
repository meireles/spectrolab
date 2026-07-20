library("testthat")
library("spectrolab")

spec = as_spectra(spec_matrix_example, name_idx = 1)

test_that("validate_spectra passes a well-formed object", {
    expect_true(validate_spectra(spec))
    expect_true(validate_spectra(spec, stop = FALSE))
})

test_that("validate_spectra catches a broken names slot", {
    bad = unclass(spec)
    bad$names = bad$names[-1]          # nrow(value) != length(names)
    class(bad) = "spectra"

    expect_error(validate_spectra(bad), "length\\(names\\)")
    expect_warning(res <- validate_spectra(bad, stop = FALSE))
    expect_false(res)
})

test_that("validate_spectra catches a non-numeric value matrix", {
    bad = unclass(spec)
    bad$value[] = as.character(bad$value)   # coerce to character
    class(bad) = "spectra"
    expect_error(validate_spectra(bad), "numeric matrix")
})

test_that("new_spectra assembles the four slots without coercion", {
    s = spectrolab:::new_spectra(value = value(spec),
                                 bands = bands(spec),
                                 names = names(spec),
                                 meta  = meta(spec))
    expect_s3_class(s, "spectra")
    expect_true(validate_spectra(s))
    expect_identical(value(s), value(spec))
})

test_that("constructor validates when spectrolab.debug is on", {
    old = getOption("spectrolab.debug")
    on.exit(options(spectrolab.debug = old), add = TRUE)
    options(spectrolab.debug = TRUE)

    ## a normal construction still works
    expect_s3_class(as_spectra(spec_matrix_example, name_idx = 1), "spectra")
})
