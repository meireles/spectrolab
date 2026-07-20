library("testthat")
library("spectrolab")

context("Tier 4 interface-consistency fixes")

s = as_spectra(spec_matrix_example, name_idx = 1)

test_that("provenance setters accept a bare NA (documented 'unknown' marker)", {
    q = s
    expect_silent(quantity(q) <- NA)
    expect_true(is.na(quantity(q)))

    w = s
    expect_silent(wavelength_unit(w) <- NA)
    expect_true(is.na(wavelength_unit(w)))
})

test_that("subset_by accepts a factor `by` (its docs promise 'coercible to factor')", {
    out = subset_by(s, by = factor(names(s)), n_min = 1, n_max = Inf)
    expect_true(is_spectra(out))
})

test_that("bands() returns empty (not an error) when a range matches nothing", {
    b = bands(s, min = 99990, max = 99999)
    expect_true(is.numeric(b))
    expect_length(b, 0)
})

test_that("resample / make_fwhm / smooth_fwhm reject non-spectra input", {
    m = matrix(1:4, 2)
    expect_error(resample(m, 1:2, 1), "class spectra")
    expect_error(make_fwhm(m, 1:2), "class spectra")
    expect_error(smooth_fwhm(m), "class spectra")
})

test_that("smooth_fwhm(x, fwhm = NULL) auto-computes (does not forward NULL)", {
    out = suppressMessages(smooth_fwhm(s, fwhm = NULL))
    expect_true(is_spectra(out))
})
