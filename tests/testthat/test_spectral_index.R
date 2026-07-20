library("testthat")
library("spectrolab")

spec = as_spectra(spec_matrix_example, name_idx = 1)

context("Two-band spectral indices")

test_that("make_spectral_index computes a normalized difference between two bands", {
    idx = make_spectral_index(800, 680)(spec)
    v   = value(spec)
    w   = bands(spec)
    r1  = v[ , which.min(abs(w - 800))]
    r2  = v[ , which.min(abs(w - 680))]
    expect_equal(unname(idx), (r1 - r2) / (r1 + r2))
    expect_equal(names(idx), names(spec))
})

test_that("make_spectral_index errors when no band is within tolerance", {
    idx_fn = make_spectral_index(800.5, 680, tolerance = 0)
    expect_error(idx_fn(spec))
})

test_that("make_spectral_index's tolerance can be overridden at call time", {
    idx_fn = make_spectral_index(800.5, 680, tolerance = 0)
    expect_error(idx_fn(spec, tolerance = 1), NA)
})

test_that("indices require strictly increasing bands", {
    bad = spec
    bands(bad)[2] = bands(bad)[1]
    expect_error(spectral_index$ndvi(bad))
})

test_that("spectral_index$ndvi is make_spectral_index(800, 680)", {
    expect_equal(spectral_index$ndvi(spec), make_spectral_index(800, 680)(spec))
})

test_that("spectral_index$pri is make_spectral_index(570, 531)", {
    expect_equal(spectral_index$pri(spec), make_spectral_index(570, 531)(spec))
})

test_that("spectral_indices returns ndvi and pri side by side", {
    out = spectral_indices(spec)
    expect_s3_class(out, "data.frame")
    expect_equal(nrow(out), unname(nrow(spec)))
    expect_equal(out$ndvi, unname(spectral_index$ndvi(spec)))
    expect_equal(out$pri, unname(spectral_index$pri(spec)))
})

test_that("spectral_indices errors on an unknown index name", {
    expect_error(spectral_indices(spec, which = "not_an_index"))
})
