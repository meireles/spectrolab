library("testthat")
library("spectrolab")

spec = as_spectra(spec_matrix_example, name_idx = 1)

test_that("normalize records a magnitude that reconstructs the original", {
    nn  = suppressWarnings(suppressMessages(normalize(spec)))
    mag = meta(nn, "normalization_magnitude")[, 1]

    expect_equal(length(mag), unname(nrow(spec)))

    ## value(normalized) * magnitude must return the original reflectance
    recon = value(nn) * mag
    expect_equal(max(abs(recon - value(spec))), 0, tolerance = 1e-8)
})

test_that("each normalized spectrum has unit vector length", {
    nn = suppressWarnings(suppressMessages(normalize(spec)))
    row_norms = sqrt(rowSums(value(nn)^2))
    expect_equal(unname(row_norms), rep(1, nrow(nn)), tolerance = 1e-8)
})

test_that("re-normalizing warns about the existing magnitude", {
    nn = suppressWarnings(suppressMessages(normalize(spec)))
    expect_warning(suppressMessages(normalize(nn)),
                   "already", ignore.case = TRUE)
})
