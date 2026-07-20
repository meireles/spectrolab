library("testthat")
library("spectrolab")

## resample() and make_fwhm() previously had zero tests.

spec = as_spectra(spec_matrix_example, name_idx = 1)  # 50 x 2101, 400-2500 nm

test_that("resample lands on the requested (in-range) bands", {
    nb = seq(500, 2400, 10)
    rs = suppressWarnings(suppressMessages(resample(spec, new_bands = nb, fwhm = 10)))

    expect_s3_class(rs, "spectra")
    expect_equal(bands(rs), nb)
    expect_equal(unname(nrow(rs)), unname(nrow(spec)))
    expect_true(all(is.finite(value(rs))))
})

test_that("resample roughly conserves reflectance magnitude", {
    nb = seq(500, 2400, 10)
    rs = suppressWarnings(suppressMessages(resample(spec, new_bands = nb, fwhm = 10)))

    ## A Gaussian-kernel resample to a similar range should preserve the
    ## per-sample mean reflectance to within a few percent.
    m_in  = rowMeans(value(spec)[, bands(spec) >= 500 & bands(spec) <= 2400])
    m_out = rowMeans(value(rs))
    expect_lt(max(abs(m_in - m_out) / m_in), 0.05)
})

test_that("resample refuses non-increasing bands", {
    raw = system.file("extdata", "svc_raw_and_overlap_matched_serbin",
                       "SVC_Files", package = "spectrolab")
    skip_if(raw == "", "serbin raw extdata not installed")
    s = suppressWarnings(suppressMessages(read_spectra(raw, format = "sig")))
    expect_false(spectrolab:::i_is_increasing(bands(s)))
    expect_error(suppressWarnings(suppressMessages(
        resample(s, new_bands = seq(500, 900, 5), fwhm = 5))))
})

test_that("make_fwhm returns one positive value per new band", {
    nb   = seq(500, 2400, 10)
    fwhm = suppressWarnings(suppressMessages(make_fwhm(spec, new_bands = nb)))
    expect_equal(length(fwhm), length(nb))
    expect_true(all(fwhm > 0))
})
