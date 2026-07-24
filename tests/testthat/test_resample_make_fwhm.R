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

################################################################################
# Overlap-integral model (boxcar source x Gaussian destination)
################################################################################

## Legacy point-sampled ("delta function") kernel, kept here only as a
## regression baseline to show the new model changes what it should.
legacy_resample_one = function(bands, values, t, fwhm){
    sigma = fwhm / (2 * sqrt(2 * log(2)))
    k     = stats::dnorm(bands, mean = t, sd = sigma)
    sum(values * k) / sum(k)
}

## Brute-force reference: dense trapezoid of a continuous spectrum times the
## destination Gaussian, restricted to the source coverage range.
bruteforce_resample_one = function(f, lo, hi, t, fwhm){
    sigma = fwhm / (2 * sqrt(2 * log(2)))
    grid  = seq(lo, hi, length.out = 20000)
    g     = stats::dnorm(grid, mean = t, sd = sigma)
    sum(f(grid) * g) / sum(g)
}

test_that("constant spectrum resamples to the same constant on any grid", {
    src_bands = c(seq(400, 999, by = 1.1), seq(1002, 1900, by = 3.8))
    cst       = matrix(0.42, nrow = 2, ncol = length(src_bands))
    s         = spectra(value = cst, bands = src_bands, names = c("a", "b"))

    nb = seq(500, 1800, 10)   # comfortably inside the source range
    rs = suppressWarnings(suppressMessages(resample(s, new_bands = nb, fwhm = 10)))

    expect_true(all(abs(value(rs) - 0.42) < 1e-9))
})

test_that("new model matches a brute-force integral and beats legacy at spacing jumps", {
    ## A smooth peak offset from the transition, so the spectrum has a real
    ## slope across the 1.1 -> 3.8 nm spacing jump (where the Delta-lambda bias
    ## of the legacy point-sampled kernel is worst).
    f         = function(l) exp(-((l - 1150) / 200)^2)
    src_bands = c(seq(400, 999, by = 1.1), seq(1002, 1900, by = 3.8))
    s         = spectra(value = matrix(f(src_bands), nrow = 1),
                        bands = src_bands, names = "a")

    t    = 1000                 # sits right on the 1.1 -> 3.8 nm spacing jump
    fwhm = 10

    new  = suppressWarnings(suppressMessages(
              resample(s, new_bands = t, fwhm = fwhm)))
    new_v = as.numeric(value(new))

    ## midpoint-rule boxcars span [b1 - f1/2, bn + fn/2]
    src_fwhm = spectrolab:::i_fwhm_midpoint(src_bands)
    lo  = src_bands[1] - src_fwhm[1] / 2
    hi  = src_bands[length(src_bands)] + src_fwhm[length(src_bands)] / 2
    ref = bruteforce_resample_one(f, lo, hi, t, fwhm)

    legacy = legacy_resample_one(src_bands, f(src_bands), t, fwhm)

    ## New model tracks the principled integral closely ...
    expect_lt(abs(new_v - ref) / ref, 0.003)
    ## ... and departs from the biased legacy point-sampled kernel.
    expect_gt(abs(new_v - legacy) / legacy, 0.005)
})

test_that("out-of-coverage destination bands become NA with a warning", {
    nb = seq(300, 2700, 20)   # extends below 400 and above 2500 nm

    expect_warning(rs <- resample(spec, new_bands = nb, fwhm = 20))

    v = value(rs)
    ## the requested grid is preserved, not trimmed
    expect_equal(bands(rs), nb)
    ## bands beyond the source range are NA; interior bands are finite
    expect_true(all(is.na(v[ , nb < 400])))
    expect_true(all(is.na(v[ , nb > 2500])))
    expect_true(all(is.finite(v[ , nb >= 450 & nb <= 2450])))
})

test_that("resample and make_fwhm are deterministic run to run", {
    nb = seq(500, 2400, 10)

    r1 = suppressWarnings(suppressMessages(resample(spec, new_bands = nb, fwhm = 10)))
    r2 = suppressWarnings(suppressMessages(resample(spec, new_bands = nb, fwhm = 10)))
    expect_identical(value(r1), value(r2))

    f1 = suppressWarnings(suppressMessages(make_fwhm(spec, new_bands = nb)))
    f2 = suppressWarnings(suppressMessages(make_fwhm(spec, new_bands = nb)))
    expect_identical(f1, f2)
})

test_that("fwhm and src_fwhm broadcasts validate their lengths", {
    nb = seq(500, 2400, 10)

    ## scalar and length-matched fwhm are fine
    expect_s3_class(suppressWarnings(suppressMessages(
        resample(spec, new_bands = nb, fwhm = 10))), "spectra")
    expect_s3_class(suppressWarnings(suppressMessages(
        resample(spec, new_bands = nb, fwhm = rep(10, length(nb))))), "spectra")
    ## a mismatched fwhm length errors
    expect_error(resample(spec, new_bands = nb, fwhm = c(10, 20)))

    ## scalar and per-source-band src_fwhm are fine
    expect_s3_class(suppressWarnings(suppressMessages(
        resample(spec, new_bands = nb, fwhm = 10, src_fwhm = 5))), "spectra")
    expect_s3_class(suppressWarnings(suppressMessages(
        resample(spec, new_bands = nb, fwhm = 10,
                 src_fwhm = rep(1, length(bands(spec)))))), "spectra")
    ## a mismatched src_fwhm length errors
    expect_error(resample(spec, new_bands = nb, fwhm = 10, src_fwhm = c(1, 2, 3)))
})

test_that("i_fwhm_midpoint follows the midpoint rule", {
    b = c(400, 402, 405, 409)
    expect_equal(spectrolab:::i_fwhm_midpoint(b),
                 c(2, (405 - 400) / 2, (409 - 402) / 2, 4))
})
