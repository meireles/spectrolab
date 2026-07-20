library("testthat")
library("spectrolab")

context("Minimal provenance: quantity and wavelength_unit")

spec = as_spectra(spec_matrix_example, name_idx = 1)

test_that("quantity/wavelength_unit default sensibly on objects without provenance", {
    expect_true(is.na(quantity(spec)))
    expect_equal(wavelength_unit(spec), "nm")
})

test_that("quantity<-/wavelength_unit<- set and read back", {
    s = spec
    quantity(s)        = "reflectance"
    wavelength_unit(s)  = "nm"
    expect_equal(quantity(s), "reflectance")
    expect_equal(wavelength_unit(s), "nm")
})

test_that("quantity<- validates its input", {
    s = spec
    expect_error(quantity(s) <- c("reflectance", "radiance"))
    expect_error(quantity(s) <- 1)
})

test_that("read_spectra sets quantity from `type` and wavelength_unit to nm", {
    dir_path = system.file("extdata/Acer_example", package = "spectrolab")

    refl = read_spectra(dir_path, format = "sig", type = "target_reflectance")
    rad  = read_spectra(dir_path, format = "sig", type = "target_radiance")
    ref_rad = read_spectra(dir_path, format = "sig", type = "reference_radiance")

    expect_equal(quantity(refl), "reflectance")
    expect_equal(quantity(rad), "radiance")
    expect_equal(quantity(ref_rad), "radiance")

    expect_equal(wavelength_unit(refl), "nm")
})

test_that("print.spectra shows quantity when known and omits it when unknown", {
    dir_path = system.file("extdata/Acer_example", package = "spectrolab")
    refl = read_spectra(dir_path, format = "sig")

    out_known   = paste(utils::capture.output(print(refl)), collapse = "\n")
    out_unknown = paste(utils::capture.output(print(spec)), collapse = "\n")

    expect_match(out_known, "quantity:\\s*reflectance")
    expect_false(grepl("quantity:", out_unknown))
})

test_that("subsetting carries quantity/wavelength_unit through", {
    s = spec
    quantity(s) = "reflectance"

    sub = s[1:3, ]
    expect_equal(quantity(sub), "reflectance")
    expect_equal(wavelength_unit(sub), wavelength_unit(s))
})

test_that("combine() keeps provenance when it agrees, clears it (with a warning) when it doesn't", {
    s1 = s2 = spec
    quantity(s1) = "reflectance"
    quantity(s2) = "reflectance"
    out = combine(s1, s2)
    expect_equal(quantity(out), "reflectance")

    quantity(s2) = "radiance"
    expect_warning(out2 <- combine(s1, s2), "quantity")
    expect_true(is.na(quantity(out2)))
})

test_that("apply_by_band/aggregate/resample carry quantity/wavelength_unit through", {
    s = spec
    quantity(s) = "reflectance"

    ab = apply_by_band(s, mean)
    expect_equal(quantity(ab), "reflectance")

    ag = suppressMessages(aggregate(s, by = names(s), FUN = mean))
    expect_equal(quantity(ag), "reflectance")

    new_bands = seq(400, 2400, 10)
    rs = resample(s, new_bands = new_bands, fwhm = make_fwhm(s, new_bands))
    expect_equal(quantity(rs), "reflectance")
})

test_that("deriv_spectra() and continuum_removal() clear quantity", {
    skip_if_not_installed("signal")

    s = spec
    quantity(s) = "reflectance"

    d = suppressMessages(deriv_spectra(s, order = 1))
    expect_true(is.na(quantity(d)))

    cr = continuum_removal(s)
    expect_true(is.na(quantity(cr)))
})

context("Ops.spectra: metadata handling, unary operators, and provenance")

test_that("unary +/- work on spectra", {
    neg = -spec
    expect_s3_class(neg, "spectra")
    expect_equal(value(neg), -value(spec))

    pos = +spec
    expect_equal(value(pos), value(spec))

    expect_error(!spec)
})

test_that("metadata is kept when it's identical between both operands", {
    s1 = s2 = spec
    meta(s1, "src") = "a"
    meta(s2, "src") = "a"
    r = s1 + s2
    expect_equal(meta(r, "src", simplify = TRUE), meta(s1, "src", simplify = TRUE))
})

test_that("metadata is cleared (with a warning) when it differs between operands", {
    s1 = s2 = spec
    meta(s1, "src") = "a"
    meta(s2, "src") = "b"
    expect_warning(r <- s1 + s2, "metadata")
    expect_equal(ncol(meta(r)), 0)
})

test_that("Ops.spectra reconciles quantity/wavelength_unit the same way combine() does", {
    s1 = s2 = spec
    quantity(s1) = "reflectance"
    quantity(s2) = "radiance"
    expect_warning(out <- s1 + s2, "quantity")
    expect_true(is.na(quantity(out)))
})
