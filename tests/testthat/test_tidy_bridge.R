library("testthat")
library("spectrolab")

spec = as_spectra(spec_matrix_example, name_idx = 1)

context("Tidy / long bridge")

test_that("to_long reshapes spectra into one row per sample/band", {
    long = to_long(spec)
    expect_s3_class(long, "data.frame")
    expect_equal(nrow(long), unname(nrow(spec) * ncol(spec)))
    expect_setequal(names(long), c("sample_name", "band", "value"))
})

test_that("to_long values match the original value matrix", {
    long = to_long(spec)
    v    = value(spec)
    ## row 1 of a column-major reshape is always sample 1, band 1 -- matching
    ## on sample_name/band directly is unsafe because sample names may repeat
    expect_equal(long$sample_name[1], names(spec)[1])
    expect_equal(long$band[1], bands(spec)[1])
    expect_equal(long$value[1], v[1, 1])
})

test_that("to_long drops metadata when metadata = FALSE", {
    long_meta   = to_long(spec, metadata = TRUE)
    long_nometa = to_long(spec, metadata = FALSE)
    expect_equal(names(long_nometa), c("sample_name", "band", "value"))
    expect_true(ncol(long_meta) >= ncol(long_nometa))
})

test_that("as_tibble.spectra returns a tibble matching to_long", {
    skip_if_not_installed("tibble")
    tbl = tibble::as_tibble(spec)
    expect_true(inherits(tbl, "tbl_df"))
    expect_equal(as.data.frame(tbl), to_long(spec), ignore_attr = TRUE)
})

test_that("autoplot.spectra returns a ggplot object", {
    skip_if_not_installed("ggplot2")
    p = ggplot2::autoplot(spec)
    expect_s3_class(p, "ggplot")
})
