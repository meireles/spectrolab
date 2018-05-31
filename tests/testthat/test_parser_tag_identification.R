library("testthat")
library("spectrolab")

## Relies on testthat setting "tests/testthat" as the working directory!!!

f_svc_added   = "data_for_tests/data_svc_added_lines.sig"
f_svc_removed = "data_for_tests/data_svc_removed_lines.sig"
f_psr_added   = "data_for_tests/data_psr_added_lines.sed"
f_psr_removed = "data_for_tests/data_psr_removed_lines.sed"

context("Parse svc and psr files by finding a data tag")

test_that("parser reads longer svc file", {
    expect_s3_class(read_spectra(f_svc_added, "svc"), class = "spectra" )
})

test_that("parser reads shorter svc file", {
    expect_s3_class(read_spectra(f_svc_removed, "svc"), class = "spectra" )
})


test_that("parser reads longer psr file", {
    expect_s3_class(read_spectra(f_psr_added, "sed"), class = "spectra" )
})

test_that("parser reads shorter psr file", {
    expect_s3_class(read_spectra(f_psr_removed, "sed"), class = "spectra" )
})

