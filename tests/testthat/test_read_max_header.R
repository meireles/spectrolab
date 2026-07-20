library("testthat")
library("spectrolab")

## Regression for S1: the ASCII header-scan window is now a parameter
## (max_header_lines) instead of a hardcoded 40.

test_that("default header scan reads SVC files, a too-small window fails", {
    d = system.file("extdata", "Acer_example", package = "spectrolab")
    skip_if(d == "", "Acer_example extdata not installed")

    ## Default (40 lines) finds the "data=" tag.
    s = suppressWarnings(suppressMessages(
        read_spectra(d, format = "sig", exclude_if_matches = c("BAD", "WR"))))
    expect_s3_class(s, "spectra")

    ## A window of 3 lines cannot reach the data tag -> informative error.
    expect_error(
        suppressWarnings(suppressMessages(
            read_spectra(d, format = "sig",
                         exclude_if_matches = c("BAD", "WR"),
                         max_header_lines = 3))),
        "skip_until_tag")
})
