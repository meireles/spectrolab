library("testthat")
library("spectrolab")

## Phase 1 of the match_sensors redesign: read-layer capture of detector-splice
## provenance (sensor_info) and the provenance-aware match_sensors dispatch.

## ---- SVC header parser (unit) ---------------------------------------------

test_that("i_parse_svc_overlap returns no crossovers for a Preserve header", {
    p = spectrolab:::i_parse_svc_overlap(
        "factors= 0.800, 0.844, 1.000 [Overlap: Preserve, Matching Type: None]")
    expect_true(is.na(p$splice_1) && is.na(p$splice_2))
})

test_that("i_parse_svc_overlap reads splice crossovers from a Remove header (first record only)", {
    ## Reprocessed files carry two bracketed records; only the first (applied)
    ## one should be parsed.
    p = spectrolab:::i_parse_svc_overlap(
        paste0("factors= 0.795, 0.848, 1.000 [Overlap: Remove @ 970,1901, ",
               "Matching Type: Radiance @ 976 - 1010 / NIR-SWIR On]",
               "0.800, 0.844, 1.000 [Overlap: Preserve, Matching Type: None]"))
    expect_equal(c(p$splice_1, p$splice_2), c(970, 1901))
})

test_that("i_parse_svc_overlap is robust to a missing/blank line", {
    p = spectrolab:::i_parse_svc_overlap(NA_character_)
    expect_true(is.na(p$splice_1))
    expect_true(is.na(p$splice_2))
})

## ---- provenance captured at read time -------------------------------------

test_that("SVC read captures provenance aligned to samples", {
    d = system.file("extdata", "svc_raw_and_overlap_matched_serbin", "SVC_Files",
                    package = "spectrolab")
    skip_if(d == "", "serbin raw extdata not installed")
    s  = suppressWarnings(suppressMessages(read_spectra(d, format = "sig")))
    si = sensor_info(s)

    expect_false(is.null(si))
    expect_equal(nrow(si), unname(nrow(s)))
    expect_true(all(si$instrument == "svc"))
    expect_named(si, c("instrument", "splice_1", "splice_2"))
})

test_that("SVC 'Remove @ 970,1901' matched files expose splice points", {
    d = system.file("extdata", "svc_raw_and_overlap_matched_serbin", "SVC_Files_moc",
                    package = "spectrolab")
    skip_if(d == "", "serbin moc extdata not installed")
    si = sensor_info(suppressWarnings(suppressMessages(read_spectra(d, format = "sig"))))

    expect_true(all(si$splice_1 == 970 & si$splice_2 == 1901))
})

test_that("ASD read surfaces the file's splice wavelengths", {
    asd_dir = "data_for_tests/asd"
    skip_if(length(list.files(asd_dir, pattern = "[.]asd$")) == 0, "no .asd fixtures")
    si = sensor_info(suppressWarnings(suppressMessages(
        read_spectra(asd_dir, format = "asd"))))

    expect_true(all(si$instrument == "asd"))
    expect_true(all(is.finite(si$splice_1) & is.finite(si$splice_2)))
    expect_true(all(si$splice_1 < si$splice_2))
})

test_that("a hand-built spectra has no sensor_info", {
    spec = as_spectra(spec_matrix_example, name_idx = 1)
    expect_null(sensor_info(spec))
})

## ---- provenance survives subsetting and combine ---------------------------

test_that("subsetting samples subsets sensor_info rows", {
    d = system.file("extdata", "svc_raw_and_overlap_matched_serbin", "SVC_Files",
                    package = "spectrolab")
    skip_if(d == "", "serbin raw extdata not installed")
    s = suppressWarnings(suppressMessages(read_spectra(d, format = "sig")))

    sub = s[1:3, ]
    expect_equal(nrow(sensor_info(sub)), 3L)
    expect_true(all(sensor_info(sub)$instrument == "svc"))
})

test_that("combine row-binds sensor_info", {
    d = system.file("extdata", "svc_raw_and_overlap_matched_serbin", "SVC_Files",
                    package = "spectrolab")
    skip_if(d == "", "serbin raw extdata not installed")
    s = suppressWarnings(suppressMessages(read_spectra(d, format = "sig")))

    both = suppressWarnings(suppressMessages(combine(s[1:2, ], s[3:4, ])))
    expect_equal(nrow(sensor_info(both)), 4L)
})

## ---- provenance-aware match_sensors dispatch ------------------------------

test_that("match_sensors() no-ops when bands are already increasing", {
    spec = as_spectra(spec_matrix_example, name_idx = 1)   # already monotonic
    expect_message(out <- match_sensors(spec), "already")
    expect_identical(dim(out), dim(spec))
    expect_identical(value(out), value(spec))
})

test_that("match_sensors() errors on non-monotonic data with no splice provenance", {
    d = system.file("extdata", "svc_raw_and_overlap_matched_serbin", "SVC_Files",
                    package = "spectrolab")
    skip_if(d == "", "serbin raw extdata not installed")
    s = suppressWarnings(suppressMessages(read_spectra(d, format = "sig")))  # Preserve: no header splice

    expect_false(spectrolab:::i_is_increasing(bands(s)))
    expect_error(suppressMessages(match_sensors(s)), "splice_at")
})

test_that("explicit splice_at still works and bypasses provenance", {
    d = system.file("extdata", "svc_raw_and_overlap_matched_serbin", "SVC_Files",
                    package = "spectrolab")
    skip_if(d == "", "serbin raw extdata not installed")
    s = suppressWarnings(suppressMessages(read_spectra(d, format = "sig")))

    m = suppressWarnings(suppressMessages(match_sensors(s, splice_at = guess_splice_at(s))))
    expect_true(spectrolab:::i_is_increasing(bands(m)))
})

test_that("print.spectra reports the instrument when provenance is present", {
    d = system.file("extdata", "svc_raw_and_overlap_matched_serbin", "SVC_Files",
                    package = "spectrolab")
    skip_if(d == "", "serbin raw extdata not installed")
    s   = suppressWarnings(suppressMessages(read_spectra(d, format = "sig")))
    out = paste(utils::capture.output(print(s)), collapse = "\n")
    expect_match(out, "instrument:\\s*svc")
})
