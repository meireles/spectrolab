library("testthat")
library("spectrolab")

context("Tier 3 regression fixes")

s = spectra(matrix(as.numeric(1:12), 4),
            bands = c(500, 600, 700),
            names = letters[1:4],
            meta  = data.frame(site = c("a", "b", "c", "d"), yr = c(1, 2, 3, 4)))

test_that("empty sample selection errors clearly (no 'object r not found')", {
    err = tryCatch(s[integer(0), ], error = function(e) conditionMessage(e))
    expect_match(err, "empty selection", fixed = TRUE)
    expect_false(grepl("object 'r' not found", err))
})

test_that("meta() with an unknown label warns and returns the matched columns", {
    expect_warning(m <- meta(s, c("site", "typo")), "not found")
    expect_equal(colnames(m), "site")
})

test_that("meta(quiet = FALSE) errors on an unknown label", {
    expect_error(suppressWarnings(meta(s, "typo", quiet = FALSE)), "not found")
})

test_that("meta() never silently returns NULL on a bad label", {
    expect_false(is.null(suppressWarnings(meta(s, "typo"))))
})

test_that("meta() still supports numeric column index and NULL (all)", {
    expect_equal(colnames(meta(s, 2)), "yr")
    expect_equal(ncol(meta(s)), 2L)
})

test_that("a routine bad-name lookup does not leak the i_is_index warning", {
    seen = FALSE
    withCallingHandlers(
        suppressWarnings(try(s["nosuch", ], silent = TRUE)),
        warning = function(w){
            if(grepl("whole number", conditionMessage(w))) seen <<- TRUE
        }
    )
    expect_false(seen)
})

test_that("logical band mask returns one column, not both duplicated bands", {
    sd_ = spectra(matrix(1:4, 1), bands = c(500, 970, 970, 1000), names = "a")
    sel = sd_[ , c(FALSE, TRUE, FALSE, FALSE), simplify = FALSE]
    expect_equal(unname(ncol(sel)), 1L)
    expect_equal(bands(sel), 970)
})
