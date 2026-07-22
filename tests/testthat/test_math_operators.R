library("testthat")
library("spectrolab")

context("Ops.spectra strict-shape arithmetic and comparison")

s = as_spectra(spec_matrix_example, name_idx = 1)
V = value(s)
n = nrow(V)      # plain matrix dims (unnamed; nrow(s)/ncol(s) carry dim names)
m = ncol(V)

test_that("scalar arithmetic works and preserves the object", {
    expect_true(is_spectra(s * 2))
    expect_equal(value(s * 2), V * 2)
    expect_equal(value(s + 2), V + 2)
    expect_equal(value(2 - s), 2 - V)          # non-commutative, spectra second
    expect_equal(value(s / 2), V / 2)
    expect_equal(value(s ^ 2), V ^ 2)
})

test_that("unary minus negates the values", {
    expect_equal(value(-s), -V)
})

test_that("a length-m vector broadcasts per band (same value to every sample)", {
    v = seq_len(m)
    expect_equal(value(s + v), sweep(V, 2, v, "+"))
    expect_equal(value(v * s), sweep(V, 2, v, "*"))    # spectra second (commutative)
})

test_that("a length-n vector broadcasts per sample (same value across every band)", {
    v = seq_len(n)
    expect_equal(value(s + v), sweep(V, 1, v, "+"))
    expect_equal(value(s - v), sweep(V, 1, v, "-"))
})

test_that("explicit 1xm and nx1 matrices force the broadcast orientation", {
    vb = matrix(seq_len(m), nrow = 1)                  # 1 x m -> per band
    vs = matrix(seq_len(n), ncol = 1)                  # n x 1 -> per sample
    expect_equal(value(s + vb), sweep(V, 2, seq_len(m), "+"))
    expect_equal(value(s + vs), sweep(V, 1, seq_len(n), "+"))
})

test_that("a full n x m matrix operates elementwise", {
    M = matrix(seq_len(n * m), n, m)
    expect_equal(value(s + M), V + M)
    expect_equal(value(M - s), M - V)                  # spectra second
})

test_that("incompatible shapes error instead of silently recycling", {
    expect_error(s + seq_len(m + 1), "incompatible")   # length not 1, n, or m
    expect_error(s * matrix(1, n + 1, m), "incompatible")
})

test_that("a bare length-n vector on a square spectra is read as per-sample", {
    sq = as_spectra(matrix(1:9 + 0.0, 3, 3,
                    dimnames = list(c("a", "b", "c"), c("400", "500", "600"))))
    v  = c(10, 20, 30)
    ## per-sample (each row + v[i]), NOT per-band
    expect_equal(value(sq + v), sweep(value(sq), 1, v, "+"))
})

test_that("comparison operators return a labeled logical matrix", {
    cmp = s > 0.2
    expect_true(is.matrix(cmp) && is.logical(cmp))
    expect_equal(dim(cmp), c(n, m))
    expect_equal(rownames(cmp), as.character(names(s)))
    expect_equal(colnames(cmp), as.character(bands(s)))
    expect_equal(unname(cmp), unname(V > 0.2))
})

test_that("comparison operators obey the same shape rules", {
    v = seq_len(m)
    expect_equal(unname(s > v), unname(sweep(V, 2, v, ">")))
    expect_error(s == seq_len(m + 1), "incompatible")
})

test_that("spectra OP spectra operates elementwise and requires identical bands", {
    expect_equal(value(s + s), V + V)
    expect_true(all(s == s))

    s2 = s
    bands(s2) = bands(s2) + 1
    expect_error(s + s2, "band labels")
})

test_that("spectra + spectra with differing sample names clears names with a warning", {
    a = s
    b = s
    names(b)[1] = "a_different_name_xyz"
    expect_warning(out <- a + b, "sample names")
    expect_true(all(is.na(names(out))))
})
