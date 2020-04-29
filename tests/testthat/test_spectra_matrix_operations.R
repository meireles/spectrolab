################################################################################
## Put on hold for now. See R/math_operator_overload.R for details
################################################################################

# library("testthat")
# library("spectrolab")
#
# context("Spectra matrix operations")
#
# spec   = as.spectra(spec_matrix_example)
# ones_c = rep.int(1, ncol(spec))
# ones_r = rep.int(1, nrow(spec))
#
# test_that("spectra mat multiplication", {
#     expect_equivalent( value(spec) %*% ones_c, spec %*% ones_c )
#     expect_equivalent( ones_r %*% value(spec), ones_r %*% spec )
# })
