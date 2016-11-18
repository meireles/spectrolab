# library("testthat")
# library("spectrolab")
#
# context("Spectra names i_names() ctor")
#
# vec1_char = c("a")
# vec1_num  = c(1)
# vec1_fac  = factor(vec1_char)
#
# vec3_char = c("a", "b", "c")
# vec3_num  = c(1, 2, 3)
# vec3_fac  = factor(vec3_char)
#
# list3_char = as.list(vec3_char)
# list3_num  = as.list(vec3_num)
# list3_fac  = as.list(vec3_fac)
#
# df3  = data.frame(vec3_char, vec3_num, vec3_fac, stringsAsFactors = FALSE)
# df3u = unname(unlist(df3))
#
# test_that("i_names is given a vector of different data types", {
#     expect_equal(i_names(vec1_char), vec1_char)
#     expect_equal(i_names(vec1_num),  "1")
#     expect_equal(i_names(vec1_fac), vec1_char)
#
#     expect_equal(i_names(vec3_char), vec3_char)
#     expect_equal(i_names(vec3_num),  as.character(vec3_num))
#     expect_equal(i_names(vec3_fac), vec3_char)
# })
#
#
# test_that("i_names is given a list of different data types", {
#     expect_equal(i_names(list3_char), vec3_char)
#     expect_equal(i_names(list3_num), as.character(vec3_num))
#     expect_equal(i_names(list3_fac), vec3_char)
# })
#
#
# test_that("i_names is given a data.frame with standard subset", {
#     expect_equal(i_names(df3[ , 1]), vec3_char)
#     expect_equal(i_names(df3[ , 2]), as.character(vec3_num))
#     expect_equal(i_names(df3[ , 3]), vec3_char)
# })
#
# test_that("i_names is given a data.frame with no drop subset", {
#     expect_equal(i_names(df3[ , 1, drop = F]), vec3_char)
#     expect_equal(i_names(df3[ , 2, drop = F]), as.character(vec3_num))
#     expect_equal(i_names(df3[ , 3, drop = F]), vec3_char)
# })
#
#
# test_that("i_names is given a data.frame with no subset", {
#     expect_equal(i_names(df3), df3u)
#     expect_equal(i_names(df3), df3u)
#     expect_equal(i_names(df3), df3u)
# })
#
# test_that("i_names is given an object of the right length", {
#     expect_equal(i_names(vec3_char, nsample = 3), vec3_char)
#     expect_equal(i_names(list3_fac, nsample = 3), vec3_char)
# })
#
# test_that("i_names is given an object of the wrong length", {
#     expect_error(i_names(vec3_char, nsample = 5))
#     expect_error(i_names(list3_fac, nsample = 2))
# })
