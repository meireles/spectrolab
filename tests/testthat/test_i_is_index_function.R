library("testthat")
library("spectrolab")

context("Internal spectrolab:::i_is_index function")

max_value             = Inf
max_value_low         = 2

num_all_whole         = c(1, 2, 3)
char_coercible        = c("1", "2", "3")
num_mix_tf            = c(1, 2, pi)

char_non_coercible    = c("a", "b", "c")
num_all_non_whole     = c(1.23, 1.3534, 2.23)
num_all_negative      = c(-2, -4, -6)
num_all_pos_neg_mix   = c(1, 3, -6)
num_has_zero          = c(0, 1, 2)

test_that("x has values that are too high", {
    expect_equal(spectrolab:::i_is_index(num_all_whole, max_value_low), c(T, T, F))
    expect_equal(spectrolab:::i_is_index(char_coercible, max_value_low), c(T, T, F))
    expect_equal(spectrolab:::i_is_index(num_all_negative, max_value_low, allow_negative = T), c(T, F, F))
})

test_that("i_is_index is given char coercible to num", {
    expect_equal(spectrolab:::i_is_index(char_coercible, max_value), c(T, T, T))
})

test_that("x has no whole numbers", {
    expect_warning(spectrolab:::i_is_index(num_all_non_whole, max_value))
    ## suppressing warnings
    expect_equal(suppressWarnings(spectrolab:::i_is_index(num_all_non_whole, max_value)),
                 c(F, F, F))
})

test_that("x has some non whole numbers or contains zero", {
    expect_equal(spectrolab:::i_is_index(num_mix_tf, max_value), c(T, T, F))
    expect_equal(spectrolab:::i_is_index(num_has_zero, max_value), c(F, T, T))
})

test_that("i_is_index is given wrong type", {
    expect_warning(spectrolab:::i_is_index(char_non_coercible, max_value))
    ## suppressing warnings
    expect_equal(suppressWarnings(spectrolab:::i_is_index(char_non_coercible, max_value)),
                 c(F, F, F))
})


test_that("spectrolab:::i_is_index is given all negative numbers", {
    expect_equal(spectrolab:::i_is_index(num_all_negative, max_value), c(F, F, F))
    expect_equal(spectrolab:::i_is_index(num_all_negative, max_value, allow_negative = T), c(T, T, T))
})


test_that("spectrolab:::i_is_index is given pos & neg mix", {
    expect_error(spectrolab:::i_is_index(num_all_pos_neg_mix, max_value))
})

