library("testthat")
library("spectrolab")

context("Spectra names spectrolab:::i_names() ctor")

vec1_char = c("a")
vec1_num  = c(1)
vec1_fac  = factor(vec1_char)

vec3_char = c("a", "b", "c")
vec3_num  = c(1, 2, 3)
vec3_fac  = factor(vec3_char)

list3_char = as.list(vec3_char)
list3_num  = as.list(vec3_num)
list3_fac  = as.list(vec3_fac)

df3  = data.frame(vec3_char, vec3_num, vec3_fac, stringsAsFactors = FALSE)
df3u = unname(unlist(df3))

pfx  = "spec_"

test_that("spectrolab:::i_names is given a vector of different data types", {
    expect_equal(spectrolab:::i_names(vec1_char, prefix = pfx), vec1_char)
    expect_equal(spectrolab:::i_names(vec1_num, prefix = pfx),  paste(pfx, "1", sep = "")  )
    expect_equal(spectrolab:::i_names(vec1_fac, prefix = pfx), vec1_char)

    expect_equal(spectrolab:::i_names(vec3_char, prefix = pfx), vec3_char)
    expect_equal(spectrolab:::i_names(vec3_num, prefix = pfx),  paste(pfx, as.character(vec3_num), sep = ""))
    expect_equal(spectrolab:::i_names(vec3_fac, prefix = pfx), vec3_char)
})


test_that("spectrolab:::i_names is given a list of different data types", {
    expect_equal(spectrolab:::i_names(list3_char, prefix = pfx), vec3_char)
    expect_equal(spectrolab:::i_names(list3_num, prefix = pfx), paste(pfx, as.character(vec3_num), sep = ""))
    expect_equal(spectrolab:::i_names(list3_fac, prefix = pfx), vec3_char)
})


test_that("spectrolab:::i_names is given a data.frame with standard subset", {
    expect_equal(spectrolab:::i_names(df3[ , 1], prefix = pfx), vec3_char)
    expect_equal(spectrolab:::i_names(df3[ , 2], prefix = pfx), paste(pfx, as.character(vec3_num), sep = ""))
    expect_equal(spectrolab:::i_names(df3[ , 3], prefix = pfx), vec3_char)
})

test_that("spectrolab:::i_names throws if given a data.frame with no drop subset", {
    expect_error(spectrolab:::i_names(df3[ , 1, drop = F]), prefix = pfx)
    expect_error(spectrolab:::i_names(df3[ , 2, drop = F]), prefix = pfx)
    expect_error(spectrolab:::i_names(df3[ , 3, drop = F]), prefix = pfx)
})


test_that("spectrolab:::i_names throws if given a data.frame with no subset", {
    expect_error(spectrolab:::i_names(df3), prefix = pfx)
    expect_error(spectrolab:::i_names(df3), prefix = pfx)
    expect_error(spectrolab:::i_names(df3), prefix = pfx)
})

test_that("spectrolab:::i_names is given an object of the right length", {
    expect_equal(spectrolab:::i_names(vec3_char, nsample = 3, prefix = pfx), vec3_char)
    expect_equal(spectrolab:::i_names(list3_fac, nsample = 3, prefix = pfx), vec3_char)
})

test_that("spectrolab:::i_names throws if given an object of the wrong length", {
    expect_error(spectrolab:::i_names(vec3_char, nsample = 5, prefix = pfx))
    expect_error(spectrolab:::i_names(list3_fac, nsample = 2, prefix = pfx))
})
