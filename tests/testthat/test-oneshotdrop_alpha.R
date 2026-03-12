# ==============================================================================
# tests/testthat/test-oneshotdrop-alpha.R
# ==============================================================================

library(psych)
library(testthat)

# Agreeableness
dta <- psych::bfi[, 1:5]  

test_that("oneshotdrop_alpha() returns manually debugged output", {
res <- oneshotdrop_alpha(
  dta,
  n_drp = 4,
  dir = "tail",
  out = "names",
  alp_mtr = "raw_alpha",
  alp_args = list(check.keys = TRUE)
)
expect_equal(res, c("A4", "A5", "A2", "A3"))
})

test_that("oneshotdrop_alpha() returns character when out = 'names'", {
  res <- oneshotdrop_alpha(
    dta,
    n_drp = 1,
    dir = "tail",
    out = "names",
    alp_mtr = "raw_alpha",
    alp_args = list(check.keys = TRUE)
  )
  expect_type(res, "character")
})

test_that("oneshotdrop_alpha() returns data.frame when out = 'subset'", {
  res <- oneshotdrop_alpha(
    dta,
    n_drp = 1,
    dir = "tail",
    out = "subset",
    alp_mtr = "raw_alpha",
    alp_args = list(check.keys = TRUE)
  )
  expect_s3_class(res, "data.frame")
})

test_that("oneshotdrop_alpha() out='both' returns two-element list with 'names' and 'subset'", {
  res <- oneshotdrop_alpha(
    dta,
    n_drp = 1,
    dir = "tail",
    out = "both",
    alp_mtr = "raw_alpha",
    alp_args = list(check.keys = TRUE)
  )
  expect_type(res, "list")
  expect_named(res, c("names", "subset"))
})

test_that("oneshotdrop_alpha() dropped name is not a column name in subset", {
  res <- oneshotdrop_alpha(
    dta,
    n_drp = 1,
    dir = "tail",
    out = "both",
    alp_mtr = "raw_alpha",
    alp_args = list(check.keys = TRUE)
  )
  expect_false(res$names %in% colnames(res$subset))
})

test_that("oneshotdrop_alpha() returns subset with expected number of columns", {
  res <- oneshotdrop_alpha(
    dta,
    n_drp = 2,
    dir = "tail",
    out = "subset",
    alp_mtr = "raw_alpha",
    alp_args = list(check.keys = TRUE)
  )
  expect_equal(ncol(res), ncol(dta) - 2)
})

test_that("oneshotdrop_alpha() removes correct number of items with dir='tail'  ", {
  res <- oneshotdrop_alpha(
    dta,
    n_drp = 2,
    dir = "tail",
    out = "names",
    alp_mtr = "raw_alpha",
    alp_args = list(check.keys = TRUE)
  )
  expect_length(res, 2)
})

test_that("oneshotdrop_alpha() removes corrects number of items with dir='head' ", {
  res <- oneshotdrop_alpha(
    dta,
    n_drp = 2,
    dir = "head",
    out = "names",
    alp_mtr = "raw_alpha",
    alp_args = list(check.keys = TRUE)
  )
  expect_length(res, 2)
})

test_that("oneshotdrop_alpha() works with alp_mtr = 'raw_alpha'", {
  res <- oneshotdrop_alpha(
    dta,
    n_drp = 1,
    dir = "tail",
    out = "names",
    alp_mtr = "raw_alpha",
    alp_args = list(check.keys = TRUE)
  )
  expect_type(res, "character")
})

test_that("oneshotdrop_alpha() works with alp_mtr = 'std.alpha'", {
  res <- oneshotdrop_alpha(
    dta,
    n_drp = 1,
    dir = "tail",
    out = "names",
    alp_mtr = "std.alpha",
    alp_args = list(check.keys = TRUE)
  )
  expect_type(res, "character")
})

test_that("oneshotdrop_alpha() n_drp = 0 returns empty character vector", {
  res <- oneshotdrop_alpha(
    dta,
    n_drp = 0,
    dir = "tail",
    out = "names",
    alp_mtr = "raw_alpha",
    alp_args = list(check.keys = TRUE)
  )
  expect_equal(res, character(0))
})

test_that("oneshotdrop_alpha() n_drp = ncol(data) drops all columns", {
  res <- oneshotdrop_alpha(
    dta,
    n_drp = ncol(dta),
    dir = "tail",
    out = "names",
    alp_mtr = "raw_alpha",
    alp_args = list(check.keys = TRUE)
  )
  res
  expect_equal(length(res), ncol(dta))
})


test_that("oneshotdrop_alpha() throws errors if 'dir' is invalid", {
  expect_error(oneshotdrop_alpha(
    dta,
    n_drp = 1,
    dir = "foobar",
    out = "names",
    alp_mtr = "raw_alpha",
    alp_args = list(check.keys = TRUE)
  ))
})

test_that("oneshotdrop_alpha() throws error if 'alp_mtr' is invalid", {
  expect_error(oneshotdrop_alpha(
    dta,
    n_drp = 1,
    dir = "tail",
    out = "names",
    alp_mtr = "foobar",
    alp_args = list(check.keys = TRUE)
  ))
})

test_that("oneshotdrop_alpha() throws error when 'out' is invalid", {
  expect_error(oneshotdrop_alpha(
    dta,
    n_drp = 1,
    dir = "tail",
    out = "foobar",
    alp_mtr = "raw_alpha",
    alp_args = list(check.keys = TRUE)
  ))
})
