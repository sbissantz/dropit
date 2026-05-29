# ==============================================================================
# tests/testthat/test-oneshotdrop-alpha.R
# ==============================================================================

dta <- psych::bfi[, 1:5]  

test_that("oneshotdrop_alpha() returns manually debugged result", {
  res <- oneshotdrop_alpha(
    dta,
    anc = NULL,
    n_drp = 4,
    dir = "tail",
    alp_mtr = "raw_alpha",
    alp_args = list(check.keys = TRUE)
  )
  expect_type(res, "list")
  expect_named(res, c("names", "subset"))
  expect_equal(res$names, c("A4", "A5", "A2", "A3"))
  expect_s3_class(res$subset, "data.frame")
})

test_that("oneshotdrop_alpha() dropped name is not a column name in subset", {
  res <- oneshotdrop_alpha(
    dta,
    anc = NULL,
    n_drp = 1,
    dir = "tail",
    alp_mtr = "raw_alpha",
    alp_args = list(check.keys = TRUE)
  )
  expect_false(res$names %in% colnames(res$subset))
})

test_that("oneshotdrop_alpha() removes correct number of items with dir='tail'  ", {
  res <- oneshotdrop_alpha(
    dta,
    anc = NULL,
    n_drp = 2,
    dir = "tail",
    alp_mtr = "raw_alpha",
    alp_args = list(check.keys = TRUE)
  )
  expect_length(res$names, 2)
  expect_equal(ncol(res$subset), ncol(dta) - 2)
})

test_that("oneshotdrop_alpha() works with alp_mtr = 'std.alpha'", {
  res <- oneshotdrop_alpha(
    dta,
    anc = NULL,
    n_drp = 1,
    dir = "tail",
    alp_mtr = "std.alpha",
    alp_args = list(check.keys = TRUE)
  )
  expect_type(res$names, "character")
})

test_that("oneshotdrop_alpha() n_drp = 0 returns empty character vector", {
  res <- oneshotdrop_alpha(
    dta,
    anc = NULL,
    n_drp = 0,
    dir = "tail",
    alp_mtr = "raw_alpha",
    alp_args = list(check.keys = TRUE)
  )
  expect_equal(res$names, character(0))
})

test_that("oneshotdrop_alpha() throws errors if 'dir' is invalid", {
  expect_error(oneshotdrop_alpha(
    dta,
    anc = NULL,
    n_drp = 1,
    dir = "foobar",
    alp_mtr = "raw_alpha",
    alp_args = list(check.keys = TRUE)
  ))
})

test_that("oneshotdrop_alpha() throws error if 'alp_mtr' is invalid", {
  expect_error(oneshotdrop_alpha(
    dta,
    anc = NULL,
    n_drp = 1,
    dir = "tail",
    alp_mtr = "foobar",
    alp_args = list(check.keys = TRUE)
  ))
})

test_that("oneshotdrop_alpha() respects anchor items", {
  res <- oneshotdrop_alpha(
    dta = dta,
    anc = "A3",
    n_drp = 2,
    dir = "tail",
    alp_mtr = "raw_alpha",
    alp_args = list(check.keys = TRUE)
  )
  expect_false("A3" %in% res$names)
  expect_equal(res$names, c("A5", "A2"))
})