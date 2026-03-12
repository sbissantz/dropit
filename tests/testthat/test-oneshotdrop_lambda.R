# ==============================================================================
# tests/testthat/test-oneshotdrop-lambda.R 
# ==============================================================================

library(psych)
library(lavaan)
library(testthat)

# Agreeableness
dta <- psych::bfi[, 1:5]  

test_that("oneshotdrop_lambda() returns manually debugged output", {
res <- oneshotdrop_lambda(
  dta,
  n_drp = 4,
  dir = "tail",
  out = "names",
  mmt_mdl = NULL,
  tgt_fct = NULL,
  lam_mtr = "std.all",
  cfa_args = list(std.lv = TRUE)
)
expect_equal(res, c("A2", "A5", "A4", "A1"))
})

test_that("oneshotdrop_lambda() errors with custom measurement model", {
expect_error(
  oneshotdrop_lambda(
    dta,
    n_drp = 1,
    dir = "tail",
    out = "subset",
    mmt_mdl = "F1 =~ A1 + A2 + A3 + A4 + A5", # Not yet supported
    tgt_fct = NULL, # Not yet supported
    lam_mtr = "std.all",
    cfa_args = list(std.lv = TRUE)
  )
)
})

test_that("oneshotdrop_lambda() errors with specified target_factor", {
expect_error(
  oneshotdrop_lambda(
    dta,
    n_drp = 1,
    dir = "tail",
    out = "subset",
    mmt_mdl = NULL, # Not yet supported
    tgt_fct = "F1", # Not yet supported 
    lam_mtr = "std.all",
    cfa_args = list(std.lv = TRUE)
  )
)
})

test_that("oneshotdrop_lambda() returns character when out = 'names'", {
  res <- oneshotdrop_lambda(
    dta,
    n_drp = 1,
    dir = "tail",
    out = "names",
    mmt_mdl = NULL,
    tgt_fct = NULL,
    lam_mtr = "std.all",
    cfa_args = list(std.lv = TRUE)
  )
  expect_type(res, "character")
})

test_that("oneshotdrop_lambda() returns data.frame when out = 'subset'", {
  res <- oneshotdrop_lambda(
    dta,
    n_drp = 1,
    dir = "tail",
    out = "subset",
    mmt_mdl = NULL,
    tgt_fct = NULL,
    lam_mtr = "std.all",
    cfa_args = list(std.lv = TRUE)
  )
  expect_s3_class(res, "data.frame")
})

test_that("oneshotdrop_lambda() put = 'both' returns two-element list with 'names' and 'subset'", {
  res <- oneshotdrop_lambda(
    dta,
    n_drp = 1,
    dir = "tail",
    out = "both",
    mmt_mdl = NULL,
    tgt_fct = NULL,
    lam_mtr = "std.all",
    cfa_args = list(std.lv = TRUE)
  )
  expect_type(res, "list")
  expect_named(res, c("names", "subset"))
})

test_that("oneshotdrop_lambda() dropped name is not a column name in subset", {
  res <- oneshotdrop_lambda(
    dta,
    n_drp = 1,
    dir = "tail",
    out = "both",
    mmt_mdl = NULL,
    tgt_fct = NULL,
    lam_mtr = "std.all",
    cfa_args = list(std.lv = TRUE)
  )
  expect_false(res$names %in% colnames(res$subset))
})

test_that("oneshotdrop_lambda() returns subset has expected number of columns", {
  res <- oneshotdrop_lambda(
    dta,
    n_drp = 2,
    dir = "tail",
    out = "subset",
    mmt_mdl = NULL,
    tgt_fct = NULL,
    lam_mtr = "std.all",
    cfa_args = list(std.lv = TRUE)
  )
  expect_equal(ncol(res), ncol(dta) - 2)
})

test_that("oneshotdrop_lambda() dir='tail' with multiple drops removes correct number of items", {
  res <- oneshotdrop_lambda(
    dta,
    n_drp = 2,
    dir = "tail",
    out = "names",
    mmt_mdl = NULL,
    tgt_fct = NULL,
    lam_mtr = "std.all",
    cfa_args = list(std.lv = TRUE)
  )
  expect_length(res, 2)
})

test_that("oneshotdrop_lambda() dir='head' with multiple drops removes correct number of items", {
  res <- oneshotdrop_lambda(
    dta,
    n_drp = 2,
    dir = "head",
    out = "names",
    mmt_mdl = NULL,
    tgt_fct = NULL,
    lam_mtr = "std.all",
    cfa_args = list(std.lv = TRUE)
  )
  expect_length(res, 2)
})

test_that("oneshotdrop_lambda() works with lam_mtr = 'std.all'", {
  res <- oneshotdrop_lambda(
    dta,
    n_drp = 1,
    dir = "tail",
    out = "names",
    mmt_mdl = NULL,
    tgt_fct = NULL,
    lam_mtr = "std.all",
    cfa_args = list(std.lv = TRUE)
  )
  expect_type(res, "character")
})

test_that("oneshotdrop_lambda() works with lam_mtr = 'est'", {
  res <- oneshotdrop_lambda(
    dta,
    n_drp = 1,
    dir = "tail",
    out = "names",
    mmt_mdl = NULL,
    tgt_fct = NULL,
    lam_mtr = "est",
    cfa_args = list(std.lv = TRUE)
  )
  expect_type(res, "character")
})

test_that("oneshotdrop_lambda() throws error if lam_mtr invalid", {
  expect_error(
    oneshotdrop_lambda(
      dta,
      n_drp = 1,
      dir = "tail",
      out = "names",
      mmt_mdl = NULL,
      tgt_fct = NULL,
      lam_mtr = "foobar",
      cfa_args = list(std.lv = TRUE)
    )
  )
})

test_that("oneshotdrop_lambda() n_drp = 0 returns empty character vector", {
  res <- oneshotdrop_lambda(
    dta,
    n_drp = 0,
    dir = "tail",
    out = "names",
    mmt_mdl = NULL,
    tgt_fct = NULL,
    lam_mtr = "std.all",
    cfa_args = list(std.lv = TRUE)
  )
  expect_equal(res, character(0))
})

test_that("oneshotdrop_lambda() n_drp = ncol(data) drops all columns", {
  res <- oneshotdrop_lambda(
    dta,
    n_drp = ncol(dta),
    dir = "tail",
    out = "names",
    mmt_mdl = NULL,
    tgt_fct = NULL,
    lam_mtr = "std.all",
    cfa_args = list(std.lv = TRUE)
  )
  expect_equal(length(res), ncol(dta))
})

test_that("oneshotdrop_lambda() errors cleanly on invalid 'dir'", {
  expect_error(
    oneshotdrop_lambda(
      dta,
      n_drp = 1,
      dir = "foobar",
      out = "names",
      mmt_mdl = NULL,
      tgt_fct = NULL,
      lam_mtr = "std.all",
      cfa_args = list(std.lv = TRUE)
    )
  )
})

test_that("oneshotdrop_lambda() errors cleanly on invalid 'out'", {
  expect_error(
    oneshotdrop_lambda(
      dta,
      n_drp = 1,
      dir = "tail",
      out = "foobar",
      mmt_mdl = NULL,
      tgt_fct = NULL,
      lam_mtr = "std.all",
      cfa_args = list(std.lv = TRUE)
    )
  )
})