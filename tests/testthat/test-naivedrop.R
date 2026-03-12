# ==============================================================================
# tests/testthat/test-naivedrop-alpha.R 
# ==============================================================================

library(psych)
library(lavaan)
library(testthat)

dta <- psych::bfi[, 1:5]

test_that("naivedrop() inherits debugged output from oneshotdrop_alpha()", {
res <- naivedrop(
  dta,
  n_drp = 4,
  dir = "tail",
  crt = "alpha",
  apr = "oneshot",
  out = "names",
  alp_mtr = "raw_alpha",
  alp_args = list(check.keys = TRUE)
)
expect_equal(res, c("A4", "A5", "A2", "A3"))
})

test_that("naivedrop() inherits debugged output from oneshotdrop_lambda()", {
  res <- naivedrop(
    dta = dta,
    n_drp = 4,
    dir = "tail",
    crt = "lambda",
    apr = "oneshot",
    out = "names",
    mmt_mdl = NULL,
    tgt_fct = NULL,
    lam_mtr = "std.all",
    cfa_args = list(std.lv = TRUE) 
  )
  expect_equal(res, c("A2", "A5", "A4", "A1"))
})

test_that("naivedrop() inherits debugged output from greedydrop_alpha()", {
res <- naivedrop(
  dta,
  n_drp = 4,
  dir = "tail",
  crt = "alpha",
  apr = "greedy",
  out = "names",
  alp_mtr = "raw_alpha",
  alp_args = list(check.keys = TRUE)
)
expect_equal(res, c("A3", "A2", "A5", "A1"))
})

test_that("naivedrop() inherits debugged output from greedydrop_lambda()", {
  res <- naivedrop(
    dta = dta,
    n_drp = 3,
    dir = "tail",
    crt = "lambda",
    apr = "greedy",
    out = "names",
    mmt_mdl = NULL,
    tgt_fct = NULL,
    lam_mtr = "std.all",
    cfa_args = list(std.lv = TRUE)
  )
expect_equal(res, c("A1", "A4", "A2"))
})

test_that("naivedrop() dispatches correctly for alpha-greedy", {
  res <- naivedrop(
    dta = dta,
    n_drp = 1,
    dir = "tail",
    crt = "alpha",
    apr = "greedy",
    out = "names",
    alp_mtr = "raw_alpha",
    alp_args = list(check.keys = TRUE)
  )
  expect_type(res, "character")
})

test_that("naivedrop() dispatches correctly for lambda-oneshot", {
  res <- naivedrop(
    dta = dta,
    n_drp = 1,
    dir = "tail",
    crt = "lambda",
    apr = "oneshot",
    out = "names",
    alp_mtr = "raw_alpha",
    alp_args = list(check.keys = TRUE),
    mmt_mdl = NULL,
    tgt_fct = NULL,
    lam_mtr = "std.all",
    cfa_args = list(std.lv = TRUE) 
  )
  expect_type(res, "character")
})

test_that("naivedrop() dispatches correctly for lambda-greedy", {
  res <- naivedrop(
    dta = dta,
    n_drp = 1,
    dir = "tail",
    crt = "lambda",
    apr = "greedy",
    out = "names",
    alp_mtr = "raw_alpha",
    alp_args = list(check.keys = TRUE),
    mmt_mdl = NULL,
    tgt_fct = NULL,
    lam_mtr = "std.all",
    cfa_args = list(std.lv = TRUE) 
  )
  expect_type(res, "character")
})

test_that("naivedrop() out='names' returns character vector", {
  res <- naivedrop(
    dta,
    n_drp = 1,
    dir = "tail",
    crt = "alpha",
    apr = "oneshot",
    out = "names",
    alp_mtr = "raw_alpha",
    alp_args = list(check.keys = TRUE),
    mmt_mdl = NULL,
    tgt_fct = NULL,
    lam_mtr = "std.all",
    cfa_args = list(std.lv = TRUE) 
  )
  expect_type(res, "character")
})

test_that("naivedrop() out='subset' returns data.frame", {
  res <- naivedrop(
    dta,
    n_drp = 1,
    dir = "tail",
    crt = "alpha",
    apr = "oneshot",
    out = "subset",
    alp_mtr = "raw_alpha",
    alp_args = list(check.keys = TRUE),
    mmt_mdl = NULL,
    tgt_fct = NULL,
    lam_mtr = "std.all",
    cfa_args = list(std.lv = TRUE) 
  )
  expect_s3_class(res, "data.frame")
})

test_that("naivedrop() out='both' returns named list", {
  res <- naivedrop(
    dta,
    n_drp = 1,
    dir = "tail",
    crt = "alpha",
    apr = "oneshot",
    out = "both",
    alp_mtr = "raw_alpha",
    alp_args = list(check.keys = TRUE),
    mmt_mdl = NULL,
    tgt_fct = NULL,
    lam_mtr = "std.all",
    cfa_args = list(std.lv = TRUE) 
  )
  expect_named(res, c("names", "subset"))
})

test_that("naivedrop() invalid 'dir' throws error", {
  expect_error(
    naivedrop(
      dta,
      n_drp = 1,
      dir = "foobar",
      crt = "alpha",
      apr = "oneshot",
      out = "names",
      alp_mtr = "raw_alpha",
      alp_args = list(check.keys = TRUE),
      mmt_mdl = NULL,
      tgt_fct = NULL,
      lam_mtr = "std.all",
      cfa_args = list(std.lv = TRUE) 
    )
  )
})


test_that("naivedrop() invalid 'crt' throws error", {
  expect_error(
    naivedrop(
      dta,
      n_drp = 1,
      dir = "tail",
      crt = "foobar",
      apr = "oneshot",
      out = "names",
      alp_mtr = "raw_alpha",
      alp_args = list(check.keys = TRUE),
      mmt_mdl = NULL,
      tgt_fct = NULL,
      lam_mtr = "std.all",
      cfa_args = list(std.lv = TRUE) 
    )
  )
})

test_that("naivedrop() invalid 'apr' throws informative error", {
  expect_error(
    naivedrop(
      dta,
      n_drp = 1,
      dir = "tail",
      crt = "alpha",
      apr = "foobar",
      out = "names",
      alp_mtr = "raw_alpha",
      alp_args = list(check.keys = TRUE),
      mmt_mdl = NULL,
      tgt_fct = NULL,
      lam_mtr = "std.all",
      cfa_args = list(std.lv = TRUE) 
    )
  )
})

test_that("naivedrop() invalid 'out' propagates cleanly to inner functions", {
  expect_error(
    naivedrop(
      dta,
      n_drp = 1,
      dir = "tail",
      crt = "alpha",
      apr = "oneshot",
      out = "foobar",
      alp_mtr = "raw_alpha",
      alp_args = list(check.keys = TRUE),
      mmt_mdl = NULL,
      tgt_fct = NULL,
      lam_mtr = "std.all",
      cfa_args = list(std.lv = TRUE) 
    )
  )
})

test_that("naivedrop() always returns exactly n_drp items (when out='names')", {
  for (crt in c("alpha", "lambda")) {
    for (apr in c("oneshot", "greedy")) {
      res <- naivedrop(
        dta,
        n_drp = 2,
        dir = "tail",
        crt = crt,
        apr = apr,
        out = "names",
        alp_mtr = "raw_alpha",
        alp_args = list(check.keys = TRUE),
        mmt_mdl = NULL,
        tgt_fct = NULL,
        lam_mtr = "std.all",
        cfa_args = list(std.lv = TRUE) 
      )
      expect_length(res, 2)
    }
  }
})