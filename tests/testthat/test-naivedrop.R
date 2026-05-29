# ==============================================================================
# tests/testthat/test-naivedrop-alpha.R 
# ==============================================================================

dta <- psych::bfi[, 1:5]

# ----------------------------------------------------------------------- 

test_that("naivedrop() inherits output from oneshotdrop_alpha()", {
res <- naivedrop(
  dta,
  anc = NULL,
  n_drp = 4,
  dir = "tail",
  crt = "alpha",
  apr = "oneshot",
  alp_mtr = "raw_alpha",
  alp_args = list(check.keys = TRUE)
)
expect_equal(res$names, c("A4", "A5", "A2", "A3"))
})

test_that("naivedrop() inherits output from oneshotdrop_lambda()", {
  res <- naivedrop(
    dta = dta,
    anc = NULL,
    n_drp = 4,
    dir = "tail",
    crt = "lambda",
    apr = "oneshot",
    mmt_mdl = NULL,
    tgt_fct = NULL,
    lam_mtr = "std.all",
    cfa_args = list(std.lv = TRUE) 
  )
  expect_equal(res$names, c("A2", "A5", "A4", "A1"))
})

test_that("naivedrop() inherits output from greedydrop_alpha()", {
res <- naivedrop(
  dta,
  anc = NULL,
  n_drp = 4,
  dir = "tail",
  crt = "alpha",
  apr = "greedy",
  alp_mtr = "raw_alpha",
  alp_args = list(check.keys = TRUE)
)
expect_equal(res$names, c("A3", "A2", "A5", "A1"))
})

test_that("naivedrop() inherits output from greedydrop_lambda()", {
  res <- naivedrop(
    dta = dta,
    anc = NULL,
    n_drp = 3,
    dir = "tail",
    crt = "lambda",
    apr = "greedy",
    mmt_mdl = NULL,
    tgt_fct = NULL,
    lam_mtr = "std.all",
    cfa_args = list(std.lv = TRUE)
  )
expect_equal(res$names, c("A1", "A4", "A2"))
})

test_that("naivedrop() always returns list with names and subset", {
  res <- naivedrop(
    dta,
    anc = NULL,
    n_drp = 1,
    dir = "tail",
    crt = "alpha",
    apr = "oneshot",
    alp_mtr = "raw_alpha",
    alp_args = list(check.keys = TRUE),
    mmt_mdl = NULL,
    tgt_fct = NULL,
    lam_mtr = "std.all",
    cfa_args = list(std.lv = TRUE) 
  )
  expect_type(res, "list")
  expect_named(res, c("names", "subset"))
  expect_type(res$names, "character")
  expect_s3_class(res$subset, "data.frame")
})

test_that("naivedrop() invalid 'dir' throws error", {
  expect_error(
    naivedrop(
      dta,
      anc = NULL,
      n_drp = 1,
      dir = "foobar",
      crt = "alpha",
      apr = "oneshot",
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
      anc = NULL,
      n_drp = 1,
      dir = "tail",
      crt = "foobar",
      apr = "oneshot",
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
      anc = NULL,
      n_drp = 1,
      dir = "tail",
      crt = "alpha",
      apr = "foobar",
      alp_mtr = "raw_alpha",
      alp_args = list(check.keys = TRUE),
      mmt_mdl = NULL,
      tgt_fct = NULL,
      lam_mtr = "std.all",
      cfa_args = list(std.lv = TRUE) 
    )
  )
})

test_that("naivedrop() always returns exactly n_drp items", {
  for (crt in c("alpha", "lambda")) {
    for (apr in c("oneshot", "greedy")) {
      res <- naivedrop(
        dta,
        anc = NULL,
        n_drp = 2,
        dir = "tail",
        crt = crt,
        apr = apr,
        alp_mtr = "raw_alpha",
        alp_args = list(check.keys = TRUE),
        mmt_mdl = NULL,
        tgt_fct = NULL,
        lam_mtr = "std.all",
        cfa_args = list(std.lv = TRUE) 
      )
      expect_length(res$names, 2)
    }
  }
})

test_that("naivedrop() respects anchor items", {
  res <- naivedrop(
    dta = dta,
    anc = "A3",
    n_drp = 2,
    dir = "tail",
    crt = "alpha",
    apr = "greedy",
    alp_mtr = "raw_alpha",
    alp_args = list(check.keys = TRUE),
    mmt_mdl = NULL,
    tgt_fct = NULL,
    lam_mtr = "std.all",
    cfa_args = list()
  )
  expect_equal(res$names, c("A2", "A5"))
  expect_false("A3" %in% res$names)
  expect_length(res$names, 2)
})