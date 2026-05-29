# ==============================================================================
# tests/testthat/test-oneshotdrop-lambda.R 
# ==============================================================================

dta <- psych::bfi[, 1:5]  

test_that("oneshotdrop_lambda() returns manually debugged result", {
  res <- oneshotdrop_lambda(
    dta,
    anc = NULL,
    n_drp = 4,
    dir = "tail",
    mmt_mdl = NULL,
    tgt_fct = NULL,
    lam_mtr = "std.all",
    cfa_args = list(std.lv = TRUE)
  )
  expect_type(res, "list")
  expect_named(res, c("names", "subset"))
  expect_equal(res$names, c("A2", "A5", "A4", "A1"))
})

test_that("oneshotdrop_lambda() errors with custom measurement model", {
expect_error(
  oneshotdrop_lambda(
    dta,
    anc = NULL,
    n_drp = 1,
    dir = "tail",
    mmt_mdl = "F1 =~ A1 + A2 + A3 + A4 + A5",
    tgt_fct = NULL,
    lam_mtr = "std.all",
    cfa_args = list(std.lv = TRUE)
  )
)
})

test_that("oneshotdrop_lambda() dropped name is not a column name in subset", {
  res <- oneshotdrop_lambda(
    dta,
    anc = NULL,
    n_drp = 1,
    dir = "tail",
    mmt_mdl = NULL,
    tgt_fct = NULL,
    lam_mtr = "std.all",
    cfa_args = list(std.lv = TRUE)
  )
  expect_false(res$names %in% colnames(res$subset))
})

test_that("oneshotdrop_lambda() removes correct number of items", {
  res <- oneshotdrop_lambda(
    dta,
    anc = NULL,
    n_drp = 2,
    dir = "tail",
    mmt_mdl = NULL,
    tgt_fct = NULL,
    lam_mtr = "std.all",
    cfa_args = list(std.lv = TRUE)
  )
  expect_length(res$names, 2)
  expect_equal(ncol(res$subset), ncol(dta) - 2)
})

test_that("oneshotdrop_lambda() works with lam_mtr = 'est'", {
  res <- oneshotdrop_lambda(
    dta,
    anc = NULL,
    n_drp = 1,
    dir = "tail",
    mmt_mdl = NULL,
    tgt_fct = NULL,
    lam_mtr = "est",
    cfa_args = list(std.lv = TRUE)
  )
  expect_type(res$names, "character")
})

test_that("oneshotdrop_lambda() throws error if lam_mtr invalid", {
  expect_error(
    oneshotdrop_lambda(
      dta,
      anc = NULL,
      n_drp = 1,
      dir = "tail",
      mmt_mdl = NULL,
      tgt_fct = NULL,
      lam_mtr = "foobar",
      cfa_args = list(std.lv = TRUE)
    )
  )
})

test_that("oneshotdrop_lambda() respects anchor items", {
  res <- oneshotdrop_lambda(
    dta = dta,
    anc = "A1",
    n_drp = 1,
    dir = "tail",
    mmt_mdl = NULL,
    tgt_fct = NULL,
    lam_mtr = "std.all",
    cfa_args = list(std.lv = TRUE)
  )
  expect_false("A1" %in% res$names)
  expect_equal(res$names, "A4")
})