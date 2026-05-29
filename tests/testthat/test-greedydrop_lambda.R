# ==============================================================================
# tests/testthat/test-greedydrop-lambda.R
# ==============================================================================

dta <- psych::bfi[, 1:5]

test_that("greedydrop_lambda() returns manually debugged result", {
  res <- greedydrop_lambda(
    dta = dta,
    anc = NULL,
    n_drp = 3,
    dir = "tail",
    mmt_mdl = NULL,
    tgt_fct = NULL,
    lam_mtr = "std.all",
    cfa_args = list(std.lv = TRUE)
  )
  expect_type(res, "list")
  expect_named(res, c("names", "subset"))
  expect_equal(res$names, c("A1", "A4", "A2"))
  expect_false(any(res$names %in% colnames(res$subset)))
})

test_that("greedydrop_lambda() drops exactly n_drp unique items", {
  res <- greedydrop_lambda(
    dta,
    anc = NULL,
    n_drp = 3,
    dir = "tail",
    mmt_mdl = NULL, 
    tgt_fct = NULL,
    lam_mtr = "std.all",
    cfa_args = list(std.lv = TRUE)
  )
  expect_length(res$names, 3)
  expect_equal(length(unique(res$names)), 3)
})

test_that("greedydrop_lambda() greedy vs oneshot differ when n_drp > 1", {
  greedy <- greedydrop_lambda(
    dta,
    anc = NULL,
    n_drp = 3,
    dir = "tail",
    mmt_mdl = NULL,
    tgt_fct = NULL,
    lam_mtr = "std.all",
    cfa_args = list(std.lv = TRUE)
  )
  oneshot <- oneshotdrop_lambda(
    dta,
    anc = NULL,
    n_drp = 3,
    dir = "tail",
    mmt_mdl = NULL,
    tgt_fct = NULL,
    lam_mtr = "std.all",
    cfa_args = list(std.lv = TRUE)
  )
  expect_false(identical(greedy$names, oneshot$names))
})

test_that("greedydrop_lambda() invalid 'dir' throws clean error", {
  expect_error(
    greedydrop_lambda(
      dta,
      anc = NULL,
      n_drp = 1,
      dir = "foobar",
      mmt_mdl = NULL,
      tgt_fct = NULL,
      lam_mtr = "std.all",
      cfa_args = list(std.lv = TRUE)
    )
  )
})

test_that("greedydrop_lambda() respects anchor items", {
  res <- greedydrop_lambda(
    dta = dta,
    anc = "A1",
    n_drp = 2,
    dir = "tail",
    mmt_mdl = NULL,
    tgt_fct = NULL,
    lam_mtr = "std.all",
    cfa_args = list(std.lv = TRUE)
  )
  expect_equal(res$names, c("A4", "A5"))
  expect_false("A1" %in% res$names)
  expect_length(res$names, 2)
})