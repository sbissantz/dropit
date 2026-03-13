# ==============================================================================
# tests/testthat/test-greedydrop-lambda.R
# ==============================================================================

dta <- psych::bfi[, 1:5]

test_that("greedydrop_lambda() returns manually debugged output", {
  res <- greedydrop_lambda(
    dta = dta,
    n_drp = 3,
    dir = "tail",
    out = "names",
    mmt_mdl = NULL,
    tgt_fct = NULL,
    lam_mtr = "std.all",
    cfa_args = list(std.lv = TRUE)
  )
  expect_equal(res, c("A1", "A4", "A2"))
})

test_that("greedydrop_lambda() drops exactly n_drp unique items", {
  res <- greedydrop_lambda(
    dta,
    n_drp = 3,
    dir = "tail",
    out = "names",
    mmt_mdl = NULL, 
    tgt_fct = NULL,
    lam_mtr = "std.all",
    cfa_args = list(std.lv = TRUE)
  )
  expect_length(res, 3)
  expect_equal(length(unique(res)), 3)
})

test_that("greedydrop_lambda() subset has expected number of columns", {
  res <- greedydrop_lambda(
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

test_that("greedydrop_lambda() returns both names and subset when out = 'both'", {
  res <- greedydrop_lambda(
    dta,
    n_drp = 2,
    dir = "tail",
    out = "both",
    mmt_mdl = NULL,
    tgt_fct = NULL,
    lam_mtr = "std.all",
    cfa_args = list(std.lv = TRUE)
  )
  expect_named(res, c("names", "subset"))
  expect_false(any(res$names %in% colnames(res$subset)))
})

test_that("greedydrop_lambda() dir = 'head' works", {
  res <- greedydrop_lambda(
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

test_that("greedydrop_lambda() greedy vs oneshot differ when n_drp > 1", {
  greedy <- greedydrop_lambda(
    dta,
    n_drp = 3,
    dir = "tail",
    out = "names",
    mmt_mdl = NULL,
    tgt_fct = NULL,
    lam_mtr = "std.all",
    cfa_args = list(std.lv = TRUE)
  )

  oneshot <- oneshotdrop_lambda(
    dta,
    n_drp = 3,
    dir = "tail",
    out = "names",
    mmt_mdl = NULL,
    tgt_fct = NULL,
    lam_mtr = "std.all",
    cfa_args = list(std.lv = TRUE)
  )

  expect_false(identical(greedy, oneshot))
})

test_that("greedydrop_lambda() n_drp = 0 returns empty vector", {
  res <- greedydrop_lambda(
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

test_that("greedydrop_lambda() invalid 'dir' or 'out' throws clean error", {
  expect_error(
    greedydrop_lambda(
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
  expect_error(
    greedydrop_lambda(
      dta,
      n_drp = 1,
      dir = "tail",
      out = "grapefruit",
      mmt_mdl = NULL,
      tgt_fct = NULL,
      lam_mtr = "std.all",
      cfa_args = list(std.lv = TRUE)
    )
  )
})

test_that("greedydrop_lambda() works with lam_mtr = 'est'", {
  res <- greedydrop_lambda(
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

test_that("greedydrop_lambda() works when no model provided (mmt_mdl = NULL)", {
  res <- greedydrop_lambda(
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

test_that("greedydrop_lambda errors for invalid output type", {
  dat <- make_test_data()
  fake_fit <- structure(list(), class = "fake_lavaan_fit")
  fake_lambda <- matrix(c(0.2, 0.4, 0.8, 0.6), ncol = 1)
  rownames(fake_lambda) <- colnames(dat)
  colnames(fake_lambda) <- "F"
  testthat::local_mocked_bindings(
    lavaan_cfa_internal = function(...) fake_fit,
    lavaan_inspect_internal = function(object, what) {
      list(lambda = fake_lambda)
    }
  )
  expect_message(
    expect_error(
      greedydrop_lambda(
        dta = dat,
        n_drp = 1,
        dir = "tail",
        out = "wrong",
        mmt_mdl = NULL,
        tgt_fct = NULL,
        lam_mtr = "std",
        cfa_args = list()
      ),
      regexp = "Invalid output"
    ),
    regexp = "No measurement model was specified"
  )
})