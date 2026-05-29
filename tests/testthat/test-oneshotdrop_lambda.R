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

test_that("oneshotdrop_lambda() errors when lambda matrix missing", {
  # Use an internal dummy dataset
  dat <- data.frame(i1=1:6, i2=2:7, i3=c(2,3,4,1,2,3), i4=6:1)
  fake_fit <- structure(list(), class = "fake")
  
  testthat::local_mocked_bindings(
    lavaan_cfa_internal = function(...) fake_fit,
    lavaan_inspect_internal = function(...) list()
  )
  
  expect_error(
    oneshotdrop_lambda(
      dta = dat, anc = NULL, n_drp = 1, dir = "tail",
      mmt_mdl = NULL, tgt_fct = NULL, lam_mtr = "std", cfa_args = list()
    ),
    "No 'lambda' matrix found"
  )
})

test_that("oneshotdrop_lambda() errors when multiple factors present", {
  dat <- data.frame(i1=1:6, i2=2:7, i3=c(2,3,4,1,2,3), i4=6:1)
  fake_fit <- structure(list(), class = "fake")
  
  # Mock a lambda matrix with two factors
  fake_lambda <- matrix(
    c(0.8, 0.1, 0.7, 0.2, 0.6, 0.3, 0.5, 0.4),
    nrow = 4, byrow = TRUE
  )
  rownames(fake_lambda) <- colnames(dat)
  colnames(fake_lambda) <- c("F1", "F2")
  
  testthat::local_mocked_bindings(
    lavaan_cfa_internal = function(...) fake_fit,
    lavaan_inspect_internal = function(...) list(lambda = fake_lambda)
  )
  
  expect_error(
    oneshotdrop_lambda(
      dta = dat, anc = NULL, n_drp = 1, dir = "tail",
      mmt_mdl = NULL, tgt_fct = NULL, lam_mtr = "std", cfa_args = list()
    ),
    "multiple factors"
  )
})