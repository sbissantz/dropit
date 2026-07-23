# ==============================================================================
# tests/testthat/test-internals.R
#
# Internal seams and defensive paths that the public-facing tests cannot reach
# cleanly: the naivedrop() dispatcher, the "should never happen" guards, the
# CFA error branches (exercised with mocks so no model is fit), and the greedy
# progress messages.
# ==============================================================================

# ------------------------------------------------------------------------------
# Dispatch -- naivedrop() must route to the matching algorithm
# ------------------------------------------------------------------------------

test_that("naivedrop() routes each criterion x approach to its algorithm", {
  # Verified by equivalence to a direct call, not by pinned values: the point
  # is that the dispatcher picks the right function, whatever that function
  # returns.
  dta <- toy_scale(n = 200, k = 5)
  nd <- function(crt, apr) suppressWarnings(naivedrop(
    dta, anc = NULL, n_drp = 2, dir = "tail", crt = crt, apr = apr,
    alp_mtr = "raw_alpha", alp_args = list(check.keys = TRUE),
    mmt_mdl = NULL, tgt_fct = NULL, lam_mtr = "std.all",
    cfa_args = list(std.lv = TRUE)
  )$names)

  expect_equal(nd("alpha", "oneshot"),
               suppressWarnings(oneshotdrop_alpha(dta, NULL, 2, "tail", "raw_alpha", list(check.keys = TRUE)))$names)
  expect_equal(nd("alpha", "greedy"),
               suppressWarnings(greedydrop_alpha(dta, NULL, 2, "tail", "raw_alpha", list(check.keys = TRUE)))$names)
  expect_equal(nd("lambda", "oneshot"),
               oneshotdrop_lambda(dta, NULL, 2, "tail", NULL, NULL, "std.all", list(std.lv = TRUE))$names)
  expect_equal(nd("lambda", "greedy"),
               greedydrop_lambda(dta, NULL, 2, "tail", NULL, NULL, "std.all", list(std.lv = TRUE))$names)
})

test_that("naivedrop() rejects an unknown criterion or approach", {
  base <- list(dta = toy_scale(k = 4), anc = NULL, n_drp = 1, dir = "tail",
               alp_mtr = "raw_alpha", alp_args = list(check.keys = TRUE),
               mmt_mdl = NULL, tgt_fct = NULL, lam_mtr = "std.all",
               cfa_args = list(std.lv = TRUE))
  expect_error(do.call(naivedrop, c(base, list(crt = "foobar", apr = "oneshot"))))
  expect_error(do.call(naivedrop, c(base, list(crt = "alpha", apr = "foobar"))))
  expect_error(
    do.call(naivedrop, c(base, list(crt = "lambda", apr = "foobar"))),
    "Use 'oneshot' or 'greedy'"
  )
})

# ------------------------------------------------------------------------------
# Lambda path -- error branches, exercised with mocks (no CFA is fit)
# ------------------------------------------------------------------------------

test_that("oneshotdrop_lambda() rejects a custom measurement model", {
  expect_error(
    oneshotdrop_lambda(toy_scale(k = 5), anc = NULL, n_drp = 1, dir = "tail",
                       mmt_mdl = "F1 =~ i1 + i2 + i3 + i4 + i5", tgt_fct = NULL,
                       lam_mtr = "std.all", cfa_args = list(std.lv = TRUE))
  )
})

test_that("oneshotdrop_lambda() accepts an alternative lambda metric", {
  res <- oneshotdrop_lambda(toy_scale(k = 5), anc = NULL, n_drp = 1, dir = "tail",
                            mmt_mdl = NULL, tgt_fct = NULL, lam_mtr = "est",
                            cfa_args = list(std.lv = TRUE))
  expect_type(res$names, "character")
})

test_that("oneshotdrop_lambda() errors when no lambda matrix is returned", {
  dat <- data.frame(i1 = 1:6, i2 = 2:7, i3 = c(2, 3, 4, 1, 2, 3), i4 = 6:1)
  testthat::local_mocked_bindings(
    lavaan_cfa_internal     = function(...) structure(list(), class = "fake"),
    lavaan_inspect_internal = function(...) list()
  )
  expect_error(
    oneshotdrop_lambda(dat, anc = NULL, n_drp = 1, dir = "tail",
                       mmt_mdl = NULL, tgt_fct = NULL, lam_mtr = "std",
                       cfa_args = list()),
    "No 'lambda' matrix found"
  )
})

test_that("oneshotdrop_lambda() errors on a multi-factor solution without target_factor", {
  dat <- data.frame(i1 = 1:6, i2 = 2:7, i3 = c(2, 3, 4, 1, 2, 3), i4 = 6:1)
  fake_lambda <- matrix(c(0.8, 0.1, 0.7, 0.2, 0.6, 0.3, 0.5, 0.4),
                        nrow = 4, byrow = TRUE,
                        dimnames = list(colnames(dat), c("F1", "F2")))
  testthat::local_mocked_bindings(
    lavaan_cfa_internal     = function(...) structure(list(), class = "fake"),
    lavaan_inspect_internal = function(...) list(lambda = fake_lambda)
  )
  expect_error(
    oneshotdrop_lambda(dat, anc = NULL, n_drp = 1, dir = "tail",
                       mmt_mdl = NULL, tgt_fct = NULL, lam_mtr = "std",
                       cfa_args = list()),
    "multiple factors"
  )
})

# ------------------------------------------------------------------------------
# Greedy progress reporting
# ------------------------------------------------------------------------------

test_that("greedydrop_lambda() reports each round when verbose = TRUE", {
  expect_message(
    greedydrop_lambda(toy_scale(k = 5), anc = NULL, n_drp = 1, dir = "tail",
                      mmt_mdl = NULL, tgt_fct = NULL, lam_mtr = "std.all",
                      cfa_args = list(std.lv = TRUE), verbose = TRUE),
    "Model \\(1/1\\)"
  )
})

test_that("greedydrop_alpha() reports each round when verbose = TRUE", {
  # Symmetry with greedydrop_lambda: both greedy paths announce their rounds.
  expect_message(
    suppressWarnings(greedydrop_alpha(
      toy_scale(k = 5), anc = NULL, n_drp = 1, dir = "tail",
      alp_mtr = "raw_alpha", alp_args = list(check.keys = TRUE), verbose = TRUE
    )),
    "Scale \\(1/1\\)"
  )
})
