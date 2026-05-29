# ==============================================================================
# tests/testthat/test-greedydrop-alpha.R 
# ==============================================================================

dta <- psych::bfi[, 1:5]

test_that("greedydrop_alpha() returns manually debugged result", {
  res <- greedydrop_alpha(
    dta,
    anc = NULL,
    n_drp = 4,
    dir = "tail",
    alp_mtr = "raw_alpha",
    alp_args = list(check.keys = TRUE)
  )
  expect_type(res, "list")
  expect_named(res, c("names", "subset"))
  expect_equal(res$names, c("A3", "A2", "A5", "A1"))
  expect_false(any(res$names %in% colnames(res$subset)))
})

test_that("greedydrop_alpha() drops exactly n_drp unique items", {
  res <- greedydrop_alpha(
    dta,
    anc = NULL,
    n_drp = 3,
    dir = "tail",
    alp_mtr = "raw_alpha",
    alp_args = list(check.keys = TRUE)
  )
  expect_length(res$names, 3)
  expect_equal(length(unique(res$names)), 3)
})

test_that("greedydrop_alpha() greedy vs oneshot differ when n_drp > 1", {
  greedy <- greedydrop_alpha(
    dta,
    anc = NULL,
    n_drp = 3,
    dir = "tail",
    alp_mtr = "raw_alpha",
    alp_args = list(check.keys = TRUE)
  )
  oneshot <- oneshotdrop_alpha(
    dta,
    anc = NULL,
    n_drp = 3,
    dir = "tail",
    alp_mtr = "raw_alpha",
    alp_args = list(check.keys = TRUE)
  )
  expect_false(identical(greedy$names, oneshot$names))
})

test_that("greedydrop_alpha() throws error if n_drp = ncol(dta)", {
  expect_error(
    res <- greedydrop_alpha(
      dta,
      anc = NULL,
      n_drp = ncol(dta),
      dir = "tail",
      alp_mtr = "raw_alpha",
      alp_args = list(check.keys = TRUE)
    )
  )
})

test_that("greedydrop_alpha() invalid 'dir' throws clean error", {
  expect_error(greedydrop_alpha(
    dta,
    anc = NULL,
    n_drp = 1,
    dir = "foobar",
    alp_mtr = "raw_alpha",
    alp_args = list(check.keys = TRUE)
  ))
})

test_that("greedydrop_alpha() respects anchor items", {
  res <- greedydrop_alpha(
    dta = dta,
    anc = "A3",
    n_drp = 2,
    dir = "tail",
    alp_mtr = "raw_alpha",
    alp_args = list(check.keys = TRUE)
  )
  expect_false("A3" %in% res$names)
  expect_length(res$names, 2)
})