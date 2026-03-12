# ==============================================================================
# tests/testthat/test-greedydrop-alpha.R 
# ==============================================================================

library(psych)
library(testthat)

dta <- psych::bfi[, 1:5]

test_that("greedydrop_alpha() returns manually debugged output", {
res <- greedydrop_alpha(
  dta,
  n_drp = 4,
  dir = "tail",
  out = "names",
  alp_mtr = "raw_alpha",
  alp_args = list(check.keys = TRUE)
)
expect_equal(res, c("A3", "A2", "A5", "A1"))
})

test_that("greedydrop_alpha() drops exactly n_drp unique items", {
  res <- greedydrop_alpha(
    dta,
    n_drp = 3,
    dir = "tail",
    out = "names",
    alp_mtr = "raw_alpha",
    alp_args = list(check.keys = TRUE)
  )
  expect_length(res, 3)
  expect_equal(length(unique(res)), 3)
})

test_that("greedydrop_alpha() subset has expected number of columns", {
  res <- greedydrop_alpha(
    dta,
    n_drp = 2,
    dir = "tail",
    out = "subset",
    alp_mtr = "raw_alpha",
    alp_args = list(check.keys = TRUE)
  )
  expect_equal(ncol(res), ncol(dta) - 2)
})

test_that("greedydrop_alpha() returns both names and subset when out = 'both'", {
  res <- greedydrop_alpha(
    dta,
    n_drp = 2,
    dir = "tail",
    out = "both",
    alp_mtr = "raw_alpha",
    alp_args = list(check.keys = TRUE)
  )
  expect_named(res, c("names", "subset"))
  expect_false(any(res$names %in% colnames(res$subset)))
})

test_that("greedydrop_alpha() dir = 'head' works", {
  res <- greedydrop_alpha(
    dta,
    n_drp = 2,
    dir = "head",
    out = "names",
    alp_mtr = "raw_alpha",
    alp_args = list(check.keys = TRUE)
  )
  expect_length(res, 2)
})

test_that("greedydrop_alpha() greedy vs oneshot differ when n_drp > 1", {
  greedy <- greedydrop_alpha(
    dta,
    n_drp = 3,
    dir = "tail",
    out = "names",
    alp_mtr = "raw_alpha",
    alp_args = list(check.keys = TRUE)
  )
  oneshot <- oneshotdrop_alpha(
    dta,
    n_drp = 3,
    dir = "tail",
    out = "names",
    alp_mtr = "raw_alpha",
    alp_args = list(check.keys = TRUE)
  )
  expect_false(identical(greedy, oneshot))
})

test_that("greedydrop_alpha() n_drp = 0 returns empty vector", {
  res <- greedydrop_alpha(
    dta,
    n_drp = 0,
    dir = "tail",
    out = "names",
    alp_mtr = "raw_alpha",
    alp_args = list(check.keys = TRUE)
  )
  expect_equal(res, character(0))
})

test_that("greedydrop_alpha() throws error if n_drp = ncol(dta) - alpha requires more than one item for correlations", {
  expect_error(
    res <- greedydrop_alpha(
      dta,
      n_drp = ncol(dta),
      dir = "tail",
      out = "names",
      alp_mtr = "raw_alpha",
      alp_args = list(check.keys = TRUE)
    )
  )
})

test_that("greedydrop_alpha() invalid 'dir' or 'out' throws clean error", {
  expect_error(greedydrop_alpha(
    dta,
    n_drp = 1,
    dir = "foobar",
    out = "names",
    alp_mtr = "raw_alpha",
    alp_args = list(check.keys = TRUE)
  ))
  expect_error(greedydrop_alpha(
    dta,
    1,
    "tail",
    "grapefruit",
    "raw_alpha",
    alp_args
  ))
})
