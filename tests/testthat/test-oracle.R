# ==============================================================================
# tests/testthat/test-oracle.R
#
# Ground-truth ("oracle") tests for the ranking criteria.
#
# Every other test file pins values that were read off the functions' own
# output. Those cannot detect a wrong answer -- they freeze whatever the code
# already did. The tests below instead use a toy dataset whose correct answer
# is known *by construction*, before any dropit code runs:
#
#   item k = common factor + noise, with noise variance strictly increasing.
#
# So i1 is the strongest indicator and i5 the weakest, necessarily. Both
# criteria must recover that order, and must agree with each other. This is
# what catches a polarity inversion; a golden-value test never can.
# ==============================================================================

# --- toy data with a known strength ordering: i1 (best) ... i5 (worst) ---------
toy_data <- function(n = 800, seed = 42) {
  set.seed(seed)
  fct <- stats::rnorm(n)
  sds <- c(0.30, 0.60, 0.90, 1.20, 1.60)  # strictly increasing noise
  dta <- as.data.frame(lapply(sds, function(s) {
    z <- fct + stats::rnorm(n, 0, s)
    # discretise to a 5-point Likert scale, the package's actual domain
    as.integer(cut(z, breaks = c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf), labels = FALSE))
  }))
  names(dta) <- paste0("i", seq_along(sds))
  dta
}

dta <- toy_data()

# strongest to weakest, by construction
truth <- paste0("i", 1:5)

drop_alpha <- function(dta, n_drp, dir, anc = NULL, mtr = "raw_alpha") {
  suppressWarnings(oneshotdrop_alpha(
    dta, anc = anc, n_drp = n_drp, dir = dir,
    alp_mtr = mtr, alp_args = list(check.keys = TRUE)
  ))
}

drop_lambda <- function(dta, n_drp, dir, anc = NULL) {
  oneshotdrop_lambda(
    dta, anc = anc, n_drp = n_drp, dir = dir,
    mmt_mdl = NULL, tgt_fct = NULL, lam_mtr = "std.all",
    cfa_args = list(std.lv = TRUE)
  )
}

# ------------------------------------------------------------------------------
# The generator's promise actually holds. If this fails, the toy data is broken
# and every other test in this file is meaningless -- check here first.
# ------------------------------------------------------------------------------

test_that("toy data has the intended strictly monotone strength ordering", {
  alp <- suppressWarnings(psych::alpha(dta, check.keys = TRUE))
  # alpha-if-dropped rises as items get worse
  expect_identical(order(alp[["alpha.drop"]][, "raw_alpha"]), 1:5)
  # item-rest correlation falls as items get worse
  expect_identical(order(alp[["item.stats"]][["r.drop"]], decreasing = TRUE), 1:5)
})

test_that("alpha.drop and r.drop are exact inverse rankings", {
  # A property of psych::alpha itself, and the assumption the alpha path rests
  # on: `alpha.drop` is inversely related to item strength. If psych ever
  # changes this, the polarity conversion in oneshotdrop_alpha() is wrong.
  alp <- suppressWarnings(psych::alpha(dta, check.keys = TRUE))
  expect_identical(
    order(alp[["alpha.drop"]][, "raw_alpha"]),
    order(alp[["item.stats"]][["r.drop"]], decreasing = TRUE)
  )
})

# ------------------------------------------------------------------------------
# dir = "tail" drops the weakest items
# ------------------------------------------------------------------------------

test_that("alpha with dir='tail' drops the weakest items", {
  expect_equal(drop_alpha(dta, 1, "tail")[["names"]], "i5")
  expect_equal(drop_alpha(dta, 2, "tail")[["names"]], c("i4", "i5"))
  expect_equal(drop_alpha(dta, 4, "tail")[["names"]], c("i2", "i3", "i4", "i5"))
})

test_that("lambda with dir='tail' drops the weakest items", {
  expect_equal(drop_lambda(dta, 1, "tail")[["names"]], "i5")
  expect_equal(drop_lambda(dta, 2, "tail")[["names"]], c("i4", "i5"))
  expect_equal(drop_lambda(dta, 4, "tail")[["names"]], c("i2", "i3", "i4", "i5"))
})

# ------------------------------------------------------------------------------
# dir = "head" drops the strongest items.
#
# This is the assertion that would have caught the inversion immediately: under
# the bug, alpha's "head" returned the weakest item while claiming the
# strongest. It was previously untested for the alpha path.
# ------------------------------------------------------------------------------

test_that("alpha with dir='head' drops the strongest items", {
  expect_equal(drop_alpha(dta, 1, "head")[["names"]], "i1")
  expect_equal(drop_alpha(dta, 2, "head")[["names"]], c("i1", "i2"))
})

test_that("lambda with dir='head' drops the strongest items", {
  expect_equal(drop_lambda(dta, 1, "head")[["names"]], "i1")
  expect_equal(drop_lambda(dta, 2, "head")[["names"]], c("i1", "i2"))
})

# ------------------------------------------------------------------------------
# Cross-criterion agreement
# ------------------------------------------------------------------------------

test_that("alpha and lambda agree on clean unidimensional data", {
  # On data with a single strong factor and no ties, the two criteria are
  # ranking the same underlying quantity and must produce the same answer.
  # Systematic disagreement here means one of them has the wrong polarity.
  for (n_drp in 1:4) {
    expect_equal(
      drop_alpha(dta, n_drp, "tail")[["names"]],
      drop_lambda(dta, n_drp, "tail")[["names"]],
      info = sprintf("n_drp = %d", n_drp)
    )
  }
})

# ------------------------------------------------------------------------------
# Invariance and interaction with other arguments
# ------------------------------------------------------------------------------

test_that("reverse-keyed items are ranked on strength, not direction", {
  # i2 is flipped: same information, opposite direction. Its rank must not
  # move, and psych's trailing "-" marker must be stripped from the result.
  rev_dta <- dta
  rev_dta[["i2"]] <- 6L - rev_dta[["i2"]]

  res <- drop_alpha(rev_dta, 4, "tail")[["names"]]
  expect_equal(res, c("i2", "i3", "i4", "i5"))
  expect_false(any(grepl("-$", res)))
})

test_that("anchored items are protected and the rest still rank correctly", {
  # Anchor the two weakest; the next-weakest droppable item is i3.
  expect_equal(drop_alpha(dta, 1, "tail", anc = c("i4", "i5"))[["names"]], "i3")
  expect_equal(drop_lambda(dta, 1, "tail", anc = c("i4", "i5"))[["names"]], "i3")

  # Anchor the strongest; "head" must then take the second-strongest.
  expect_equal(drop_alpha(dta, 1, "head", anc = "i1")[["names"]], "i2")
  expect_equal(drop_lambda(dta, 1, "head", anc = "i1")[["names"]], "i2")
})

test_that("std.alpha metric gives the same ordering as raw_alpha", {
  expect_equal(drop_alpha(dta, 2, "tail", mtr = "std.alpha")[["names"]], c("i4", "i5"))
})

# ------------------------------------------------------------------------------
# Greedy variants inherit the correct polarity
# ------------------------------------------------------------------------------

test_that("greedy alpha removes items weakest-first", {
  res <- suppressWarnings(greedydrop_alpha(
    dta, anc = NULL, n_drp = 2, dir = "tail",
    alp_mtr = "raw_alpha", alp_args = list(check.keys = TRUE)
  ))
  expect_equal(res[["names"]], c("i5", "i4"))
})

test_that("greedy lambda removes items weakest-first", {
  res <- greedydrop_lambda(
    dta, anc = NULL, n_drp = 2, dir = "tail",
    mmt_mdl = NULL, tgt_fct = NULL, lam_mtr = "std.all",
    cfa_args = list(std.lv = TRUE)
  )
  expect_equal(res[["names"]], c("i5", "i4"))
})
