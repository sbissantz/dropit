# ==============================================================================
# tests/testthat/test-properties.R
#
# Invariants that must hold for ANY input, checked across several seeds and
# both criteria. No fixed expected values appear in this file -- each test
# states a relationship that has to be true, so it keeps its diagnostic power
# on data it has never seen.
#
# These are the tests that catch silent wrongness: the package returns a
# plausible-looking character vector no matter what goes wrong, so assertions
# about *structure* are what separate a right answer from a wrong-shaped one.
# ==============================================================================

seeds <- c(1L, 7L, 42L)

# ------------------------------------------------------------------------------
# Ranking must depend on the data, and nothing but the data
# ------------------------------------------------------------------------------

test_that("result does not depend on column order", {
  # The single most valuable invariant here. `order()` breaks ties by column
  # position, so any tie-dependence shows up as a different answer after a
  # harmless reshuffle -- which for a preregistered instrument would mean the
  # test form silently depends on how the data frame was assembled.
  for (s in seeds) {
    dta <- toy_scale(n = 400, k = 5, seed = s)
    shuffled <- dta[, rev(names(dta)), drop = FALSE]
    for (crt in names(both_criteria)) {
      f <- both_criteria[[crt]]
      expect_setequal(f(dta, 2), f(shuffled, 2))
      expect_setequal(f(dta, 3, dir = "head"), f(shuffled, 3, dir = "head"))
    }
  }
})

test_that("result does not depend on item names", {
  # Renaming carries no psychometric information, so the same *positions* must
  # be selected. Guards against any accidental name-based ordering.
  for (s in seeds) {
    dta <- toy_scale(n = 400, k = 5, seed = s)
    renamed <- dta
    names(renamed) <- paste0("zz", rev(seq_len(ncol(dta))))
    for (crt in names(both_criteria)) {
      f <- both_criteria[[crt]]
      expect_equal(
        match(f(dta, 2), names(dta)),
        match(f(renamed, 2), names(renamed))
      )
    }
  }
})

test_that("reverse-keying an item does not change which items are dropped", {
  # Flipping an item's direction preserves its information; only its sign
  # changes. Its rank must not move.
  for (s in seeds) {
    dta <- toy_scale(n = 400, k = 5, seed = s)
    flipped <- dta
    flipped[["i2"]] <- 6L - flipped[["i2"]]
    for (crt in names(both_criteria)) {
      f <- both_criteria[[crt]]
      expect_setequal(f(dta, 2), f(flipped, 2))
      # psych's trailing "-" marker must never leak into returned names
      expect_false(any(grepl("-$", f(flipped, 2))))
    }
  }
})

# ------------------------------------------------------------------------------
# Ranking must track item strength
# ------------------------------------------------------------------------------

test_that("a pure-noise item is always dropped first", {
  # An item unrelated to the common factor is unambiguously the worst. If a
  # criterion does not drop it first, that criterion's polarity is wrong.
  for (s in seeds) {
    dta <- toy_with_noise_item(n = 600, k = 4, seed = s)
    for (crt in names(both_criteria)) {
      expect_equal(both_criteria[[crt]](dta, 1), "junk", info = crt)
    }
  }
})

test_that("a pure-noise item is never dropped by direction = 'head'", {
  # The mirror of the above, and the direct check for the inversion bug:
  # "head" claims to drop the strongest, so it must never pick the noise item.
  for (s in seeds) {
    dta <- toy_with_noise_item(n = 600, k = 4, seed = s)
    for (crt in names(both_criteria)) {
      expect_false("junk" %in% both_criteria[[crt]](dta, 2, dir = "head"), info = crt)
    }
  }
})

test_that("both criteria recover the generator's strength ordering", {
  for (s in seeds) {
    dta <- toy_scale(n = 600, k = 5, seed = s)
    truth <- toy_truth(5)  # strongest to weakest, by construction
    for (crt in names(both_criteria)) {
      f <- both_criteria[[crt]]
      expect_equal(f(dta, 1), truth[5], info = crt)                 # weakest
      expect_equal(f(dta, 1, dir = "head"), truth[1], info = crt)   # strongest
      expect_setequal(f(dta, 2), truth[4:5])
    }
  }
})

# ------------------------------------------------------------------------------
# Structural guarantees
# ------------------------------------------------------------------------------

test_that("head and tail select opposite ends and never overlap", {
  for (s in seeds) {
    dta <- toy_scale(n = 400, k = 6, seed = s)
    for (crt in names(both_criteria)) {
      f <- both_criteria[[crt]]
      expect_length(intersect(f(dta, 2, dir = "head"), f(dta, 2, dir = "tail")), 0)
    }
  }
})

test_that("one-shot drops are nested as n_drop grows", {
  # A single ranking is computed once, so dropping n items must be a subset of
  # dropping n+1. If this ever fails, the ranking is not stable across calls.
  for (s in seeds) {
    dta <- toy_scale(n = 400, k = 6, seed = s)
    for (crt in names(both_criteria)) {
      f <- both_criteria[[crt]]
      for (n in 1:3) {
        expect_true(all(f(dta, n) %in% f(dta, n + 1)), info = sprintf("%s n=%d", crt, n))
      }
    }
  }
})

test_that("anchors are never dropped and exactly n_drop items are returned", {
  for (s in seeds) {
    dta <- toy_scale(n = 400, k = 6, seed = s)
    anc <- c("i5", "i6")  # the two weakest, i.e. the ones most at risk
    for (crt in names(both_criteria)) {
      f <- both_criteria[[crt]]
      for (n in 1:3) {
        res <- f(dta, n, anc = anc)
        expect_length(res, n)
        expect_length(intersect(res, anc), 0)
        expect_true(all(res %in% names(dta)))
        expect_false(anyDuplicated(res) > 0)
      }
    }
  }
})

test_that("dropped items are removed from the subset and nothing else is", {
  for (s in seeds) {
    dta <- toy_scale(n = 400, k = 5, seed = s)
    res <- suppressWarnings(oneshotdrop_alpha(
      dta, anc = NULL, n_drp = 2, dir = "tail",
      alp_mtr = "raw_alpha", alp_args = list(check.keys = TRUE)
    ))
    expect_setequal(colnames(res[["subset"]]), setdiff(names(dta), res[["names"]]))
    expect_equal(nrow(res[["subset"]]), nrow(dta))
  }
})

# ------------------------------------------------------------------------------
# Greedy-specific structure
# ------------------------------------------------------------------------------

greedy_criteria <- list(
  alpha  = function(dta, n) suppressWarnings(greedydrop_alpha(
    dta, anc = NULL, n_drp = n, dir = "tail",
    alp_mtr = "raw_alpha", alp_args = list(check.keys = TRUE)))[["names"]],
  lambda = function(dta, n) greedydrop_lambda(
    dta, anc = NULL, n_drp = n, dir = "tail", mmt_mdl = NULL, tgt_fct = NULL,
    lam_mtr = "std.all", cfa_args = list(std.lv = TRUE))[["names"]]
)

test_that("greedy dropping returns exactly n_drop distinct items", {
  for (s in seeds) {
    dta <- toy_scale(n = 400, k = 6, seed = s)
    for (crt in names(greedy_criteria)) {
      res <- greedy_criteria[[crt]](dta, 3)
      expect_length(res, 3)
      expect_length(unique(res), 3)
      expect_true(all(res %in% names(dta)), info = crt)
    }
  }
})

test_that("greedy and one-shot may disagree once more than one item is dropped", {
  # Not a fixed result -- the point is only that refitting each round can lead
  # somewhere a single ranking would not, so the two approaches are genuinely
  # different procedures rather than aliases.
  disagreed <- FALSE
  for (s in seeds) {
    dta <- toy_scale(n = 400, k = 6, seed = s)
    for (crt in names(greedy_criteria)) {
      g <- greedy_criteria[[crt]](dta, 3)
      o <- both_criteria[[crt]](dta, 3)
      if (!identical(g, o)) disagreed <- TRUE
    }
  }
  expect_true(disagreed)
})
