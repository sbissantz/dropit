# ==============================================================================
# tests/testthat/test-safeguards.R
#
# Tests for the machinery that turns silent wrong answers into loud ones:
# the criterion registry (polarity), boundary-tie detection, the small-scale
# guard, the missing-data contract, and the `checks` toggle that gates the
# advisory guards for simulation loops.
#
# Each of these exists because of a defect that shipped. They are only worth
# having if they actually fire, so every guard is tested in both directions:
# it warns when it should, and stays quiet when it should not.
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. Criterion registry -- polarity is declared, not assumed
# ------------------------------------------------------------------------------

test_that("every registered criterion declares a scoring rule", {
  # Guards against adding a criterion and forgetting the polarity question.
  for (crt in names(criterion_registry)) {
    expect_true(is.function(criterion_registry[[crt]][["score_rule"]]), info = crt)
    expect_true(nzchar(criterion_registry[[crt]][["lbl"]]), info = crt)
  }
})

test_that("alpha's mapping inverts, because alpha.drop opposes item strength", {
  # A high alpha.drop means removing the item helps, i.e. the item is weak,
  # so the item score must fall as alpha.drop rises.
  expect_equal(score_items("alpha", c(0.6, 0.7, 0.8)), c(-0.6, -0.7, -0.8))
  expect_true(all(diff(score_items("alpha", c(0.6, 0.7, 0.8))) < 0))
})

test_that("lambda's mapping takes magnitude, because sign is keying only", {
  expect_equal(score_items("lambda", c(-0.9, 0.2, -0.4)), c(0.9, 0.2, 0.4))
})

test_that("an unregistered criterion is refused and the registered ones listed", {
  expect_error(score_items("kappa", 1), "Unregistered criterion")
  expect_error(score_items("kappa", 1), "higher means 'keep'")
  # the error lists the registered criteria by key and label (via `lbl`)
  expect_error(score_items("kappa", 1), "alpha")
  expect_error(score_items("kappa", 1), "Cronbach")
})

# ------------------------------------------------------------------------------
# 2. Boundary ties -- an invisible coin flip must be announced
# ------------------------------------------------------------------------------

test_that("a tie at the drop boundary warns", {
  # i2_copy duplicates i2 exactly, so the two are formally indistinguishable.
  # They rank 1st and 2nd, so the cut falls between them only at n_drp = 4.
  dta <- toy_with_exact_tie()
  expect_true(warned_with(
    drop_alpha_raw(dta, n_drp = 4, dir = "tail"), "Tie at the drop boundary"
  ))
})

test_that("a tie away from the drop boundary does not warn", {
  # The guard must be quiet when the tied pair is not being split, otherwise
  # it becomes noise and gets ignored -- which is how warnings stop working.
  dta <- toy_with_exact_tie()
  for (n in 1:3) {
    expect_false(warned_with(
      drop_alpha_raw(dta, n_drp = n, dir = "tail"), "Tie at the drop boundary"
    ), info = sprintf("n_drp = %d", n))
  }
})

test_that("no tie warning on clean data at any boundary", {
  dta <- toy_scale(n = 400, k = 5)
  for (n in 1:4) {
    expect_false(warned_with(
      drop_alpha_raw(dta, n_drp = n, dir = "tail"), "Tie at the drop boundary"
    ), info = sprintf("n_drp = %d", n))
  }
})

test_that("a tied boundary really does make the result input-order dependent", {
  # Demonstrates the thing the warning is warning about: with the tie split,
  # the input order alone decides which of the tied items is cut. This is why
  # the warning has to exist rather than being a nicety.
  #
  # The tie is built by hand rather than derived from psych::alpha() on a
  # duplicated column: that route needs the two alpha-if-dropped values to come
  # out bit-identical, which holds only if the two submatrices sum in the same
  # floating-point order. That is BLAS-dependent, so it holds on some platforms
  # and not others.
  scr <- c(i1 = 3, i2 = 1, i3 = 2, i2_copy = 1)
  a <- suppressWarnings(trim_items(rank_items(scr, anc = NULL), 1, "tail"))
  b <- suppressWarnings(trim_items(rank_items(rev(scr), anc = NULL), 1, "tail"))
  expect_false(setequal(a, b))
})

# ------------------------------------------------------------------------------
# 3. Small-scale guard
# ------------------------------------------------------------------------------

test_that("ranking fewer than three items warns", {
  expect_warning(warn_small(2, "alpha"), "not meaningful below 3 items")
  expect_warning(warn_small(1, "lambda"), "not meaningful below 3 items")
})

test_that("ranking three or more items does not warn", {
  expect_no_warning(warn_small(3, "alpha"))
  expect_no_warning(warn_small(60, "lambda"))
})

test_that("the guard fires through dropit() on a 2-item scale", {
  # The concrete case that produced a meaningless golden value: with 2 items
  # every item-rest correlation is the same number, yet raw_alpha still
  # reports a difference driven by unequal item variances. The check now lives
  # in preflight_checks(), so it is exercised through the public entry point.
  dta <- toy_scale(k = 5)[, c("i1", "i5")]
  res <- dropit(dta, n_drop = 1, criterion = "alpha",
                alpha_args = list(check.keys = TRUE), verbose = FALSE)
  expect_true(any(grepl("not meaningful below 3 items", res[["log"]][["warnings"]])))
})

test_that("preflight predicts the smallest greedy scale (m - n_drop + 1)", {
  # Greedy's last round ranks m - n_drop + 1 items, so preflight must warn from
  # the inputs alone -- before any fit -- when that prediction drops below 3.
  four <- toy_scale(k = 4)
  expect_warning(preflight_checks(four, NULL, n_drp = 3, apr = "greedy", crt = "alpha"),
                 "not meaningful below 3 items")   # 4 - 3 + 1 = 2
  expect_no_warning(preflight_checks(four, NULL, n_drp = 1, apr = "greedy", crt = "alpha"))
  # oneshot ranks the full scale, so only its own size matters
  expect_no_warning(preflight_checks(four, NULL, n_drp = 3, apr = "oneshot", crt = "alpha"))
})

test_that("preflight checks each partition arm's smallest scale", {
  dta <- toy_scale(k = 5)
  arm_of_two <- c("A", "A", "B", "B", "B")   # arm A holds only 2 items
  expect_warning(
    preflight_checks(dta, arm_of_two, n_drp = 1, apr = "oneshot", crt = "alpha"),
    "not meaningful below 3 items"
  )
})

# ------------------------------------------------------------------------------
# 4. Missing data -- handled by the engine, never silently by dropit()
#
# dropit() does not delete respondents. Missing values are passed straight to
# the ranking engine through its native arguments, so the treatment stays
# visible in the call. These tests pin that contract: dropit() must not strip
# rows behind the user's back, and it must warn about the one combination
# (greedy + missing data) where the sample can drift unnoticed.
# ------------------------------------------------------------------------------

test_that("dropit() does not delete respondents behind the engine's back", {
  # The bug this replaces: an up-front listwise deletion would silently defeat
  # cfa_args = list(missing = "fiml") by removing incomplete rows before lavaan
  # ever saw them. Capture the data actually handed to cfa and confirm every
  # row survives.
  dta <- toy_scale(n = 300, k = 5)
  dta[1:40, "i3"] <- NA

  seen_nrow <- NA_integer_
  testthat::local_mocked_bindings(
    lavaan_cfa_internal = function(...) {
      seen_nrow <<- nrow(list(...)[["data"]])
      structure(list(), class = "fake")
    },
    lavaan_inspect_internal = function(...) {
      lam <- matrix(c(0.8, 0.7, 0.6, 0.5, 0.4), ncol = 1,
                    dimnames = list(paste0("i", 1:5), "F"))
      list(lambda = lam)
    }
  )
  dropit(dta, n_drop = 1, criterion = "lambda", verbose = FALSE)
  expect_equal(seen_nrow, 300)  # all rows reach the engine, incomplete included
})

test_that("engine-native missing args flow through untouched", {
  # `use` for alpha and `missing` for lambda are the documented seams; confirm
  # dropit() forwards them rather than intercepting.
  dta <- toy_scale(n = 200, k = 5)
  dta[1:20, "i2"] <- NA
  expect_silent(suppressWarnings(
    dropit(dta, n_drop = 1, criterion = "alpha",
           alpha_args = list(check.keys = TRUE, use = "complete.obs"),
           verbose = FALSE)
  ))
})

# dropit() muffles warnings into `$log$warnings` rather than re-signalling
# them, so a dropit()-level warning is checked by inspecting the log.
logged <- function(res, pattern) any(grepl(pattern, res[["log"]][["warnings"]]))

test_that("greedy dropping on data with missing values warns", {
  # The one place the sample can drift between rounds. It must be announced.
  dta <- toy_scale(n = 200, k = 5)
  dta[1:15, "i3"] <- NA
  res <- dropit(dta, n_drop = 2, criterion = "alpha", approach = "greedy",
                alpha_args = list(check.keys = TRUE), verbose = FALSE)
  expect_true(logged(res, "Greedy dropping and missing data"))
})

test_that("one-shot on missing data, and greedy on complete data, stay quiet", {
  # The warning must be specific to greedy + missing, or it becomes noise.
  complete <- toy_scale(n = 200, k = 5)
  missing  <- complete
  missing[1:15, "i3"] <- NA

  # one-shot + missing: no drift possible, single fit
  res_os <- dropit(missing, n_drop = 2, criterion = "alpha", approach = "oneshot",
                   alpha_args = list(check.keys = TRUE), verbose = FALSE)
  expect_false(logged(res_os, "Greedy dropping and missing data"))
  # greedy + complete: sample is fixed regardless of round
  res_gc <- dropit(complete, n_drop = 2, criterion = "alpha", approach = "greedy",
                   alpha_args = list(check.keys = TRUE), verbose = FALSE)
  expect_false(logged(res_gc, "Greedy dropping and missing data"))
})

test_that("warn_greedy_missing fires only on the greedy + missing combination", {
  dta_na <- toy_scale(n = 100, k = 4); dta_na[1:5, "i2"] <- NA
  dta_ok <- toy_scale(n = 100, k = 4)
  expect_warning(warn_greedy_missing(dta_na, "greedy"), "Greedy dropping")
  expect_no_warning(warn_greedy_missing(dta_na, "oneshot"))
  expect_no_warning(warn_greedy_missing(dta_ok, "greedy"))
})

# ------------------------------------------------------------------------------
# 5. The `checks` toggle -- one switch for simulation loops
#
# checks = FALSE silences every *advisory* guard (tie, small-scale, greedy +
# missing) in one place, without touching the drop decision or the hard input
# validation that prevents wrong results. This is what verbose = FALSE does
# NOT do: verbose only hides the printed summary; the guards still run.
# ------------------------------------------------------------------------------

test_that("checks = FALSE silences all three advisory guards", {
  # A single dataset that would trip every guard at once:
  #  - exact-duplicate column  (boundary tie)
  #  - a 2-item partition arm   (small-scale)
  #  - missing values + greedy  (sample drift)
  dta <- toy_with_exact_tie(n = 200, k = 4)  # i1 i2 i3 i4 i2_copy
  dta[1:10, "i3"] <- NA
  ptn <- c("A", "A", "A", "B", "B")          # arm B holds only 2 items

  args <- list(data = dta, partition = ptn, n_drop = 1, criterion = "alpha",
               approach = "greedy", alpha_args = list(check.keys = TRUE),
               verbose = FALSE)

  on  <- do.call(dropit, args)
  off <- do.call(dropit, c(args, list(checks = FALSE)))

  # every advisory present with checks on ...
  expect_true(logged(on, "Greedy dropping and missing data"))
  expect_true(logged(on, "not meaningful below 3 items"))
  # ... and none of them with checks off
  expect_false(logged(off, "Greedy dropping and missing data"))
  expect_false(logged(off, "not meaningful below 3 items"))
  expect_false(logged(off, "Tie at the drop boundary"))
})

test_that("checks = FALSE does not change which items are dropped", {
  dta <- toy_scale(n = 300, k = 6)
  for (apr in c("oneshot", "greedy")) {
    on  <- dropit(dta, n_drop = 2, criterion = "alpha", approach = apr,
                  alpha_args = list(check.keys = TRUE), verbose = FALSE)
    off <- dropit(dta, n_drop = 2, criterion = "alpha", approach = apr,
                  alpha_args = list(check.keys = TRUE), checks = FALSE, verbose = FALSE)
    expect_identical(on[["names"]], off[["names"]], info = apr)
  }
})

test_that("checks = FALSE still enforces hard input validation", {
  # The toggle disables advisory warnings only -- never the validation that
  # stops a run from silently producing nonsense.
  dta <- toy_scale(n = 100, k = 4)
  # n_drop exceeds available items: must error regardless of checks
  expect_error(dropit(dta, n_drop = 99, checks = FALSE, verbose = FALSE))
  # non-logical checks is itself rejected
  expect_error(dropit(dta, n_drop = 1, checks = "no", verbose = FALSE))
})
