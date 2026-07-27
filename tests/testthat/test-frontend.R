# ==============================================================================
# tests/testthat/test-frontend.R
#
# The user-facing behaviour of dropit(): the parts that are the frontend's own
# responsibility rather than the ranking algorithms' -- output shape, the
# criterion x approach dispatch, partitions, anchors, the n_drop = 0 no-op,
# and seed handling. Ranking correctness itself lives in test-oracle.R and
# test-properties.R; this file assumes those hold and checks the plumbing
# around them.
# ==============================================================================

bfi5 <- psych::bfi[, 1:5]

# ------------------------------------------------------------------------------
# Output contract
# ------------------------------------------------------------------------------

test_that("dropit() returns the documented object structure", {
  res <- dropit(bfi5, n_drop = 1, criterion = "alpha",
                alpha_args = list(check.keys = TRUE), verbose = FALSE)
  expect_s3_class(res, "dropit")
  expect_named(res, c("names", "subset", "log"))
  expect_named(res$log, c("warnings", "messages"))
  expect_type(res$names, "character")
  expect_s3_class(res$subset, "data.frame")
  expect_equal(ncol(res$subset), ncol(bfi5) - 1)
})

test_that("every criterion x approach combination returns n_drop items", {
  # A smoke test for the dispatch: each of the four routes must run end to end
  # and return the requested number of items. (Which items is oracle's job.)
  for (crt in c("alpha", "lambda")) {
    for (apr in c("oneshot", "greedy")) {
      res <- suppressWarnings(dropit(
        bfi5, n_drop = 2, criterion = crt, approach = apr,
        alpha_args = list(check.keys = TRUE), cfa_args = list(std.lv = TRUE),
        verbose = FALSE
      ))
      expect_length(res$names, 2)
      expect_equal(ncol(res$subset), ncol(bfi5) - 2, info = sprintf("%s/%s", crt, apr))
    }
  }
})

test_that("dropit() runs end-to-end with direction = 'head'", {
  res <- dropit(bfi5, n_drop = 1, direction = "head", criterion = "lambda",
                cfa_args = list(std.lv = TRUE), verbose = FALSE)
  expect_type(res$names, "character")
  expect_length(res$names, 1)
})

# ------------------------------------------------------------------------------
# Partitions -- dropping runs independently within each arm
# ------------------------------------------------------------------------------

test_that("dropit() drops within each partition independently", {
  # 25 BFI items, one arm per trait (first letter of the item name).
  dta <- psych::bfi[1:500, -(26:28)]
  ptn <- substr(colnames(dta), 1, 1)
  res <- dropit(dta, partition = ptn, n_drop = 2L, criterion = "alpha",
                alpha_args = list(check.keys = TRUE), verbose = FALSE)$names
  # Per-facet weakest pairs, verified against item-rest correlations: the
  # dropped SET matches r.drop in all five facets (in C and O the two weakest
  # are near-tied, so their internal order differs slightly).
  expect_identical(res, list(
    A = c("A4", "A1"), C = c("C1", "C5"), E = c("E3", "E5"),
    N = c("N5", "N4"), O = c("O4", "O2")
  ))
  # each arm dropped exactly n_drop items
  expect_true(all(vapply(res, length, integer(1)) == 2L))
})

# ------------------------------------------------------------------------------
# Anchors -- shielded items are never dropped, whole-scale and per-partition
# ------------------------------------------------------------------------------

test_that("dropit() shields anchor items from removal", {
  res <- dropit(bfi5, anchor = c("A3", "A2"), n_drop = 2L, criterion = "alpha",
                approach = "greedy", alpha_args = list(check.keys = TRUE),
                verbose = FALSE)
  expect_length(res$names, 2)
  expect_false(any(c("A3", "A2") %in% res$names))
  expect_true(all(c("A3", "A2") %in% colnames(res$subset)))
})

test_that("dropit() shields anchors within each partition arm", {
  dta <- psych::bfi[, 1:10]          # A1-A5, C1-C5
  ptn <- substr(colnames(dta), 1, 1)
  res <- dropit(dta, anchor = c("A2", "C4"), partition = ptn, n_drop = 3,
                criterion = "alpha", alpha_args = list(check.keys = TRUE),
                verbose = FALSE)
  expect_true("A2" %in% colnames(res$subset$A))
  expect_true("C4" %in% colnames(res$subset$C))
  expect_false("A2" %in% res$names$A)
  expect_false("C4" %in% res$names$C)
})

# ------------------------------------------------------------------------------
# n_drop = 0 -- a safe no-op, so pipelines can include the zero-drop baseline
# ------------------------------------------------------------------------------

test_that("n_drop = 0 returns the original data untouched, whole-scale", {
  dta <- data.frame(v1 = c(1, 2, 3), v2 = c(2, 3, 4), v3 = c(3, 4, 5))
  expect_message(
    res <- dropit(dta, n_drop = 0L, verbose = TRUE),
    "No items were dropped \\(n_drop = 0\\). Returning original."
  )
  expect_s3_class(res, "dropit")
  expect_equal(res$names, character(0))
  expect_equal(res$subset, dta)
  expect_equal(res$log$warnings, character(0))
  expect_equal(res$log$messages, character(0))
})

test_that("n_drop = 0 returns each partition arm untouched", {
  dta <- data.frame(v1 = c(1, 2, 3), v2 = c(2, 3, 4), v3 = c(3, 4, 5))
  prt <- c("fct1", "fct1", "fct2")
  res <- dropit(dta, n_drop = 0L, partition = prt, verbose = FALSE)
  expect_type(res$names, "list")
  pos <- split(seq_along(prt), prt)
  for (p in names(pos)) {
    expect_equal(res$names[[p]], character(0))
    expect_equal(res$subset[[p]], dta[, pos[[p]], drop = FALSE])
  }
})

# ------------------------------------------------------------------------------
# Reproducibility -- seed is honoured and the global RNG state is restored
# ------------------------------------------------------------------------------

test_that("check_ignored() messages only about arguments that don't apply", {
  # dropit() calls this to tell the user when, say, cfa_args was passed to the
  # alpha path. No overlap means silence; overlap means one informative message.
  expect_silent(check_ignored(c("a", "b"), c("x", "y")))
  expect_message(
    check_ignored(c("alpha_args", "lambda_metric"), c("lambda_metric")),
    "Argument\\(s\\) 'lambda_metric' not applicable and ignored\\."
  )
  expect_message(
    check_ignored(c("a", "b", "c"), c("b", "c")),
    "Argument\\(s\\) 'b', 'c' not applicable and ignored\\."
  )
})

test_that("trim_newlines() strips only leading and trailing newlines", {
  expect_equal(trim_newlines("\n\nHello\n"), "Hello")
  expect_equal(trim_newlines("Hello\n\n"), "Hello")
  expect_equal(trim_newlines("\nHello\nWorld\n"), "Hello\nWorld")
  expect_equal(trim_newlines("Hello"), "Hello")
})

test_that("seed makes stochastic runs reproducible without leaking RNG state", {
  set.seed(100)
  simdta <- as.data.frame(replicate(6, rnorm(50)))
  names(simdta) <- paste0("v", 1:6)

  seed_init <- if (exists(".Random.seed", envir = .GlobalEnv)) {
    get(".Random.seed", envir = .GlobalEnv)
  } else {
    NULL
  }
  res1 <- suppressWarnings(dropit(simdta, n_drop = 1, seed = 42,
                                  cfa_args = list(se = "bootstrap", bootstrap = 2)))
  res2 <- suppressWarnings(dropit(simdta, n_drop = 1, seed = 42,
                                  cfa_args = list(se = "bootstrap", bootstrap = 2)))
  expect_equal(res1, res2)
  # global RNG state must be exactly as we left it
  if (is.null(seed_init)) {
    expect_false(exists(".Random.seed", envir = .GlobalEnv))
  } else {
    expect_equal(get(".Random.seed", envir = .GlobalEnv), seed_init)
  }
})

# ------------------------------------------------------------------------------
# Condition reporting -- one line per distinct problem, with its count
# ------------------------------------------------------------------------------

test_that("repeated conditions are condensed and counted", {
  # Deduplication alone would make "failed once" and "failed every round"
  # indistinguishable, so the count has to survive the condensing.
  expect_equal(tally_conditions(c("a", "a", "b")), c("a (x2)", "b"))
  expect_equal(tally_conditions(c("solo")), "solo")
  expect_equal(tally_conditions(character(0)), character(0))
})

test_that("condition tally keeps order of first occurrence", {
  # Chronological order is more informative than alphabetical: the first
  # problem reported is usually the one that explains the others.
  expect_equal(tally_conditions(c("z", "a", "z")), c("z (x2)", "a"))
})

test_that("multi-line conditions are flattened before matching", {
  # Engine warnings wrap across lines; two identical conditions must not fail
  # to match merely because of line breaks.
  expect_equal(tally_conditions(c("one\n  two", "one two")), "one two (x2)")
  expect_equal(tally_conditions("\nleading and trailing\n"), "leading and trailing")
})

test_that("trace records the greedy fit history, one entry per round", {
  # The point of tracing: greedy refits on a shrinking item set, and without
  # this the sequence of models is invisible to the user.
  dta <- toy_scale(n = 150, k = 5)
  res <- dropit(dta, n_drop = 3, criterion = "lambda", approach = "greedy",
                trace = TRUE, verbose = FALSE)
  expect_length(res$log$messages, 3)
  expect_match(res$log$messages[1], "Model \\(1/3\\)")
  # each round fits one fewer item than the last
  n_items <- lengths(regmatches(res$log$messages, gregexpr("i[0-9]", res$log$messages)))
  expect_equal(n_items, c(5L, 4L, 3L))
})

test_that("trace works for the alpha criterion too", {
  dta <- toy_scale(n = 150, k = 5)
  res <- suppressWarnings(dropit(
    dta, n_drop = 2, criterion = "alpha", approach = "greedy", trace = TRUE,
    alpha_args = list(check.keys = TRUE), verbose = FALSE
  ))
  expect_length(res$log$messages, 2)
  expect_match(res$log$messages[1], "Scale \\(1/2\\)")
})

test_that("trace defaults to FALSE and is independent of verbose", {
  # verbose governs the printed summary; trace governs what gets collected.
  # Leaving trace at its default must not populate the log.
  dta <- toy_scale(n = 150, k = 5)
  quiet <- dropit(dta, n_drop = 3, criterion = "lambda", approach = "greedy",
                  verbose = FALSE)
  expect_length(quiet$log$messages, 0)
  # verbose = TRUE alone still records nothing
  loud <- dropit(dta, n_drop = 3, criterion = "lambda", approach = "greedy",
                 verbose = TRUE)
  expect_length(loud$log$messages, 0)
})

test_that("trace is only meaningful for greedy, and one-shot stays silent", {
  # One-shot fits a single model, so there is no history to record.
  dta <- toy_scale(n = 150, k = 5)
  res <- dropit(dta, n_drop = 3, criterion = "lambda", approach = "oneshot",
                trace = TRUE, verbose = FALSE)
  expect_length(res$log$messages, 0)
})

test_that("trace must be a single logical", {
  dta <- toy_scale(n = 100, k = 4)
  expect_error(dropit(dta, n_drop = 1, trace = "yes", verbose = FALSE))
  expect_error(dropit(dta, n_drop = 1, trace = c(TRUE, FALSE), verbose = FALSE))
})

test_that("a warning repeated across greedy rounds is logged once, with a count", {
  # The real case the tally exists for: greedy refits per round, so a
  # reverse-keying warning fires on every round rather than once.
  dta <- toy_scale(n = 200, k = 5)
  dta[["i2"]] <- 6L - dta[["i2"]]   # reverse-keyed: psych warns on each fit
  res <- dropit(dta, n_drop = 3, criterion = "alpha", approach = "greedy",
                alpha_args = list(check.keys = TRUE), verbose = FALSE)
  keyed <- grep("check.keys", res$log$warnings, value = TRUE)
  expect_length(keyed, 1)                  # condensed to a single line ...
  expect_match(keyed, "\\(x[0-9]+\\)")     # ... carrying its occurrence count
})
