# ==============================================================================
# tests/testthat/test-validation.R
#
# Input validation: bad arguments are rejected before any model is fit, with a
# clear error. This is the hard validation that always runs (it is not gated by
# `checks`; that toggle governs only the advisory guards -- see
# test-safeguards.R).
# ==============================================================================

bfi5 <- psych::bfi[, 1:5]

# helper: the standard alpha-path arguments, so each test varies one thing
alpha_call <- function(...) {
  dropit(bfi5, criterion = "alpha", alpha_args = list(check.keys = TRUE),
         verbose = FALSE, ...)
}
lambda_call <- function(...) {
  dropit(bfi5, criterion = "lambda", cfa_args = list(std.lv = TRUE),
         verbose = FALSE, ...)
}

# ------------------------------------------------------------------------------
# Core arguments
# ------------------------------------------------------------------------------

test_that("n_drop must be a valid count", {
  expect_error(alpha_call(n_drop = -1L))
  # more items requested than exist
  expect_error(alpha_call(n_drop = ncol(bfi5) + 1L))
})

test_that("direction, criterion, approach are matched against their choices", {
  expect_error(alpha_call(n_drop = 1L, direction = "foobar"))
  expect_error(dropit(bfi5, n_drop = 1L, criterion = "foobar", verbose = FALSE))
  expect_error(alpha_call(n_drop = 1L, approach = "foobar"))
})

# ------------------------------------------------------------------------------
# Method-specific arguments
# ------------------------------------------------------------------------------

test_that("alpha arguments are validated", {
  expect_error(alpha_call(n_drop = 1L, alpha_metric = "foobar"))
  expect_error(dropit(bfi5, n_drop = 1L, criterion = "alpha",
                      alpha_args = "foobar", verbose = FALSE))
})

test_that("lambda arguments are validated", {
  expect_error(lambda_call(n_drop = 1L, lambda_metric = "foobar"))
  expect_error(dropit(bfi5, n_drop = 1L, criterion = "lambda",
                      cfa_args = "foobar", verbose = FALSE))
})

test_that("custom measurement model and target factor are rejected (unsupported)", {
  expect_error(lambda_call(n_drop = 1L, measurement_model = "foobar"))
  expect_error(lambda_call(n_drop = 1L, target_factor = "foobar"))
})

test_that("seed must be a single integerish value", {
  simdta <- data.frame(v1 = rnorm(10), v2 = rnorm(10))
  expect_error(dropit(simdta, seed = "123"), "Must be of type 'integerish'")
  expect_error(dropit(simdta, seed = c(1, 2)), "Must have length 1")
  expect_error(dropit(simdta, seed = 1.5), "Must be of type 'integerish'")
})

# ------------------------------------------------------------------------------
# Partitions
# ------------------------------------------------------------------------------

test_that("a partition with fewer droppable items than n_drop is rejected by name", {
  bad_partition <- c("Group1", "Group1", "Group2", "Group2", "Group3")
  expect_error(
    alpha_call(partition = bad_partition, n_drop = 2L),
    "Assertion on 'n_drop' failed: Partition\\(s\\) have fewer available items than n_drop \\(2\\): Group3"
  )
})

# ------------------------------------------------------------------------------
# Coercion and degenerate requests
# ------------------------------------------------------------------------------

test_that("a non-data.frame input is coerced, with the coercion logged", {
  mat <- matrix(1:24, ncol = 4, dimnames = list(NULL, c("i1", "i2", "i3", "i4")))
  res <- dropit(mat, n_drop = 1, criterion = "alpha",
                alpha_args = list(check.keys = TRUE), verbose = FALSE)
  expect_true(any(grepl("coerced to a data.frame", res$log$warnings)))
})

test_that("greedy dropping of every item errors rather than ranking a 1-item scale", {
  # Greedy refits round by round; the final round would fit a single item,
  # which the engine cannot rank. This must surface as an error, not a result.
  expect_error(suppressWarnings(
    alpha_call(n_drop = ncol(bfi5), approach = "greedy")
  ))
})
