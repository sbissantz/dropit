# ==============================================================================
# tests/testthat/test-print.R
# ==============================================================================

# Dummy objects for testing
dummy_df <- data.frame(i1 = 1:5, i2 = 1:5)

obj_base <- list(
  names=c("A1", "A2"),
  subset=dummy_df,
  log=list(warnings=character(0), messages=character(0))
)
class(obj_base) <- c("dropit", "list")

obj_part <- list(
  names=list(F1 = "A1", F2 = "B1"),
  subset=list(F1 = dummy_df, F2 = dummy_df),
  log=list(warnings=character(0), messages=character(0))
)
class(obj_part) <- c("dropit", "list")

obj_log <- obj_base
obj_log$log <- list(warnings="Test warning", messages=c("Msg 1", "Msg 2"))


# ------------------------------------------------------------------------------
# Tests for print.dropit
# ------------------------------------------------------------------------------

test_that("print.dropit() outputs correct headers for unpartitioned data", {
  expect_output(
    print(obj_base),
    "Dropped Items:"
  )
  expect_output(
    print(obj_base),
    "Subset\\(s\\):"
  )
  # str() output should be present
  expect_output(
    print(obj_base),
    "data.frame"
  )
})

test_that("print.dropit() handles partitioned (list) subsets", {
  expect_output(
    print(obj_part),
    "\\$F1"
  )
  expect_output(
    print(obj_part),
    "\\$F2"
  )
})

test_that("print.dropit() displays log summary only when logs exist", {
  # Base object has empty logs, so this string should NOT appear
  out_empty <- capture.output(print(obj_base))
  expect_false(any(grepl("Access via `\\$log`", out_empty)))
  
  # Log object has logs, so it SHOULD appear (using .* to ignore invisible ANSI color codes)
  expect_output(
    print(obj_log),
    "Run.*ended with 1.*warning\\(s\\).*and 2.*message\\(s\\).*logged"
  )
})

test_that("print.dropit() returns the object invisibly", {
  expect_invisible(res <- print(obj_base))
  expect_identical(res, obj_base)
})

# ------------------------------------------------------------------------------
# Tests for print.dropit_log
# ------------------------------------------------------------------------------

# Dummy logs
log_empty <- structure(list(warnings = character(0), messages = character(0)), class = c("dropit_log", "list"))
log_warn <- structure(list(warnings = "A single warning", messages = character(0)), class = c("dropit_log", "list"))
log_msg <- structure(list(warnings = character(0), messages = c("Msg A", "Msg B")), class = c("dropit_log", "list"))
log_both <- structure(list(warnings = "A single warning", messages = "Msg A"), class = c("dropit_log", "list"))


test_that("print.dropit_log() handles empty logs gracefully", {
  expect_output(
    print(log_empty),
    "Log is empty \\(0 warnings, 0 messages\\)\\."
  )
})

test_that("print.dropit_log() prints warnings correctly", {
  expect_output(
    print(log_warn),
    "Warning\\(s\\):"
  )
  expect_output(
    print(log_warn),
    "\\* A single warning"
  )
  # Should not print messages header
  out <- capture.output(print(log_warn))
  expect_false(any(grepl("Message\\(s\\):", out)))
})

test_that("print.dropit_log() prints messages correctly", {
  expect_output(
    print(log_msg),
    "Message\\(s\\):"
  )
  expect_output(
    print(log_msg),
    "\\* Msg A"
  )
  # Should not print warnings header
  out <- capture.output(print(log_msg))
  expect_false(any(grepl("Warning\\(s\\):", out)))
})

test_that("print.dropit_log() prints both sections when present", {
  out <- capture.output(print(log_both))
  expect_true(any(grepl("Warning\\(s\\):", out)))
  expect_true(any(grepl("Message\\(s\\):", out)))
})

test_that("print.dropit_log() returns the object invisibly", {
  expect_invisible(res <- print(log_both))
  expect_identical(res, log_both)
})