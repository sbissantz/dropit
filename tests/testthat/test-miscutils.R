# ==============================================================================
# tests/testthat/test-miscutils.R 
# ==============================================================================

test_that("colormsg prints correctly and returns NULL invisibly", {
  # capture printed output
  out <- capture.output(
    res <- colormsg("Hello", color_code = 31, bold = TRUE, newline = TRUE)
  )

  # output contains correct ANSI sequence
  expect_true(any(grepl("\033\\[1;31mHello\033\\[0m", out)))

  # function returns NULL invisibly
  expect_null(res)
})

test_that("trim_newlines removes only leading/trailing newlines", {
  expect_equal(trim_newlines("\n\nHello\n"), "Hello")
  expect_equal(trim_newlines("Hello\n\n"), "Hello")
  expect_equal(trim_newlines("\nHello\nWorld\n"), "Hello\nWorld")
  expect_equal(trim_newlines("Hello"), "Hello")
})

test_that("check_ignored prints message only when needed", {
  # 1. No overlap → no message
  expect_silent(check_ignored(c("a", "b"), c("x", "y")))

  # 2. Overlap → emits message
  expect_message(
    check_ignored(c("alpha_args", "lambda_metric"), c("lambda_metric")),
    regexp = "Argument\\(s\\) 'lambda_metric' not applicable and ignored\\."
  )

  # 3. Multiple ignored arguments
  expect_message(
    check_ignored(c("a", "b", "c"), c("b", "c")),
    regexp = "Argument\\(s\\) 'b', 'c' not applicable and ignored\\."
  )
})
