# ==============================================================================
# tests/testthat/test-miscutils.R 
# ==============================================================================

test_that("colormsg handles bold = TRUE and newline = TRUE", {

  out <- capture.output(
    res <- colormsg("Hello", color_code = 31, bold = TRUE, newline = TRUE)
  )

  expect_true(any(grepl("\033[1;31mHello\033[0m", out, fixed = TRUE)))
  expect_null(res)

})

test_that("colormsg handles bold = FALSE and newline = FALSE", {

  out <- capture.output(
    res <- colormsg("Hello", color_code = 31, bold = FALSE, newline = FALSE)
  )

  expect_true(any(grepl("\033[31mHello\033[0m", out, fixed = TRUE)))
  expect_null(res)

})

test_that("colormsg handles mixed argument combinations", {

  out1 <- capture.output(
    colormsg("Hi", color_code = 32, bold = TRUE, newline = FALSE)
  )

  expect_true(any(grepl("\033[1;32mHi\033[0m", out1, fixed = TRUE)))

  out2 <- capture.output(
    colormsg("Hi", color_code = 32, bold = FALSE, newline = TRUE)
  )

  expect_true(any(grepl("\033[32mHi\033[0m", out2, fixed = TRUE)))

})

test_that("trim_newlines removes only leading/trailing newlines", {

  expect_equal(trim_newlines("\n\nHello\n"), "Hello")
  expect_equal(trim_newlines("Hello\n\n"), "Hello")
  expect_equal(trim_newlines("\nHello\nWorld\n"), "Hello\nWorld")
  expect_equal(trim_newlines("Hello"), "Hello")

})

test_that("check_ignored prints message only when needed", {

  # No overlap → no message
  expect_silent(check_ignored(c("a", "b"), c("x", "y")))

  # Single ignored argument
  expect_message(
    check_ignored(c("alpha_args", "lambda_metric"), c("lambda_metric")),
    regexp = "Argument\\(s\\) 'lambda_metric' not applicable and ignored\\."
  )

  # Multiple ignored arguments
  expect_message(
    check_ignored(c("a", "b", "c"), c("b", "c")),
    regexp = "Argument\\(s\\) 'b', 'c' not applicable and ignored\\."
  )

})


test_that("make_test_data returns the expected default dataset", {

  dat <- make_test_data()

  expect_s3_class(dat, "data.frame")
  expect_equal(nrow(dat), 6)
  expect_equal(ncol(dat), 4)
  expect_named(dat, c("i1", "i2", "i3", "i4"))

  expect_equal(dat$i1, 1:6)
  expect_equal(dat$i2, 2:7)
  expect_equal(dat$i3, c(2, 3, 4, 1, 2, 3))
  expect_equal(dat$i4, 6:1)

})

test_that("make_test_data respects the n argument", {

  dat <- make_test_data(4)

  expect_s3_class(dat, "data.frame")
  expect_equal(nrow(dat), 4)
  expect_equal(ncol(dat), 4)
  expect_named(dat, c("i1", "i2", "i3", "i4"))

  expect_equal(dat$i1, 1:4)
  expect_equal(dat$i2, 2:5)
  expect_equal(dat$i3, c(2, 3, 4, 1))
  expect_equal(dat$i4, 4:1)

})

test_that("make_test_data returns a fresh data frame on each call", {

  dat1 <- make_test_data()
  dat2 <- make_test_data()

  dat1$i1[1] <- 999

  expect_equal(dat2$i1[1], 1)

})