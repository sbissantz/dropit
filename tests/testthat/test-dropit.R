# ==============================================================================
# tests/testthat/test-dropit.R 
# ==============================================================================

library(testthat)
library(psych)
library(lavaan)

# agreableness items 
dta <- psych::bfi[, 1:5]

# ----------------------------------------------------------------------- 

test_that("dropit() produces debugged results with 'partitions'", { 
  # fake 'paritions' for testing
  dta_ptn <- psych::bfi[1:500, -(26:28)]
  ptn <- substr(colnames(dta_ptn), 1, 1)
  res <- dropit(
    data = dta_ptn,
    n_drop = 2L,
    direction = "tail",
    criterion = "alpha",
    approach = "oneshot",
    output_type = "names",
    alpha_metric = "raw_alpha",
    alpha_args = list(check.keys = TRUE),
    partition = ptn,
    verbose = TRUE
  )
  lst <- list(
    A = c("A3", "A2"),
    C = c("C2", "C4"),
    E = c("E1", "E2"),
    N = c("N2", "N3"),
    O = c("O5", "O3")
  )
  expect_identical(res, lst)
})

test_that("dropit() inherits output from oneshotdrop_alpha()", {
  res <- dropit(
    data = dta,
    n_drop = 4,
    direction = "tail",
    criterion = "alpha",
    approach = "oneshot",
    output_type = "names",
    alpha_metric = "raw_alpha",
    alpha_args = list(check.keys = TRUE),
    verbose = FALSE
  ) 
  expect_equal(res, c("A4", "A5", "A2", "A3"))
})

test_that("dropit() inherits output from oneshotdrop_lambda()", {
  res <- dropit(
    data = dta,
    n_drop = 4,
    direction = "tail",
    criterion = "lambda",
    approach = "oneshot",
    output_type = "names",
    measurement_model = NULL, 
    target_factor = NULL,
    lambda_metric = "std.all",
    cfa_args = list(std.lv = TRUE) 
  )
  expect_equal(res, c("A2", "A5", "A4", "A1"))
})

test_that("dropit() inherits output from greedydrop_alpha()", {
  res <- dropit(
    data = dta,
    n_drop = 4,
    direction = "tail",
    criterion = "alpha",
    approach = "greedy",
    output_type = "names",
    alpha_metric = "raw_alpha",
    alpha_args = list(check.keys = TRUE),
    verbose = FALSE
  ) 
  expect_equal(res, c("A3", "A2", "A5", "A1"))
})

test_that("dropit() inherits output from greedydrop_lambda()", {
  res <- dropit(
    data = dta,
    n_drop = 3,
    direction = "tail",
    criterion = "lambda",
    approach = "greedy",
    output_type = "names",
    measurement_model = NULL, 
    target_factor = NULL,
    lambda_metric = "std.all",
    cfa_args = list(std.lv = TRUE) 
  )
expect_equal(res, c("A1", "A4", "A2"))
})

# ----------------------------------------------------------------------- 

test_that("dropit() errors with invalid 'n_drop'", {
expect_error( 
  dropit(
    data = dta,
    n_drop = -1L,
    direction = "tail",
    criterion = "alpha",
    approach = "oneshot",
    output_type = "names",
    alpha_metric = "raw_alpha",
    alpha_args = list(check.keys = TRUE),
    verbose = FALSE
  ) 
)
})

test_that("dropit() errors with invalid 'direction'", {
expect_error( 
  dropit(
    data = dta,
    n_drop = 1L,
    direction = "foobar",
    criterion = "alpha",
    approach = "oneshot",
    output_type = "names",
    alpha_metric = "raw_alpha",
    alpha_args = list(check.keys = TRUE),
    verbose = FALSE
  ) 
)
})

test_that("dropit() errors with invalid 'criterion'", {
expect_error( 
  dropit(
    data = dta,
    n_drop = 1L,
    direction = "tail",
    criterion = "foobar",
    approach = "oneshot",
    output_type = "names",
    alpha_metric = "raw_alpha",
    alpha_args = list(check.keys = TRUE),
    verbose = FALSE
  ) 
)
})

test_that("dropit() errors with invalid 'approach'", {
expect_error( 
  dropit(
    data = dta,
    n_drop = 1L,
    direction = "tail",
    criterion = "alpha",
    approach = "foobar",
    output_type = "names",
    alpha_metric = "raw_alpha",
    alpha_args = list(check.keys = TRUE),
    verbose = FALSE
  ) 
)
})

test_that("dropit() errors with invalid 'output_type'", {
expect_error( 
  dropit(
    data = dta,
    n_drop = 1L,
    direction = "tail",
    criterion = "alpha",
    approach = "oneshot",
    output_type = "foobar",
    alpha_metric = "raw_alpha",
    alpha_args = list(check.keys = TRUE),
    verbose = FALSE
  ) 
)
})

test_that("dropit() errors with invalid 'alpha_metric'", {
expect_error( 
  dropit(
    data = dta,
    n_drop = 1L,
    direction = "tail",
    criterion = "alpha",
    approach = "oneshot",
    output_type = "names",
    alpha_metric = "foobar",
    alpha_args = list(check.keys = TRUE),
    verbose = FALSE
  ) 
)
})

test_that("dropit() errors with invalid 'alpha_args'", {
expect_error( 
  dropit(
    data = dta,
    n_drop = 1L,
    direction = "tail",
    criterion = "alpha",
    approach = "oneshot",
    output_type = "names",
    alpha_metric = "raw_alpha",
    alpha_args = "foobar",
    verbose = FALSE
  ) 
)
})

test_that("dropit() errors with custom measurement model", {
expect_error(
  dropit(
    data = dta,
    n_drop = 1L,
    direction = "tail",
    criterion = "lambda",
    approach = "greedy",
    output_type = "names",
    measurement_model = "foobar", 
    target_factor = NULL,
    lambda_metric = "std.all",
    cfa_args = list(std.lv = TRUE) 
  )
)
expect_error(
  dropit(
    data = dta,
    n_drop = 1L,
    direction = "tail",
    criterion = "lambda",
    approach = "greedy",
    output_type = "names",
    measurement_model = NULL, 
    target_factor = "foobar",
    lambda_metric = "std.all",
    cfa_args = list(std.lv = TRUE) 
  )
)
})

test_that("dropit() errors with invalid 'lambda_metric'", {
expect_error(
  dropit(
    data = dta,
    n_drop = 1L,
    direction = "tail",
    criterion = "lambda",
    approach = "greedy",
    output_type = "names",
    measurement_model = NULL, 
    target_factor = NULL,
    lambda_metric = "foobar",
    cfa_args = list(std.lv = TRUE) 
  )
)
})

test_that("dropit() errors with invalid 'cfa_args'", {
expect_error(
  dropit(
    data = dta,
    n_drop = 1L,
    direction = "tail",
    criterion = "lambda",
    approach = "greedy",
    output_type = "names",
    measurement_model = NULL, 
    target_factor = NULL,
    lambda_metric = "foobar",
    cfa_args = "foobar"
  )
)
})

# ----------------------------------------------------------------------- 

test_that("dropit() runs end-to-end with 'direction = head'", {
  res <- dropit(
    data = dta,
    n_drop = 1,
    direction = "head",
    criterion = "lambda",
    approach = "oneshot",
    output_type = "names",
    # custom measurement models currently not supported by oneshotdrop_lambda()
    measurement_model = NULL,
    target_factor = NULL,
    lambda_metric = "std.all",
    cfa_args = list(std.lv = TRUE),
    verbose = TRUE 
  )
  expect_type(res, "character")
  expect_length(res, 1)
})

# -----------------------------------------------------------------------

test_that("dropit() produces correct output_type", {
  # names
  out_names <- dropit(
    data = dta,
    n_drop = 1,
    direction = "tail",
    criterion = "alpha",
    approach = "oneshot",
    output_type = "names",
    alpha_metric = "raw_alpha",
    alpha_args = list(check.keys = TRUE),
    verbose = FALSE
  )
  expect_type(out_names, "character")

  # subset
  out_subset <- dropit(
    data = dta,
    n_drop = 2,
    direction = "tail",
    criterion = "alpha",
    approach = "oneshot",
    output_type = "subset",
    alpha_metric = "raw_alpha",
    verbose = FALSE
  )
  expect_s3_class(out_subset, "data.frame")
  expect_equal(ncol(out_subset), ncol(dta) - 2)

  # both
  out_both <- dropit(
    data = dta,
    n_drop = 1,
    direction = "tail",
    criterion = "alpha",
    approach = "oneshot",
    output_type = "both",
    alpha_metric = "raw_alpha",
    verbose = FALSE
  )
  expect_named(out_both, c("names", "subset"))
  expect_false(out_both$names %in% colnames(out_both$subset))
})

test_that('dropit() output_type = "debug" returns structured diagnostics', {
  dbg <- dropit(
    data = dta,
    n_drop = 1,
    direction = "tail",
    criterion = "alpha",
    approach = "oneshot",
    output_type = "debug",
    alpha_metric = "raw_alpha",
    alpha_args = list(check.keys = TRUE),
    verbose = FALSE 
  )
  expect_type(dbg, "list")
  expect_true(all(c("result", "warnings", "messages") %in% names(dbg)))
})

## ----------------------------------------------------------------------

test_that("dropit() with n_drop = 0 returns original data set", {
  res <- dropit(
    data = dta,
    n_drop = 0,
    direction = "tail",
    criterion = "alpha",
    approach = "oneshot",
    output_type = "subset",
    alpha_metric = "raw_alpha",
    alpha_args = list(check.keys = TRUE),
    verbose = TRUE 
  )
  expect_identical(dta, res)
  # Same with tail
})

test_that("dropit() with n_drop = 0 returns original data set", {
  res <- dropit(
    data = dta,
    n_drop = 0,
    direction = "tail",
    criterion = "alpha",
    approach = "oneshot",
    output_type = "subset",
    alpha_metric = "raw_alpha",
    alpha_args = list(check.keys = TRUE),
    verbose = TRUE 
  )
  expect_identical(dta, res)
  # Same with tail
})

test_that("dropit() errors when n_drop > ncol(data) (explicit guard at top level)", {
  expect_error(
    dropit(
      data = dta,
      n_drop = ncol(dta) + 1L,
      direction = "tail",
      criterion = "alpha",
      approach = "oneshot",
      output_type = "names",
      alpha_metric = "raw_alpha",
      alpha_args = list(check.keys = TRUE),
      verbose = FALSE
    )
  )
})