# ==============================================================================
# tests/testthat/test-dropit.R 
# ==============================================================================

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
    alpha_metric = "raw_alpha",
    alpha_args = list(check.keys = TRUE),
    partition = ptn,
    verbose = TRUE
  )$names
  lst <- list(
    A = c("A3", "A2"),
    C = c("C2", "C4"),
    E = c("E1", "E2"),
    N = c("N2", "N3"),
    O = c("O5", "O3")
  )
  expect_identical(res, lst)
})
# ----------------------------------------------------------------------- 

test_that("dropit() produces correct results with 'partitions'", {
  # fake 'paritions' for testing
  dta_ptn <- psych::bfi[1:500, -(26:28)]
  ptn <- substr(colnames(dta_ptn), 1, 1)
  res <- dropit(
    data = dta_ptn,
    n_drop = 2L,
    direction = "tail",
    criterion = "alpha",
    approach = "oneshot",
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
  expect_identical(res$names, lst)
})

test_that("dropit() inherits output from oneshotdrop_alpha()", {
  res <- dropit(
    data = dta,
    n_drop = 4,
    direction = "tail",
    criterion = "alpha",
    approach = "oneshot",
    alpha_metric = "raw_alpha",
    alpha_args = list(check.keys = TRUE),
    verbose = FALSE
  ) 
  expect_equal(res$names, c("A4", "A5", "A2", "A3"))
})

test_that("dropit() inherits output from oneshotdrop_lambda()", {
  res <- dropit(
    data = dta,
    n_drop = 4,
    direction = "tail",
    criterion = "lambda",
    approach = "oneshot",
    measurement_model = NULL, 
    target_factor = NULL,
    lambda_metric = "std.all",
    cfa_args = list(std.lv = TRUE) 
  )
  expect_equal(res$names, c("A2", "A5", "A4", "A1"))
})

test_that("dropit() inherits output from greedydrop_alpha()", {
  res <- dropit(
    data = dta,
    n_drop = 4,
    direction = "tail",
    criterion = "alpha",
    approach = "greedy",
    alpha_metric = "raw_alpha",
    alpha_args = list(check.keys = TRUE),
    verbose = FALSE
  ) 
  expect_equal(res$names, c("A3", "A2", "A5", "A1"))
})

test_that("dropit() inherits output from greedydrop_lambda()", {
  res <- dropit(
    data = dta,
    n_drop = 3,
    direction = "tail",
    criterion = "lambda",
    approach = "greedy",
    measurement_model = NULL, 
    target_factor = NULL,
    lambda_metric = "std.all",
    cfa_args = list(std.lv = TRUE) 
  )
expect_equal(res$names, c("A1", "A4", "A2"))
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
    measurement_model = NULL, 
    target_factor = NULL,
    lambda_metric = "std.all",
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
    measurement_model = NULL,
    target_factor = NULL,
    lambda_metric = "std.all",
    cfa_args = list(std.lv = TRUE),
    verbose = TRUE 
  )
  expect_type(res$names, "character")
  expect_length(res$names, 1)
})

# -----------------------------------------------------------------------

test_that("dropit() always returns master list structure with logs", {
  res <- dropit(
    data = dta,
    n_drop = 1,
    direction = "tail",
    criterion = "alpha",
    approach = "oneshot",
    alpha_metric = "raw_alpha",
    alpha_args = list(check.keys = TRUE),
    verbose = FALSE
  )
  
  expect_s3_class(res, "dropit")
  expect_type(res, "list")
  expect_named(res, c("names", "subset", "log"))
  expect_named(res$log, c("warnings", "messages"))
  
  expect_type(res$names, "character")
  expect_s3_class(res$subset, "data.frame")
  expect_equal(ncol(res$subset), ncol(dta) - 1)
})

## ----------------------------------------------------------------------

test_that("dropit() with n_drop = 0 returns original data set", {
  res <- dropit(
    data = dta,
    n_drop = 0,
    direction = "tail",
    criterion = "alpha",
    approach = "oneshot",
    alpha_metric = "raw_alpha",
    alpha_args = list(check.keys = TRUE),
    verbose = TRUE 
  )
  expect_identical(dta, res$subset)
  expect_length(res$names, 0)
})

test_that("dropit() errors when n_drop > ncol(data) (explicit guard at top level)", {
  expect_error(
    dropit(
      data = dta,
      n_drop = ncol(dta) + 1L,
      direction = "tail",
      criterion = "alpha",
      approach = "oneshot",
      alpha_metric = "raw_alpha",
      alpha_args = list(check.keys = TRUE),
      verbose = FALSE
    )
  )
})

test_that("dropit() frontend correctly passes and shields anchor items", {
  res <- dropit(
    data = dta,
    anchor = c("A3", "A2"),
    n_drop = 2L,
    direction = "tail",
    criterion = "alpha",
    approach = "greedy",
    alpha_metric = "raw_alpha",
    alpha_args = list(check.keys = TRUE),
    verbose = FALSE
  )
  expect_equal(res$names, c("A5", "A4"))
  expect_false(any(c("A3", "A2") %in% res$names))
  expect_length(res$names, 2)
})