# ==============================================================================
# tests/testthat/test-dropit.R 
# ==============================================================================

# agreableness items 
dta <- psych::bfi[, 1:5]

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

test_that("dropit() errors when n_drop > ncol(data)", {
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

test_that("dropit() correctly shields anchors across multiple partitions", {
  dta_ptn <- psych::bfi[, 1:10] # A1-A5, C1-C5
  ptn <- substr(colnames(dta_ptn), 1, 1)
  # Anchor one item in A and one in C
  res <- dropit(
    data = dta_ptn,
    anchor = c("A2", "C4"),
    partition = ptn,
    n_drop = 3, # Drops 3 from A, 3 from C
    direction = "tail",
    criterion = "alpha",
    approach = "oneshot",
    alpha_args = list(check.keys = TRUE),
    verbose = FALSE
  )
  # Ensure anchors survived in their respective subsets
  expect_true("A2" %in% colnames(res$subset$A))
  expect_true("C4" %in% colnames(res$subset$C))
  # Ensure they were shielded from the dropped vectors
  expect_false("A2" %in% res$names$A)
  expect_false("C4" %in% res$names$C)
})

test_that("dropit records warning when non-data.frame input is coerced", {
  # Create a matrix with column names so it survives the col.names check
  mat <- matrix(1:24, ncol = 4, dimnames = list(NULL, c("i1", "i2", "i3", "i4")))
  res <- dropit(
    data = mat,
    n_drop = 1,
    direction = "tail",
    criterion = "alpha",
    approach = "oneshot",
    alpha_args = list(check.keys = TRUE),
    verbose = FALSE
  )
  # The warning should be safely captured in the log
  expect_true(any(grepl("coerced to a data.frame", res$log$warnings)))
})

test_that("dropit() errors when a partition has fewer droppable columns than n_drop", {
  bad_partition <- c("Group1", "Group1", "Group2", "Group2", "Group3")
  
  expect_error(
    dropit(
      data = dta,
      partition = bad_partition,
      n_drop = 2L, 
      direction = "tail",
      criterion = "alpha",
      approach = "oneshot",
      alpha_metric = "raw_alpha",
      alpha_args = list(check.keys = TRUE),
      verbose = FALSE
    ),
    # Added the Assertion prefix to the regex expectation
    regexp = "Assertion on 'n_drop' failed: Partition\\(s\\) have fewer available items than n_drop \\(2\\): Group3"
  )
})

test_that("dropit respects seed argument and preserves global RNG state", {
  set.seed(100)
  simdta <- data.frame(
    v1 = rnorm(50), v2 = rnorm(50), v3 = rnorm(50),
    v4 = rnorm(50), v5 = rnorm(50), v6 = rnorm(50)
  )
  # Capture global RNG state before dropit
  if (exists(".Random.seed", envir = .GlobalEnv)) {
    seed_init <- get(".Random.seed", envir = .GlobalEnv)
  } else {
    seed_init <- NULL
  }
  # Run dropit twice with identical seed and stochastic lavaan args
  # (Note: adjust 'anchor' or other required dropit arguments as needed for your package)
  res1 <- suppressWarnings(
    dropit(
      data = simdta,
      n_drop = 1,
      seed = 42,
      cfa_args=list(se="bootstrap", bootstrap=2)
    )
  )
  res2 <- suppressWarnings(
    dropit(
      data = simdta,
      n_drop = 1,
      seed = 42,
      cfa_args = list(se="bootstrap", bootstrap=2)
    )
  )
  # Check equality of outputs
  expect_equal(res1, res2)
  # Check global env seed was restored
  if (is.null(seed_init)) {
    expect_false(exists(".Random.seed", envir = .GlobalEnv))
  } else {
    final_seed <- get(".Random.seed", envir = .GlobalEnv)
    expect_equal(seed_init, final_seed)
  }
})

test_that("dropit() errors with invalid seeds", {
  simdta <- data.frame(v1 = rnorm(10), v2 = rnorm(10))
  # The seed must be a single integer-like number.
  # Expecting checkmate to throw an error for these:
  expect_error(
    dropit(data=simdta, seed="123"), 
    "Must be of type 'integerish'"
  )
  expect_error(
    dropit(data=simdta, seed=c(1, 2)), 
    "Must have length 1"
  )
  expect_error(
    dropit(data=simdta, seed = 1.5), 
    "Must be of type 'integerish'")
})

test_that("dropit acts as a no-op when n_drop = 0", {
  
  dta <- data.frame(
    "v1" = c(1, 2, 3), 
    "v2" = c(2, 3, 4), 
    "v3" = c(3, 4, 5)
  )
  # A: No partitions
  expect_message(
    res_orig <- dropit(data = dta, n_drop = 0L, verbose = TRUE),
    "No items were dropped \\(n_drop = 0\\). Returning original."
  )
  expect_s3_class(res_orig, "dropit")
  expect_equal(res_orig$names, character(0))           
  expect_equal(res_orig$subset, dta)                   
  expect_equal(res_orig$log$warnings, character(0))    
  expect_equal(res_orig$log$messages, character(0))    
  # B: With partitions
  prt <- c("fct1", "fct1", "fct2")
  res_orig_prt <- dropit(
    data = dta, 
    n_drop = 0L, 
    partition = prt, 
    verbose = FALSE
  )
  expect_s3_class(res_orig_prt, "dropit")
  expect_type(res_orig_prt$names, "list")
  # Calculate expected positions (no hardcoding)
  pos_expct <- split(seq_along(prt), prt)
  # Iterate through partitions exist and check they match
  for (p in names(pos_expct)) {
    expect_equal(res_orig_prt$names[[p]], character(0))
    expect_equal(
      res_orig_prt$subset[[p]], 
      dta[, pos_expct[[p]], drop = FALSE]
    )
  }
})
