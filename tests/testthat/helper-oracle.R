# ==============================================================================
# tests/testthat/helper-oracle.R
#
# Shared fixtures for tests whose expected values are known BEFORE any dropit
# code runs.
#
# House rule for this suite: an expectation must cite a source outside the
# function under test -- a generator's ground truth, an independently computed
# statistic, or hand arithmetic -- and say so in a comment. If a value can only
# be justified by running `dropit()`, it does not belong in a test. Two bugs
# reached production because expectations were read off the functions' own
# output, which freezes whatever the code already did.
# ==============================================================================

#' Toy scale with a known item-strength ordering.
#'
#' item k = common factor + noise, noise SD strictly increasing in k. So i1 is
#' the strongest indicator and i_k the weakest, by construction rather than by
#' measurement. Responses are discretised to a 5-point Likert scale, the
#' package's actual domain.
#'
#' @return data.frame with columns i1..ik, ordered strongest to weakest.
toy_scale <- function(n = 800, k = 5, seed = 42, sd_from = 0.30, sd_to = 1.60) {
  withr::with_seed(seed, {
    fct <- stats::rnorm(n)
    sds <- seq(sd_from, sd_to, length.out = k)
    dta <- as.data.frame(lapply(sds, function(s) {
      z <- fct + stats::rnorm(n, 0, s)
      as.integer(cut(z, c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf), labels = FALSE))
    }))
    names(dta) <- paste0("i", seq_len(k))
    dta
  })
}

#' The generator's promise, as a plain character vector: strongest to weakest.
toy_truth <- function(k = 5) paste0("i", seq_len(k))

#' Toy scale plus a pure-noise item, which must always rank last.
toy_with_noise_item <- function(n = 800, k = 4, seed = 42, name = "junk") {
  dta <- toy_scale(n = n, k = k, seed = seed)
  withr::with_seed(seed + 1, {
    dta[[name]] <- sample.int(5, n, replace = TRUE)  # unrelated to the factor
  })
  dta
}

#' Toy scale containing an exact duplicate column, i.e. a guaranteed exact tie.
#' Used to test tie handling; random data never produces exact ties.
toy_with_exact_tie <- function(n = 800, k = 4, seed = 42, name = "i2_copy") {
  dta <- toy_scale(n = n, k = k, seed = seed)
  dta[[name]] <- dta[["i2"]]
  dta
}

# --- independent ground truth from psych, not from dropit ---------------------

#' Item strength ordering (strongest to weakest) via item-rest correlations.
#'
#' `r.drop` is an independent read on item strength: it is computed by psych,
#' is directly interpretable (higher = item agrees more with the rest of the
#' scale), and is not the column the alpha path ranks on.
truth_by_r_drop <- function(dta) {
  a <- suppressWarnings(psych::alpha(dta, check.keys = TRUE))
  rd <- a[["item.stats"]][["r.drop"]]
  names(rd) <- sub("-$", "", rownames(a[["item.stats"]]))
  names(sort(rd, decreasing = TRUE))
}

# --- thin call wrappers, to keep expectations readable ------------------------

drop_alpha <- function(dta, n_drp, dir = "tail", anc = NULL, mtr = "raw_alpha") {
  suppressWarnings(oneshotdrop_alpha(
    dta, anc = anc, n_drp = n_drp, dir = dir,
    alp_mtr = mtr, alp_args = list(check.keys = TRUE)
  ))[["names"]]
}

#' Did evaluating `expr` emit a warning matching `pattern`?
#'
#' Targets one specific warning rather than asserting silence overall, because
#' the engines emit unrelated warnings of their own (e.g. psych reports a
#' non-positive-definite matrix whenever two items are perfectly collinear).
warned_with <- function(expr, pattern) {
  hit <- FALSE
  withCallingHandlers(
    force(expr),
    warning = function(w) {
      if (grepl(pattern, conditionMessage(w))) hit <<- TRUE
      invokeRestart("muffleWarning")
    }
  )
  hit
}

#' As drop_alpha(), but does NOT swallow warnings -- for testing the guards.
drop_alpha_raw <- function(dta, n_drp, dir = "tail", anc = NULL, mtr = "raw_alpha") {
  oneshotdrop_alpha(
    dta, anc = anc, n_drp = n_drp, dir = dir,
    alp_mtr = mtr, alp_args = list(check.keys = TRUE)
  )[["names"]]
}

drop_lambda <- function(dta, n_drp, dir = "tail", anc = NULL, mtr = "std.all") {
  suppressWarnings(oneshotdrop_lambda(
    dta, anc = anc, n_drp = n_drp, dir = dir,
    mmt_mdl = NULL, tgt_fct = NULL, lam_mtr = mtr,
    cfa_args = list(std.lv = TRUE)
  ))[["names"]]
}

#' Both criteria, for invariants that must hold regardless of criterion.
both_criteria <- list(alpha = drop_alpha, lambda = drop_lambda)
