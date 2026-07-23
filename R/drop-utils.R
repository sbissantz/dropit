#' Rank items by score, strongest to weakest, dropping protected anchors.
#'
#' Returns the ranked scores as a named numeric vector: names are the candidate
#' items in rank order, values their scores. The scores travel with the ranking
#' so the trimming step can both take the ends and inspect the cut for a tie.
#' Every item score is on the common **item score (higher means 'keep')** scale;
#' see [score_items()].
#'
#' @keywords internal
#' @noRd
rank_items <- function(itmscr, anc) {
  ranked <- sort(itmscr, decreasing = TRUE)   # strongest to weakest
  ranked[!names(ranked) %in% anc]             # drop anchors, keep order + scores
}

#' Trim the n weakest (or strongest) items from a ranked vector.
#'
#' Given items already ranked strongest to weakest (see [rank_items()]),
#' `dir = "tail"` cuts the weakest `n_drp`, `dir = "head"` the strongest. The
#' boundary-tie check lives here because a tie only matters *at the cut*;
#' `check` gates it, so `dropit(checks = FALSE)` can skip it in simulation
#' loops.
#'
#' @keywords internal
#' @noRd
trim_items <- function(itmscr_rnk, n_drp, dir, check = TRUE) {
  if (check) warn_tie(itmscr_rnk, n_drp = n_drp, dir = dir)
  match.fun(dir)(names(itmscr_rnk), n_drp)
}

#' Criterion registry: the one place metric polarity is declared.
#'
#' Every ranking criterion must state how its raw metric maps onto the common
#' **item score (higher means 'keep')**. Going through the registry is the
#' only supported way to obtain that mapping, so a new criterion cannot be
#' wired up without answering the question a `decreasing =` flag lets you skip
#' — which is how an inverted alpha ranking once shipped unnoticed.
#'
#' @keywords internal
#' @noRd
criterion_registry <- list(
  alpha = list(
    lbl = "Cronbach's alpha (alpha-if-dropped)",
    # `alpha.drop` is alpha *without* the item, so it runs OPPOSITE to the
    # score: a HIGH value means removing the item improves the scale, i.e. the
    # item is weak. Negate so higher means 'keep'.
    score_rule = function(x) -x
  ),
  lambda = list(
    lbl = "CFA factor loading",
    # |loading| already points the keep way (higher = stronger item); the sign
    # carries keying direction only, so it is discarded.
    score_rule = abs
  )
)

#' Score items under a criterion by applying its scoring rule.
#'
#' Looks up the criterion in [criterion_registry] and applies its `score_rule`
#' to the raw metric `x`, returning item scores on the common scale where
#' **higher means 'keep'**.
#' @keywords internal
#' @noRd
score_items <- function(crt, x) {
  spec <- criterion_registry[[crt]]
  if (is.null(spec)) {
    known <- vapply(
      names(criterion_registry),
      function(k) sprintf("%s (%s)", k, criterion_registry[[k]][["lbl"]]),
      character(1)
    )
    stop(sprintf(
      paste0(
        "Unregistered criterion '%s'. Registered criteria: %s. Add a new one ",
        "to `criterion_registry` with its scoring rule (item score, higher ",
        "means 'keep')."
      ),
      crt, paste(known, collapse = ", ")
    ), call. = FALSE)
  }
  spec[["score_rule"]](x)
}

#' Warn when the drop boundary falls inside a tie.
#'
#' `order()` breaks ties by position, so a tied boundary means the result
#' depends on the column order of `dta` rather than on the data. That is a
#' silent, invisible coin flip; for a fixed test form it must be stated.
#'
#' @param itmscr_rnk Item scores of the drop candidates, already ranked
#'   strongest to weakest.
#' @keywords internal
#' @noRd
warn_tie <- function(itmscr_rnk, n_drp, dir, tol = 1e-8) {
  n <- length(itmscr_rnk)
  if (n_drp <= 0 || n_drp >= n) return(invisible(FALSE))
  # index of the last item on the kept side of the cut
  bnd <- if (identical(dir, "tail")) n - n_drp else n_drp
  hi <- itmscr_rnk[[bnd]]
  lo <- itmscr_rnk[[bnd + 1L]]
  if (abs(hi - lo) > tol * max(1, abs(hi), abs(lo))) return(invisible(FALSE))
  warning(sprintf(
    paste0(
      "Tie at the drop boundary: '%s' and '%s' have indistinguishable ",
      "item scores, so which one is dropped is decided by column order, ",
      "not by the data. Reordering the columns of `data` would change the ",
      "result."
    ),
    names(itmscr_rnk)[bnd], names(itmscr_rnk)[bnd + 1L]
  ), call. = FALSE)
  invisible(TRUE)
}

#' Warn when a scale is too small for its item statistics to mean anything.
#'
#' Below three items, item-level statistics stop being informative. With
#' exactly two items every item-rest correlation is the *same* single
#' correlation, so the items are formally indistinguishable; `raw_alpha`
#' nevertheless reports a difference, driven by unequal item variances rather
#' than by any real difference between items.
#'
#' @keywords internal
#' @noRd
warn_small <- function(n_itm, crt, min_itm = 3L) {
  if (n_itm >= min_itm) return(invisible(FALSE))
  warning(sprintf(
    paste0(
      "Ranking %d item(s) with criterion '%s': item statistics are not ",
      "meaningful below %d items. With 2 items every item-rest correlation ",
      "is the same single correlation, so any ordering reflects an artefact ",
      "(for `raw_alpha`, unequal item variances) rather than a real difference."
    ),
    n_itm, crt, min_itm
  ), call. = FALSE)
  invisible(TRUE)
}

#' Warn when greedy dropping runs on data that contains missing values.
#'
#' `dropit()` does not delete respondents itself; the ranking engine handles
#' missingness through its own arguments. Under the greedy approach each round
#' refits on a smaller item set, so the engine re-derives its sample every
#' round and the composition can drift as columns are removed. That drift is
#' invisible in the output, so it is stated. Pre-filtering `data` to complete
#' cases fixes the sample across rounds and silences the warning.
#'
#' @keywords internal
#' @noRd
warn_greedy_missing <- function(dta, apr) {
  if (!identical(apr, "greedy") || !anyNA(dta)) return(invisible(FALSE))
  warning(
    "Greedy dropping and missing data: each round refits on a smaller item ",
    "set, so the sample the ranking uses can change from round to round as ",
    "the engine re-derives it. Resolve the missing values before calling ",
    "(for instance by restricting `data` to complete cases) for a fixed ",
    "sample across rounds.",
    call. = FALSE
  )
  invisible(TRUE)
}

#' Run the input-derivable advisory checks once, before any model is fit.
#'
#' The guards split into two kinds. Checks that depend only on the inputs
#' (`data`, `partition`, `n_drop`, `approach`, `criterion`) can run up front,
#' and live here; the boundary-tie check needs the computed rankings and so
#' stays in-flight in [trim_items()]. `dropit()` calls this once when
#' `checks = TRUE`, and skips it (and the in-flight check) when `FALSE` — the
#' switch a simulation loop flips to avoid re-emitting the same advisories on
#' every iteration.
#'
#' The small-scale check is *predicted* rather than observed: the smallest
#' scale a run will ever rank is the full arm under `oneshot`, and
#' `m - n_drop + 1` items under `greedy` (each round removes one item). This
#' fires the warning once, up front, instead of once per greedy round.
#'
#' @keywords internal
#' @noRd
preflight_checks <- function(dta, prtn, n_drp, apr, crt) {
  # greedy + missing: the sample can drift between rounds
  warn_greedy_missing(dta, apr)
  # small-scale: check the smallest scale each partition arm will rank
  arms <- if (is.null(prtn)) list(colnames(dta)) else split(colnames(dta), prtn)
  for (cols in arms) {
    m <- length(cols)
    smallest <- if (identical(apr, "greedy")) m - n_drp + 1L else m
    warn_small(smallest, crt)
  }
  invisible(NULL)
}

# Internal Wrappers for psych / lavaan Calls to Simplify Testing
#
# These exist purely as seams: tests replace them with `local_mocked_bindings`
# to exercise error paths without fitting a real model.

#' @keywords internal
#' @noRd
lavaan_cfa_internal <- function(...) {
  lavaan::cfa(...)
}

#' @keywords internal
#' @noRd
lavaan_inspect_internal <- function(x, what) {
  lavaan::lavInspect(x, what)
}

#' @keywords internal
#' @noRd
psych_alpha_internal <- function(...) {
  psych::alpha(...)
}
