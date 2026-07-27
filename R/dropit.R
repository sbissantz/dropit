#' Drop Items from a Psychometric Scale
#'
#' Removes the weakest (or strongest) items from a psychometric scale by either
#' (a) minimizing the decrease in Cronbach’s alpha or
#' (b) selecting the smallest absolute CFA loadings.
#' The function can operate on the full data set or within
#' user-defined column partitions.
#'
#' @section Methods:
#' * **Alpha** – ranks items using \code{\link[psych]{alpha}}, dropping those
#'   whose removal least reduces Cronbach’s alpha (a measure of internal
#'   consistency that equals true reliability when essential tau-equivalence
#'   holds).
#' * **Lambda** – fits a one-factor CFA via
#'   \code{\link[lavaan]{cfa}} and ranks items by absolute standardized
#'   loadings.
#'
#' @param data A `data.frame` of item responses
#'   (rows = respondents, columns = items). Must have at least one row
#'   and one column. Non-`data.frame` inputs are coerced.
#' @param anchor Optional character vector of item names to protect from 
#'   removal. These theoretical anchors will be shielded from the dropping 
#'   algorithm, ensuring they remain in the final subset regardless of their 
#'   statistical performance. Defaults to `NULL`.
#' @param partition Optional character vector of length `ncol(data)`
#'   giving a partition label for each column. Dropping is performed
#'   independently within each partition.
#' @param n_drop Integer scalar. The number of items to remove. Behavior 
#'   depends on the `partition` argument: if `partition = NULL` (the default), 
#'   this is the total number of items dropped from the full dataset. If 
#'   `partition` is specified, this is the number of items dropped from 
#'   *each* partition independently. Setting `n_drop = 0` acts as a safe 
#'   no-operation, returning the original dataset unmodified.
#' @param direction Character string, `"tail"` (default) or `"head"`.
#'   `"tail"` drops the weakest items; `"head"` drops the strongest.
#' @param criterion Character string, `"alpha"` (Cronbach’s alpha)
#'   or `"lambda"` (CFA loadings).
#' @param approach Character string, `"oneshot"` (single pass)
#'   or `"greedy"` (iterative dropping and refitting).
#' @param alpha_metric Character string naming the column of
#'   `psych::alpha$alpha.drop` used to rank items. Defaults to `"raw_alpha"`.
#'
#'   The two common choices answer different questions, and they can disagree:
#'
#'   * `"raw_alpha"` is the reliability of the **raw sum** of item responses,
#'     computed from the covariance matrix. This is the reliability of the
#'     score an administered form actually produces, so it is usually the
#'     decision-relevant quantity when abbreviating an instrument that will be
#'     scored by summing or averaging raw responses.
#'   * `"std.alpha"` is the reliability of the **standardized** composite,
#'     computed from the correlation matrix, i.e. of a score formed after
#'     z-scoring each item.
#'
#'   Because `"raw_alpha"` is variance-weighted, items that spread respondents
#'   more contribute more to it. That is appropriate for a raw-scored form —
#'   such items genuinely carry more of the total score variance — but it means
#'   the two metrics can rank items differently when item variances differ,
#'   even on a shared response scale. Pick the one that matches how the final
#'   form will be scored, and state the choice; do not switch metrics after
#'   inspecting results.
#'
#'   Note that below three items no `alpha.drop` column is interpretable: with
#'   two items every item-total correlation is the same single correlation, so
#'   the items are formally indistinguishable and any ordering reflects an
#'   artefact rather than a real difference between items. `dropit()` warns in
#'   that case.
#' @param alpha_args Named list of extra arguments for
#'   \code{\link[psych]{alpha}}. This is also where missing-data handling for
#'   the alpha criterion is set, via `use` (e.g. `use = "complete.obs"`) or
#'   `impute`; see the "Missing data" section.
#' @param measurement_model Optional character string containing a
#'   \link[lavaan]{model.syntax} specification. If `NULL`, a single-factor
#'   model with all items loading on one latent factor is used.
#' @param target_factor Character scalar giving the name of the factor
#'   whose loadings determine item ranking when multiple factors are
#'   present.
#' @param lambda_metric Character string indicating which solution matrix
#'   to extract from \code{\link[lavaan]{inspect}}
#'   (e.g., `"est"`, `"std"`, `"std.lv"`, `"std.nox"`, `"std.all"`).
#' @param cfa_args Named list of additional arguments passed to
#'   \code{\link[lavaan]{cfa}}. This is also where missing-data handling for
#'   the lambda criterion is set, via `missing` (e.g. `missing = "fiml"`);
#'   see the "Missing data" section.
#' @param seed Optional integer scalar. Sets the random seed for stochastic
#'   operations (e.g., CFA bootstrapping) to ensure reproducibility. The global 
#'   RNG state is temporarily modified and safely restored upon exit. 
#'   Defaults to `NULL`.
#' @param checks Logical; if `TRUE` (default) the advisory guards run — the
#'   boundary-tie, small-scale, and greedy-with-missing-data warnings. Set
#'   `FALSE` in a simulation loop, once you have validated the design, to skip
#'   them and avoid re-emitting the same advisories on every iteration. Note
#'   that `verbose = FALSE` does **not** do this: it only hides the printed
#'   summary, while the guards still run and still collect into `$log`. Hard
#'   input validation is unaffected by `checks` and always runs.
#' @param trace Logical; if `TRUE`, records what the machinery did internally
#'   into `$log$messages` — one entry per greedy round, naming the items still
#'   in play (for `criterion = "lambda"`, the model syntax actually fitted).
#'   Defaults to `FALSE`, since a greedy run over several partitions produces
#'   one entry per round per arm. Only `approach = "greedy"` has a history to
#'   record; one-shot fits a single model. Distinct from `verbose`, which
#'   governs the printed end-of-run summary rather than what is collected.
#' @param verbose Logical; if `TRUE` (default) prints a structured,
#'   color-formatted report of all messages, warnings, and errors
#'   captured during the run.
#'
#' @details
#' * Input validation is strict: missing or duplicated column names are
#'   fixed or rejected, and each partition must contain at least `n_drop`
#'   items.
#' * Arguments that are not applicable to the chosen method (e.g.,
#'   `alpha_args` when `criterion = "lambda"`) are detected automatically
#'   and reported once via an informational message.
#' * All messages, warnings, and errors from downstream calls are
#'   captured and printed together at the end when `verbose = TRUE`,
#'   formatted using the internal helper \code{colormsg()}.
#'
#' @section Missing data:
#' `dropit()` never deletes respondents itself. Missing values are handled by
#' the ranking engine, through its own native arguments, so the treatment is
#' visible in the call rather than hidden behind a wrapper option: pass `use`
#' or `impute` via `alpha_args` for the alpha criterion, and `missing` (for
#' example `missing = "fiml"`) via `cfa_args` for the lambda criterion. The two
#' engines default differently — \code{\link[psych]{alpha}} to pairwise
#' deletion, \code{\link[lavaan]{cfa}} to listwise — so a ranking's sample
#' follows whichever engine the chosen criterion uses. To compare alpha and
#' lambda on one identical sample, set matching options in both lists (or
#' restrict `data` to complete cases before calling).
#'
#' Under `approach = "greedy"` each round refits on a smaller set of items, so
#' with incomplete data the sample can shift from round to round as the engine
#' re-derives it. `dropit()` warns when this combination is requested (unless
#' `checks = FALSE`). If a fixed sample across rounds matters, choose your
#' missing-data method beforehand — for instance by restricting `data` to
#' complete cases before the call.
#'
#' @return
#' An object of class \code{dropit}, which is a list containing:
#' \describe{
#'   \item{names}{A character vector of dropped item names (or a named list of vectors if \code{partition} is used).}
#'   \item{subset}{The reduced \code{data.frame} (or a named list of \code{data.frame}s if \code{partition} is used).}
#'   \item{log}{A list containing \code{warnings} and \code{messages} captured during execution.}
#' }
#'
#' @seealso
#' [psych::alpha()], [lavaan::cfa()], [lavaan::lavInspect()].
#'
#' @examples
#' dat <- data.frame(
#'   i1 = c(1, 2, 3, 4, 5),
#'   i2 = c(2, 2, 3, 4, 4),
#'   i3 = c(1, 1, 2, 3, 4),
#'   i4 = c(4, 3, 2, 1, 1)
#' )
#'
#' dropit(dat, n_drop = 1, verbose = FALSE)
#' 
#' @export
dropit <- function(
  # core
  data = data.frame(),
  anchor = NULL,
  partition = NULL,
  n_drop = 1L,
  direction = c("tail", "head"),
  # method selection
  criterion = names(criterion_registry),
  approach = c("oneshot", "greedy"),
  # alpha-specific
  alpha_metric = c(
    "raw_alpha",
    "std.alpha",
    "G6(smc)",
    "average_r",
    "S/N",
    "alpha se",
    "var.r",
    "med.r"
  ),
  alpha_args = list(),
  # lambda-specific
  measurement_model = NULL,
  target_factor = NULL,
  lambda_metric = c("est", "std", "std.lv", "std.nox", "std.all"),
  cfa_args = list(),
  # reproducibility
  seed = NULL,
  # guards
  checks = TRUE,
  # reporting
  trace = FALSE,
  verbose = TRUE
) {

  ## ---- track user-supplied args ----

  mc <- match.call(expand.dots = FALSE)
  usr_spl <- names(as.list(mc)[-1]) 

  ## ---- variables with special needs ----

  # verbose
  # ---
  # msgs
  # wrns

  ## ---- verbose ----

  checkmate::assert_logical(
    verbose,
    any.missing = FALSE,
    all.missing = FALSE,
    len = 1
  )
  # short name
 vbs <- verbose

  ## ---- checks ----

  checkmate::assert_logical(
    checks,
    any.missing = FALSE,
    all.missing = FALSE,
    len = 1
  )
  # short name
  check <- checks

  ## ---- trace ----

  checkmate::assert_logical(
    trace,
    any.missing = FALSE,
    all.missing = FALSE,
    len = 1
  )
  # short name
  trc <- trace

  # collectors for report
  msgs <- character()
  wrns <- character()

  rtrn <- tryCatch(
    withCallingHandlers(
      {
        ## ---- general input validation ----

        # data
        # n_drop (note: order changed)
        # partition (note: order changed)
        # direction
        # criterion
        # approach

        ## ---- data ----
        if (!is.data.frame(data)) {
          # safely get the variable name, not the full expression
          data_nme <- deparse(substitute(data))
          # issue a single warning
          warning(
            sprintf("The value of '%s' was coerced to a data.frame.", data_nme),
            call. = FALSE
          )
          data <- as.data.frame(data, stringsAsFactors = FALSE)
        }
        checkmate::assert_data_frame(
          data, # original name for informative messages
          any.missing = TRUE,
          all.missing = FALSE,
          min.rows = 2,
          min.cols = 2,
          col.names = "unique"
        )
        # short name
        dta <- data # assign after validation

        ## ---- n_drop ----
        checkmate::assert_integerish(
          n_drop,
          lower = 0,             # Allow the 0 special case
          len = 1,               # Must be a single value
          any.missing = FALSE,   # No NAs allowed
          null.ok = FALSE
        )
        # short name
        n_drp <- n_drop
       
        # special case : n_drop = 0
        if (n_drp == 0L) {
          if (isTRUE(vbs)) {
            message("No items were dropped (n_drop = 0). Returning original.")
          }
          # Format 'names' output
          if (!is.null(partition)) {
            nms_orig <- lapply(
              split(seq_along(partition), partition), 
              function(x) character(0)
            )
          } else {
            nms_orig <- character(0)
          }
          # Format 'subset' output
          if (!is.null(partition)) {
            sub_orig <- lapply(
              split(seq_along(partition), partition), 
              function(x) dta[, x, drop = FALSE]
            )
          } else {
            sub_orig <- dta
          }
          # Format empty 'log' output
          log_orig <- structure(
            list(
              warnings = character(0), 
              messages = character(0)
            ), 
            class = c("dropit_log", "list")
          )
          # Assemble final object and exit immediately
          rtrn_orig <- list(
            names = nms_orig,
            subset = sub_orig,
            log = log_orig
          )
          class(rtrn_orig) <- c("dropit", "list")
          return(rtrn_orig)
        }

        ## ---- anchor ----
        checkmate::assert_character(
          anchor,
          null.ok = TRUE,
          any.missing = FALSE
        )
        if (!is.null(anchor)) {
          checkmate::assert_subset(anchor, colnames(dta))
        }
        # short name
        anc <- anchor

        ## ---- partition ----
        # note: 'partition' is validated after(!) 'n_drop' because I want the
        # validated 'n_drp' for the split (see below).

        if (!is.null(partition)) {
          checkmate::assert_character(
            partition,
            len = ncol(dta),
            any.missing = FALSE
          )
          splt_pos <- split(seq_along(partition), partition)
          bad_prts <- vapply(
            splt_pos,
            function(x) {
              cols_in_part <- colnames(dta[, x, drop = FALSE])
              n_drpbl <- length(setdiff(cols_in_part, anc))
              n_drpbl < n_drp
            },
            logical(1)
          )
          if (any(bad_prts)) {
            item_label <- if (is.null(anc)) "items" else "non-anchor items"
            # Formatted to match checkmate's exact style
            stop(sprintf(
              "Assertion on 'n_drop' failed: Partition(s) have fewer available %s than n_drop (%d): %s",
              item_label, n_drp, paste0(names(bad_prts)[bad_prts], collapse = ", ")
            ))
          }
        } else {
          n_drpbl <- ncol(dta) - length(anc)
          if (n_drp > n_drpbl) {
            item_label <- if (is.null(anc)) "items" else "non-anchor items"
            # Formatted to match checkmate's exact style
            stop(sprintf(
              "Assertion on 'n_drop' failed: n_drop (%d) exceeds the number of available %s (%d).",
              n_drp, item_label, n_drpbl
            ))
          }
        }

        # short name
        prtn <- partition

        ## ---- direction ----

        direction <- match.arg(direction)

        # short name
        dir <- direction

        ## ---- criterion ----

        criterion <- match.arg(criterion, names(criterion_registry))

        # short name
        crt <- criterion

        switch(
          crt,
          "alpha" = check_ignored(
            usr_spl,
            c("measurement_model", "target_factor", "lambda_metric", "cfa_args")
          ),
          "lambda" = check_ignored(
            usr_spl,
            c("alpha_metric", "alpha_args")
          )
        )

        ## ---- approach ----

        approach <- match.arg(approach)

        # short name
        apr <- approach

        # Advisory guards derivable from the inputs, run once, up front. The
        # in-flight boundary-tie check is gated by the same `check` flag, threaded
        # into naivedrop() below. Hard input validation above always runs.
        if (check) {
          preflight_checks(dta, prtn, n_drp, apr, crt)
        }

        ## ---- alpha-specific input validation ----

        # alpha_metric
        # alpha_args

        ## ---- alpha_metric ----

        alpha_metric <- match.arg(alpha_metric)

        # short name
        alp_mtr <- alpha_metric

        ## ---- alpha_args ----

        checkmate::assert_list(
          alpha_args,
          names = "unique",
          any.missing = FALSE
        )
        # short name
        alp_args <- alpha_args

        ## ---- lambda-specific input validation ----

        # measurement_model
        # target_factor
        # lambda_metric
        # cfa_args

        ## ---- measurement_model ----

        # important: not yet supported!

        # future work: custom measurement models. For now, return an error if the value is not null. The problem with the lavaan models is that they are too flexible and require careful handling to avoid errors.  For the current project, this level of detail is excessive.

        checkmate::assert_character(
          measurement_model,
          len = 1,
          null.ok = TRUE,
          any.missing = FALSE
        )
        if (!is.null(measurement_model)) {
          stop(
            "Custom measurement models are not yet supported. Please set 'measurement_model = NULL'."
          )
        }
        # short name
        mmt_mdl <- measurement_model

        ## ---- target_factor ----

        # important: not yet supported!

        checkmate::assert_character(
          target_factor,
          len = 1,
          null.ok = TRUE,
          any.missing = FALSE
        )
        if (!is.null(target_factor)) {
          stop(
            "Custom measurement models are not yet supported, so 'target_factor' cannot be specified. Please set 'target_factor = NULL'."
          )
        }
        # short name
        tgt_fct <- target_factor

        ## ---- lambda_metric ----

        lambda_metric <- match.arg(lambda_metric)

        # short name
        lam_mtr <- lambda_metric

        ## ---- cfa_args ----

        checkmate::assert_list(
          cfa_args,
          names = "unique",
          any.missing = FALSE
        )
        # short name
        # note: name is identical to input argument, so no need to reassign.

        ## ---- seed ----
        checkmate::assert_integerish(
          seed, 
          len = 1, 
          null.ok = TRUE, 
          any.missing = FALSE
        )
        
        if (!is.null(seed)) {
          # Safely manage the global seed state
          if (exists(".Random.seed", envir = .GlobalEnv)) {
            old_seed <- get(".Random.seed", envir = .GlobalEnv)
            on.exit(assign(".Random.seed", old_seed, envir = .GlobalEnv), add = TRUE)
          } else {
            on.exit(rm(".Random.seed", envir = .GlobalEnv), add = TRUE)
          }
          set.seed(seed)
        }

        ## --- general input validation (again) ---

        # criterion
        # verbose
         
        ## ---- main work ----

        if (!is.null(prtn)) {
          res_raw <- lapply(splt_pos, function(x) {
            naivedrop( # (or greedydrop, depending on your logic)
              dta = dta[, x, drop = FALSE],
              anc = anc,
              n_drp = n_drp,
              dir = dir,
              crt = crt,
              apr = apr,
              alp_mtr = alp_mtr,
              alp_args = alp_args,
              mmt_mdl = mmt_mdl,
              tgt_fct = tgt_fct,
              lam_mtr = lam_mtr,
              cfa_args = cfa_args,
              check = check,
              trace = trc
            )
          })
          names(res_raw) <- names(splt_pos)
          # Return transposed list OUT of the tryCatch block
          list(
            names = lapply(res_raw, `[[`, "names"),
            subset = lapply(res_raw, `[[`, "subset")
          )
        } else {
          res_raw <- naivedrop(
            dta = dta,
            anc = anc,
            n_drp = n_drp,
            dir = dir,
            crt = crt,
            apr = apr,
            alp_mtr = alp_mtr,
            alp_args = alp_args,
            mmt_mdl = mmt_mdl,
            tgt_fct = tgt_fct,
            lam_mtr = lam_mtr,
            cfa_args = cfa_args,
            check = check,
            trace = trc
          )    
          # Return the flat list OUT of the tryCatch block
          list(
            names = res_raw$names,
            subset = res_raw$subset
          )
        }
      },
      message = function(m) {
        # vip pass for the early exit message 
        if (grepl("No items were dropped", conditionMessage(m))) {
          return()
        }
        msgs <<- c(msgs, conditionMessage(m))
        invokeRestart("muffleMessage")
      },
      warning = function(w) {
        wrns <<- c(wrns, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      stop(conditionMessage(e), call. = FALSE)
    }
  ) # Returned list from above is now saved into 'rtrn'

  ## ---- Final report ----

  cln_msgs <- tally_conditions(msgs)
  cln_wrns <- tally_conditions(wrns)
  
  if (isTRUE(vbs)) {
    n_warn <- length(cln_wrns)
    n_msg <- length(cln_msgs)
    if (n_warn > 0 || n_msg > 0) {
      colormsg("Run", color_code = "38;5;67", bold = TRUE, newline = FALSE)
      cat(sprintf(" ended with %d ", n_warn))
      colormsg("warning(s)", color_code = "38;5;67", bold = TRUE, newline = FALSE)
      cat(sprintf(" and %d ", n_msg))
      colormsg("message(s)", color_code = "38;5;67", bold = TRUE, newline = FALSE)
      cat(". Access via `$log`\n")
    } 
  }

  ## ---- Return structured output ----

rtrn_final <- list(
    names = rtrn$names,
    subset = rtrn$subset,
    log = structure(
      list(
        warnings = cln_wrns,
        messages = cln_msgs
      ),
      class = c("dropit_log", "list")
    )
  )
  class(rtrn_final) <- c("dropit", "list")
  rtrn_final
}

# ------------------------------------------------------------------------------
# Internal reporting helpers for dropit()
# ------------------------------------------------------------------------------

#' Trim Leading and Trailing Newlines
#'
#' Removes leading and trailing newline characters from a string, used to tidy
#' collected warnings and messages before they are reported.
#'
#' @param x Character vector or scalar to process.
#' @return A character vector with leading/trailing newlines removed.
#' @keywords internal
trim_newlines <- function(x) {
  gsub("(^\\n+|\\n+$)", "", x)
}

#' Condense Repeated Conditions into a Counted, Unique Set
#'
#' A run may call the ranking engine many times — once per greedy round, and
#' again for every partition arm — so the same warning can surface dozens of
#' times. Reporting each occurrence buries the signal, but plain deduplication
#' hides how often a problem struck: a model that failed to converge once and
#' one that failed every round would read identically. This keeps one line per
#' distinct condition and appends a count when it occurred more than once.
#'
#' Conditions are flattened to a single line first: engine messages often wrap
#' across lines, so two textually identical conditions would otherwise fail to
#' match on line breaks alone.
#'
#' @param x Character vector of collected condition messages.
#' @return A character vector of unique messages in order of first occurrence,
#'   each suffixed with `(xN)` when it occurred `N > 1` times.
#' @keywords internal
tally_conditions <- function(x) {
  if (length(x) == 0L) return(character(0))
  flat <- gsub("\\s*\\n\\s*", " ", trim_newlines(x))
  uniq <- unique(flat)                      # order of first occurrence
  n <- tabulate(match(flat, uniq), nbins = length(uniq))
  ifelse(n > 1L, sprintf("%s (x%d)", uniq, n), uniq)
}

#' Report Ignored Arguments
#'
#' Emits an informational message when the user supplied arguments that do not
#' apply to the chosen method (e.g. `cfa_args` with `criterion = "alpha"`).
#'
#' @param usr_sup Character vector of argument names the user supplied.
#' @param ign_nms Character vector of argument names not applicable here.
#' @return Invisibly `NULL`; called for the side effect of messaging.
#' @keywords internal
check_ignored <- function(usr_sup, ign_nms) {
  bad <- intersect(usr_sup, ign_nms)
  if (length(bad)) {
    message(sprintf(
      "Argument(s) %s not applicable and ignored.",
      paste0("'", bad, "'", collapse = ", ")
    ))
  }
}