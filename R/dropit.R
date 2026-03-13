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
#' @param partition Optional character vector of length `ncol(data)`
#'   giving a partition label for each column. Dropping is performed
#'   independently within each partition.
#' @param n_drop Integer scalar. Number of items to remove in each
#'   partition.
#' @param direction Character string, `"tail"` (default) or `"head"`.
#'   `"tail"` drops the weakest items; `"head"` drops the strongest.
#' @param criterion Character string, `"alpha"` (Cronbach’s alpha)
#'   or `"lambda"` (CFA loadings).
#' @param approach Character string, `"oneshot"` (single pass)
#'   or `"greedy"` (iterative dropping and refitting).
#' @param output_type One of `"names"` (default), `"subset"`, `"both"`,
#'   or `"debug"`.
#'   * `"names"` – character vector of dropped item names.
#'   * `"subset"` – reduced data frame with dropped columns removed.
#'   * `"both"` – list with elements `names` and `subset`.
#'   * `"debug"` – as `"both"` but also includes all captured messages,
#'     warnings, and errors.
#'
#' @param alpha_metric Character string. Cronbach’s alpha metric to
#'   optimise (passed to `psych::alpha$alpha.drop`).
#' @param alpha_args Named list of extra arguments for
#'   \code{\link[psych]{alpha}}.
#'
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
#'   \code{\link[lavaan]{cfa}}.
#'
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
#' @return
#' If `output_type != "debug"`, returns either a character vector, data frame,
#' or list depending on the specified output type.  
#' If `output_type = "debug"`, returns a list with components:
#' \describe{
#'   \item{result}{The main result (or `NULL` if an error occurred).}
#'   \item{warnings}{Character vector of captured warnings.}
#'   \item{messages}{Character vector of informational messages.}
#' }
#'
#' @seealso
#' [psych::alpha()], [lavaan::cfa()], [lavaan::inspect()],
#' and the internal helpers documented at [miscutils].
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
  partition = NULL,
  n_drop = 1L,
  direction = c("tail", "head"),
  # method selection
  criterion = c("alpha", "lambda"),
  approach = c("oneshot", "greedy"),
  # output type
  output_type = c("names", "subset", "both", "debug"),
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
  # reporting
  verbose = TRUE
) {

  ## ---- track user-supplied args ----

  mc <- match.call(expand.dots = FALSE)
  usr_spl <- names(as.list(mc)[-1]) 

  ## ---- variables with special needs ----

  # verbose
  # output_type
  # ---
  # msgs
  # wrns

  ## ---- output_type ----

  output_type <- match.arg(output_type)
  checkmate::assert_character(
    output_type,
    len = 1,
    any.missing = FALSE
  )
  out <- match.arg(output_type) 
  out_tmp <- switch(
    out,
    debug = "both",
    out # default: same as input
  )

  ## ---- verbose ----

  checkmate::assert_logical(
    verbose,
    any.missing = FALSE,
    all.missing = FALSE,
    len = 1
  )
  # short name
 vbs <- verbose

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
        # output_type

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
        if (!is.integer(n_drop)) {
          # safely get the variable name, not the full expression
          n_drop_nme <- deparse(substitute(n_drop))
          # issue a single warning
          warning(
            sprintf("The value of '%s' was coerced to an integer.", n_drop_nme),
            call. = FALSE
          )
          n_drop <- as.integer(n_drop)
        }
        checkmate::assert_integer(
          n_drop,
          len = 1,
          lower = 0,
          upper = ncol(dta)-1,
          any.missing = FALSE
        )
        # short name
        n_drp <- n_drop
       
        # special case : n_drop = 0
        if (n_drp == 0L) {
          if (isTRUE(vbs)) {
            message("No items were dropped (n_drop = 0). Returning input unchanged.")
          }
        }

        ## ---- partition ----

        # TODO: debug with example partition, to see if it works.
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
            function(splt_pos) ncol(dta[, splt_pos, drop = FALSE]) < n_drp,
            logical(1)
          )
          if (any(bad_prts)) {
            stop(sprintf(
              "Partition(s) have fewer columns than n_drop=%d: %s",
              n_drp,
              paste0(names(bad_prts)[bad_prts], collapse = ", ")
            ))
          }
        }

        # short name
        prtn <- partition

        ## ---- direction ----

        direction <- match.arg(direction)
        checkmate::assert_character(
          direction,
          len = 1,
          any.missing = FALSE
        )
        # short name
        dir <- direction

        ## ---- criterion ----

        criterion <- match.arg(criterion)
        checkmate::assert_character(
          criterion,
          len = 1,
          any.missing = FALSE
        )
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
          ),
          stop("Debug: Invalid criterion specified. Use 'alpha' or 'lambda'.")
        )

        ## ---- approach ----

        approach <- match.arg(approach)
        checkmate::assert_character(
          approach,
          len = 1,
          any.missing = FALSE
        )
        # short name
        apr <- approach

        ## ---- alpha-specific input validation ----

        # alpha_metric
        # alpha_args

        ## ---- alpha_metric ----

        alpha_metric <- match.arg(alpha_metric)
        checkmate::assert_character(
          alpha_metric,
          len = 1,
          any.missing = FALSE
        )
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
            "Custom measurement models are not yet supported., so 'target_factor' cannot be specified. Please set 'target_factor = NULL'."
          )
        }
        # short name
        tgt_fct <- target_factor

        ## ---- lambda_metric ----

        lambda_metric <- match.arg(lambda_metric)
        checkmate::assert_character(
          lambda_metric,
          len = 1,
          any.missing = FALSE
        )
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

        ## --- general input validation (again) ---

        # criterion
        # verbose

        ## ---- main work ----

        if (!is.null(prtn)) {
          res <- lapply(splt_pos, function(idx) {
            naivedrop(
              dta = dta[, idx, drop = FALSE],
              n_drp,
              dir,
              crt,
              apr,
              out_tmp, # use temporary output type
              alp_mtr,
              alp_args,
              mmt_mdl,
              tgt_fct,
              lam_mtr,
              cfa_args
            )
          })
          names(res) <- names(splt_pos)
          res
        } else {
          naivedrop(
            dta,
            n_drp,
            dir,
            crt,
            apr,
            out_tmp, # use temporary output type
            alp_mtr,
            alp_args,
            mmt_mdl,
            tgt_fct,
            lam_mtr,
            cfa_args
          )
        }
      },
      message = function(m) {
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
  )

  ## ---- Final report ----

if (isTRUE(vbs) && length(c(msgs, wrns)) > 0) {

  # header
  colormsg(strrep("-", 10), color_code = "38;5;245", newline = TRUE)
  colormsg(
    "Run ended with the following issues:",
    color_code = "38;5;247",
    bold = TRUE,
    newline = TRUE
  )
  colormsg(strrep("-", 3), color_code = "38;5;245", newline = TRUE)

  # warnings 
  if (length(wrns)) {
    wrn_text <- paste0(" * ", trim_newlines(unique(wrns)), collapse = "\n")
    colormsg("Warnings", color_code = "38;5;187", bold = TRUE, newline = TRUE)
    colormsg(wrn_text, color_code = "38;5;245", newline = TRUE)
  }

  # divider before msgs if both sections exist
  if (length(wrns) && length(msgs)) {
    colormsg(strrep("-", 3), color_code = "38;5;245", newline = TRUE)
  }
  
  # messages
  if (length(msgs)) {
    msg_text <- paste0(" * ", trim_newlines(unique(msgs)), collapse = "\n")
    colormsg("Messages", color_code = "38;5;151", bold = TRUE, newline = TRUE)
    colormsg(msg_text, color_code = "38;5;245", newline = TRUE)
  }

  # footer
  colormsg(strrep("-", 10), color_code = "38;5;245", newline = TRUE)
}

  ## ---- Return structured output ----

  if (out == "debug") {
    return(list(
      result = rtrn,
      warnings = wrns,
      messages = msgs
    ))
  } else {
    rtrn
  }
}
