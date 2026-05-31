#' Internal Helper Functions for Item Dropping
#'
#' Low-level utilities used internally by [dropit()] to implement the
#' different item–removal strategies.  
#' These functions are **not** intended for direct use by end-users but
#' are documented for developers who may wish to extend or debug the
#' algorithm.
#'
#' @section Helper Overview:
#' \describe{
#'   \item{naivedrop()}{Dispatch wrapper that calls the correct algorithm
#'      based on `criterion` (`"alpha"` or `"lambda"`) and
#'      `approach` (`"oneshot"` or `"greedy"`).}
#'   \item{greedydrop_alpha()}{Iterative (“greedy”) removal using
#'      Cronbach’s alpha.}
#'   \item{oneshotdrop_alpha()}{Single–pass removal using Cronbach’s alpha.}
#'   \item{greedydrop_lambda()}{Iterative (“greedy”) removal using CFA
#'      factor loadings.}
#'   \item{oneshotdrop_lambda()}{Single–pass removal using CFA
#'      factor loadings.}
#' }
#'
#' @param dta A `data.frame` of item responses (rows = respondents,
#'   columns = items).
#' @param anc Character vector of protected item names. Passed internally 
#'   from the `anchor` argument in `dropit()`.
#' @param n_drp Integer scalar giving the number of items to remove.
#' @param dir Character string, either `"tail"` or `"head"`, passed to
#'   [utils::tail()] / [utils::head()] to select the weakest or strongest
#'   items.
#' @param crt Character string, `"alpha"` or `"lambda"`,
#'   indicating the ranking criterion.
#' @param apr Character string, `"oneshot"` or `"greedy"`,
#'   selecting the removal strategy.
#' @param alp_mtr Character string naming the column of
#'   `psych::alpha$alpha.drop` used for ranking.
#' @param alp_args Named list of additional arguments passed to
#'   [psych::alpha()].
#'
#' @param mmt_mdl Character string of lavaan model syntax for the CFA.
#'   If `NULL`, a single-factor model is created automatically.
#' @param tgt_fct Character scalar giving the name of the target latent
#'   factor used for ranking.  Required when the fitted CFA has multiple
#'   factors.
#' @param lam_mtr Character string passed to
#'   [lavaan::lavInspect()] selecting which standardized solution matrix
#'   to extract (e.g., `"std"`).
#' @param cfa_args Named list of additional arguments passed to
#'   [lavaan::cfa()].
#'
#' @details
#' * Alpha methods call [psych::alpha()] with `check.keys = TRUE`.
#' * Lambda methods call [lavaan::cfa()] (defaulting to `std.lv = TRUE`
#'   unless overridden) and extract the `"lambda"` matrix from
#'   [lavaan::lavInspect()].
#' * All helpers validate that the number of extracted loadings matches
#'   the number of columns in `dta` and throw an error otherwise.
#'
#' @return
#' Depending on `out`:
#' \itemize{
#'   \item `"names"` – character vector of dropped item names.
#'   \item `"subset"` – reduced `data.frame` with the dropped items removed.
#'   \item `"both"` – list with elements `names` and `subset`.
#' }
#'
#' @name dropit-helpers 
#' @seealso [dropit()], [psych::alpha()], [lavaan::cfa()]
#' @keywords internal
NULL

#' @rdname dropit-helpers
#' @keywords internal
naivedrop <- function(
  # core
  dta,
  anc,
  n_drp,
  dir,
  # method selection
  crt,
  apr,
  # alpha-specific
  alp_mtr,
  alp_args,
  # lambda-specific
  mmt_mdl,
  tgt_fct,
  lam_mtr,
  cfa_args,
  # reporting
  verbose = FALSE
) {
  # TODO Consistent ordering of args across all functions
  switch(
    crt,
    "alpha" = switch(
      apr,
      "oneshot" = oneshotdrop_alpha(
        dta = dta,
        anc = anc,
        n_drp = n_drp,
        dir = dir,
        alp_mtr = alp_mtr,
        alp_args = alp_args
      ),
      "greedy" = greedydrop_alpha(
        dta = dta,
        anc = anc,
        n_drp = n_drp,
        dir = dir,
        alp_mtr = alp_mtr,
        alp_args = alp_args
      ),
      # Should never reached if input validation works properly 
      stop("Debug: Invalid criterion specified. Use 'oneshot' or 'greedy'.")
    ),
    "lambda" = switch(
      apr,
      "oneshot" = oneshotdrop_lambda(
        dta = dta,
        anc = anc,
        n_drp = n_drp,
        dir = dir,
        mmt_mdl = mmt_mdl,
        tgt_fct = tgt_fct,
        lam_mtr = lam_mtr,
        cfa_args = cfa_args,
        verbose = verbose
      ),
      "greedy" = greedydrop_lambda(
        dta = dta, 
        anc = anc,
        n_drp = n_drp,
        dir = dir, 
        mmt_mdl = mmt_mdl,
        tgt_fct = tgt_fct,
        lam_mtr = lam_mtr,
        cfa_args = cfa_args,
        verbose = verbose
      ),
      # Should never reached if input validation works properly 
      stop("Debug: Invalid criterion specified. Use 'oneshot' or 'greedy'.")
    ),
    # Should never reached if input validation works properly 
    stop("Debug: Invalid criterion specified. Use 'alpha' or 'lambda'.")
  )
}

#' @rdname dropit-helpers 
#' @keywords internal
greedydrop_lambda <- function(
  # core
  dta,
  anc, 
  n_drp,
  dir,
  # lambda-specific
  mmt_mdl, 
  tgt_fct,
  lam_mtr,
  cfa_args,
  # reporting
  verbose = FALSE
) {
  itm_drp <- rep(NA_character_, n_drp)
  itm_nms <- colnames(dta)
for (i in seq_len(n_drp)) {
    # current items
    itms_cur <- setdiff(itm_nms, itm_drp[nchar(itm_drp) > 0])
    if (verbose) {
      mdl_str <- paste0("F =~ ", paste0(itms_cur, collapse = " + "))
      message(sprintf("Model (%d/%d)  %s", i, n_drp, mdl_str))
    }
    updatedta <- dta[, setdiff(itm_nms, stats::na.omit(itm_drp)), drop = FALSE]
    itm_drp[i] <- oneshotdrop_lambda(
      dta = updatedta,
      anc = anc,
      n_drp = 1,
      dir = dir,
      mmt_mdl = mmt_mdl,
      tgt_fct = tgt_fct,
      lam_mtr = lam_mtr,
      cfa_args = cfa_args,
      verbose = FALSE
    )[["names"]]
  }
  list(
    names = itm_drp,
    subset = dta[, setdiff(colnames(dta), itm_drp), drop = FALSE]
  )
}

#' @rdname dropit-helpers
#' @keywords internal
greedydrop_alpha <- function(
  # core
  dta,
  anc,
  n_drp,
  dir,
  # alpha-specific 
  alp_mtr,
  alp_args 
) {
  itm_drp <- rep(NA_character_, n_drp)
  itm_nms <- colnames(dta)
for (i in seq_len(n_drp)) {
    updatedta <- dta[, setdiff(itm_nms, stats::na.omit(itm_drp)), drop = FALSE]
    itm_drp[i] <- oneshotdrop_alpha(
      dta = updatedta,
      anc = anc,
      n_drp = 1,
      dir = dir,
      alp_mtr = alp_mtr,
      alp_args = alp_args
    )[["names"]]
  }
  list(
    names = itm_drp,
    subset = dta[, setdiff(colnames(dta), itm_drp), drop = FALSE]
  )
}


#' @rdname dropit-helpers 
#' @keywords internal
oneshotdrop_lambda <- function(
  # core
  dta,
  anc,
  n_drp,
  dir,
  # lambda-specific
  mmt_mdl, 
  tgt_fct,
  lam_mtr,
  cfa_args,
  # reporting
  verbose = FALSE
) {
  itm_nms <- colnames(dta)
  if (is.null(mmt_mdl)) {
    mmt_mdl <- paste0("F =~ ", paste0(itm_nms, collapse = " + "))
  } else {
   # Should never be reached if the input validation works properly 
   stop("Debug: Custom measurement models are not yet supported.") 
  }
  arg_ls <- c(
    list(model = mmt_mdl, data = dta),
    cfa_args
  )
  fit <- do.call(lavaan_cfa_internal, arg_ls)
  fit_info <- lavaan_inspect_internal(fit, lam_mtr)
  lam_mat <- fit_info[["lambda"]]
  if (is.null(lam_mat) || !is.matrix(lam_mat)) {
    stop(sprintf(
      "No 'lambda' matrix found in lavaan::lavInspect(fit, '%s').",
      lam_mtr
    ))
  }
  # determine which factor to use
  if (is.null(tgt_fct)) {
    if (ncol(lam_mat) > 1) {
      stop(
        "The CFA solution has multiple factors. Please specify the `target_factor` argument to indicate which factor to use.",
        " Detected: ",
        paste0(colnames(lam_mat), collapse = ", "),
        ".",
        call. = FALSE
      )
    } else {
      tgt_fct <- colnames(lam_mat)[1]
    }
  }
  # Extract loadings for the chosen factor
  lam_vec <- lam_mat[, tgt_fct]
  names(lam_vec) <- rownames(lam_mat)
  # Sort by absolute value
  lam_srt <- order(abs(lam_vec), decreasing = TRUE)
  nms_srt <- names(lam_vec)[lam_srt]
  # Propose drop candidates, safe anchor items
  cand <- setdiff(nms_srt, anc)
  mf <- match.fun(dir)
  nms_drp <- mf(cand, n_drp)
  list(
    names = nms_drp,
    subset = dta[, setdiff(colnames(dta), nms_drp), drop = FALSE]
  )
}

#' @rdname dropit-helpers
#' @keywords internal
oneshotdrop_alpha <- function(
  # core
  dta,
  anc,
  n_drp,
  dir,
  # alpha-specific
  alp_mtr,
  alp_args
) {
  itm_nms <- colnames(dta)
  # combine defaults with user-supplied args
  arg_ls <- c(list(x = dta), alp_args)
  # call psych::alpha with constructed args list
  invisible(utils::capture.output(
    alp <- do.call(psych_alpha_internal, arg_ls)
  ))
  # fix label issue with psych::alpha
  key_loc <- isTRUE(arg_ls[["check.keys"]]) 
  if(key_loc){
    warning(paste0(alp[["keys"]][[1]], collapse = ", "), call. = FALSE)
  }
  # extract the relevant metric
  alp_drp <- alp[["alpha.drop"]]
  alp_srt <- order(alp_drp[, alp_mtr], decreasing = TRUE)
  nms_raw <- rownames(alp_drp)[alp_srt]
  # Remove psych's trailing negative sign feature
  nms_cln <- sub("-$", "", nms_raw)
  # Propose drop candidates, safe anchor items
  cand <- setdiff(nms_cln, anc)
  mf <- match.fun(dir) 
  nms_drp <- mf(cand, n_drp)
  list(
    names = nms_drp,
    subset = dta[, setdiff(colnames(dta), nms_drp), drop = FALSE]
  )
}

#' Print Method for Dropit Objects
#' @param x An object of class \code{dropit}.
#' @param ... Further arguments passed to or from other methods.
#' @export
print.dropit <- function(x, ...) {
  colormsg("Dropped Items:", color_code = "38;5;67", bold = TRUE, newline = TRUE)
  print(x$names)
  cat("\n")
  colormsg("Subset(s):", color_code = "38;5;67", bold = TRUE, newline = TRUE)
  if (is.data.frame(x$subset)) {
    utils::str(x$subset)
  } else if (is.list(x$subset)) {
    for (facet in names(x$subset)) {
      cat(sprintf("$%s\n", facet))
      utils::str(x$subset[[facet]])
      cat("\n")
    }
  }
  if (!is.null(x$log)) {
    n_warn <- length(x$log$warnings)
    n_msg <- length(x$log$messages)
    
    if (n_warn > 0 || n_msg > 0) {
      cat(strrep("-", 12), "\n")
      colormsg("Run", color_code = "38;5;67", bold = TRUE, newline = FALSE)
      cat(sprintf(" ended with %d ", n_warn))
      colormsg("warning(s)", color_code = "38;5;67", bold = TRUE, newline = FALSE)
      cat(sprintf(" and %d ", n_msg))
      colormsg("message(s)", color_code = "38;5;67", bold = TRUE, newline = FALSE)
      cat(" logged. Access via `$log`\n")
    }
  }
  invisible(x)
}

#' Print method for dropit_log objects
#' @param x An object of class \code{dropit_log}.
#' @param ... Further arguments passed to or from other methods.
#' @export
print.dropit_log <- function(x, ...) {
  has_warn <- length(x$warnings) > 0
  has_msg <- length(x$messages) > 0
  # If the user explicitly asks for the log but it's empty
  if (!has_warn && !has_msg) {
    cat("Log is empty (0 warnings, 0 messages).\n")
    return(invisible(x))
  }
  # Print warnings if they exist
  if (has_warn) {
    cat(strrep("-", 12), "\n")
    colormsg("Warning(s):", color_code = "38;5;67", bold = TRUE, newline = TRUE)
    cat(paste("  *", x$warnings), sep = "\n")
  }
  # Print messages if they exist
  if (has_msg) {
    colormsg("Message(s):", color_code = "38;5;67", bold = TRUE, newline = TRUE)
    cat(paste("  *", x$messages), sep = "\n")
  }
  cat(strrep("-", 12), "\n")
  invisible(x)
}

# Internal Wrappers for lavaan Calls to Simplify Testing

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