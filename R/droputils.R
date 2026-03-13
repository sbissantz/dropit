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
#' @param n_drp Integer scalar giving the number of items to remove.
#' @param dir Character string, either `"tail"` or `"head"`, passed to
#'   [utils::tail()] / [utils::head()] to select the weakest or strongest
#'   items.
#' @param crt Character string, `"alpha"` or `"lambda"`,
#'   indicating the ranking criterion.
#' @param apr Character string, `"oneshot"` or `"greedy"`,
#'   selecting the removal strategy.
#' @param out Character string, one of `"names"`, `"subset"`, or `"both"`,
#'   determining the type of object returned.
#'
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
#'   [lavaan::inspect()] selecting which standardized solution matrix
#'   to extract (e.g., `"std"`).
#' @param cfa_args Named list of additional arguments passed to
#'   [lavaan::cfa()].
#'
#' @details
#' * Alpha methods call [psych::alpha()] with `check.keys = TRUE`.
#' * Lambda methods call [lavaan::cfa()] (defaulting to `std.lv = TRUE`
#'   unless overridden) and extract the `"lambda"` matrix from
#'   [lavaan::inspect()].
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
  n_drp,
  dir,
  # method selection
  crt,
  apr,
  # output type
  out,
  # alpha-specific
  alp_mtr,
  alp_args,
  # lambda-specific
  mmt_mdl,
  tgt_fct,
  lam_mtr,
  cfa_args
) {
  # TODO Consistent ordering of args across all functions
  switch(
    crt,
    "alpha" = switch(
      apr,
      "oneshot" = oneshotdrop_alpha(
        dta,
        n_drp,
        dir,
        out,
        alp_mtr,
        alp_args
      ),
      "greedy" = greedydrop_alpha(
        dta,
        n_drp,
        dir,
        out,
        alp_mtr,
        alp_args
      ),
      # Should never reached if input validation works properly 
      stop("Debug: Invalid criterion specified. Use 'oneshot' or 'greedy'.")
    ),
    "lambda" = switch(
      apr,
      "oneshot" = oneshotdrop_lambda(
        dta,
        n_drp,
        dir,
        out,
        mmt_mdl,
        tgt_fct = tgt_fct,
        lam_mtr = lam_mtr,
        cfa_args = cfa_args
      ),
      "greedy" = greedydrop_lambda(
        dta, 
        n_drp,
        dir, 
        out,
        mmt_mdl,
        tgt_fct,
        lam_mtr, 
        cfa_args
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
  n_drp,
  dir,
  # output type
  out,
  # lambda-specific
  mmt_mdl, 
  tgt_fct,
  lam_mtr,
  cfa_args
) {
  itm_drp <- character(n_drp)
  itm_nms <- colnames(dta)
  for (i in seq_len(n_drp)) {
    updatedta <- dta[, setdiff(itm_nms, itm_drp), drop = FALSE]
    itm_drp[i] <- oneshotdrop_lambda(
      dta = updatedta,
      n_drp = 1,
      dir,
      out = "names",
      mmt_mdl,
      tgt_fct,
      lam_mtr,
      cfa_args
    )
  }
  switch(
    out,
    "names" = itm_drp,
    "subset" = dta[, setdiff(itm_nms, itm_drp), drop = FALSE],
    "both" = list(
      "names" = itm_drp,
      "subset" = dta[, setdiff(itm_nms, itm_drp), drop = FALSE]
    ),
    # Should never reached if input validation works properly 
    stop(
      "Debug: Invalid output type specified. Use 'names', 'subset', or 'both'."
    )
  )
}

#' @rdname dropit-helpers
#' @keywords internal
greedydrop_alpha <- function(
  # core
  dta,
  n_drp,
  dir,
  # output type
  out,
  # alpha-specific 
  alp_mtr,
  alp_args 
) {
  itm_drp <- character(n_drp)
  itm_nms <- colnames(dta)
  for (i in seq_len(n_drp)) {
    # positions are relative so need name-based indexing methods
    updatedta <- dta[, setdiff(itm_nms, itm_drp), drop = FALSE]
    # drop the next item greedily
    itm_drp[i] <- oneshotdrop_alpha(
      dta = updatedta,
      n_drp = 1,
      dir,
      out = "names",
      alp_mtr,
      alp_args
    )
  }
  switch(
    out,
    "names" = itm_drp,
    "subset" = dta[, setdiff(itm_nms, itm_drp), drop = FALSE],
    "both" = list(
      "names" = itm_drp,
      "subset" = dta[, setdiff(itm_nms, itm_drp), drop = FALSE]
    ),
    # Should never reached if input validation works properly 
    stop(
      "Debug: Invalid output type specified. Use 'names', 'subset', or 'both'."
    )
  )
}

#' @rdname dropit-helpers 
#' @keywords internal
oneshotdrop_lambda <- function(
  # core
  dta,
  n_drp,
  dir,
  # output type 
  out,
  # lambda-specific
  mmt_mdl, 
  tgt_fct,
  lam_mtr,
  cfa_args
) {
  itm_nms <- colnames(dta)
  if (is.null(mmt_mdl)) {
    mmt_mdl <- paste0("F =~ ", paste0(itm_nms, collapse = " + "))
    message(
      "No measurement model was specified. Assuming a single-factor model."
    )
  } else {
   # Should never be reached if the input validation works properly 
   stop("Debug: Custom measurement models are not yet supported.") 
  }
  message("Model history:")
  message(mmt_mdl)
  arg_ls <- c(
    list(model = mmt_mdl, data = dta),
    cfa_args
  )
  fit <- do.call(lavaan_cfa_internal, arg_ls)
  fit_info <- lavaan_inspect_internal(fit, lam_mtr)
  lam_mat <- fit_info[["lambda"]]
  if (is.null(lam_mat) || !is.matrix(lam_mat)) {
    stop(sprintf(
      "No 'lambda' matrix found in lavaan::inspect(fit, '%s').",
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
  mf <- match.fun(dir)
  pos_drp <- mf(lam_srt, n_drp)
  # Can we extract the names directly in the previous step?
  nms_drp <- itm_nms[pos_drp]
  switch(
    out,
    "names" = nms_drp,
    "subset" = dta[, setdiff(colnames(dta), nms_drp), drop = FALSE],
    "both" = list(
      "names" = nms_drp,
      "subset" = dta[, setdiff(colnames(dta), nms_drp), drop = FALSE]
    ),
  #switch(
    #out,
    #"names" = itm_nms[pos_drp],
    #"subset" = dta[, -pos_drp, drop = FALSE],
    #"both" = list(
      #"names" = itm_nms[pos_drp],
      #"subset" = dta[, -pos_drp, drop = FALSE]
    #),
    # Should never reached if the input validation works properly 
    stop(
      "Debug: Invalid output type specified. Use 'names', 'subset', or 'both'."
    )
  )
}

# TODO: Missingness treatment in lavaan::cfa()?
# Add '...' For further arguments to cfa() or do.call() with list()?

#' @rdname dropit-helpers
#' @keywords internal
oneshotdrop_alpha <- function(
  # core
  dta,
  n_drp,
  dir,
  # output type 
  out,
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
  mf <- match.fun(dir) # find head() or tail()
  pos_drp <- mf(alp_srt, n_drp)
  nms_drp <- itm_nms[pos_drp]
  switch(
    out,
    "names" = nms_drp,
    "subset" = dta[, setdiff(colnames(dta), nms_drp), drop = FALSE],
    "both" = list(
      "names" = nms_drp,
      "subset" = dta[, setdiff(colnames(dta), nms_drp), drop = FALSE]
    ),
  # Removed: Issue - dropping 0 items does not work properly. 
  #switch(
    #out,
    #"names" = nms_drp,
    #"subset" = dta[, setdiff(colnames(dta), names_drp), drop = FALSE],
    #"both" = list(
      #"names" = nms_drp,
      #"subset" = dta[, setdiff(colnames(dta), names_drp), drop = FALSE],
    #),
    # Should never be reached if the input validation works properly
    stop(
      "Debug: Invalid output type specified. Use 'names', 'subset', or 'both'."
    )
  )
}

# internal wrappers for lavaan calls (makes testing easier)

#' @keywords internal
#' @noRd
lavaan_cfa_internal <- function(...) {
  lavaan::cfa(...)
}

#' @keywords internal
#' @noRd
lavaan_inspect_internal <- function(x, what) {
  lavaan::inspect(x, what)
}

#' @keywords internal
#' @noRd
psych_alpha_internal <- function(...) {
  psych::alpha(...)
}