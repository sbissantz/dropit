#' Internal Utility Functions
#'
#' A collection of internal helper functions used across the package for
#' message formatting, console output, and argument checking.
#'
#' These utilities are **not exported** and are primarily intended for
#' internal use in developer-facing code such as [dropit()].
#'
#' @section Included Helpers:
#' \describe{
#'   \item{\code{colormsg()}}{Prints colored and optionally bold messages to
#'   the console using ANSI escape codes.}
#'   \item{\code{trim_newlines()}}{Removes leading and trailing newline
#'   characters from a string.}
#'   \item{\code{check_ignored()}}{Emits an informational message when
#'   arguments that are not applicable to the current method have been
#'   provided by the user.}
#' }
#'
#' @details
#' Some consoles (e.g., RGui on Windows) may not support ANSI colors.
#' For full color compatibility, use RStudio, VS Code, or a Unix terminal.
#'
#' @keywords internal
#' @name miscutils
NULL

#' Print Colored Message to Console
#'
#' Prints a message to the console using ANSI color codes. Supports both
#' standard 16-color and extended 256-color modes, as well as bold styling.
#'
#' @param txt Character scalar. The message text to print.
#' @param color_code Integer or character. ANSI color code
#'   (e.g., `31` = red, `32` = green, `33` = yellow, `"38;5;244"` = gray256).
#'   Defaults to 32 (green).
#' @param bold Logical; if `TRUE`, applies bold formatting. Default is `FALSE`.
#' @param newline Logical; if `TRUE`, prints a newline after the message.
#'   Default is `FALSE`.
#'
#' @return Invisibly returns `NULL`. Called for its side effect of printing
#'   colored text to the console.
#'
#' @keywords internal
colormsg <- function(txt, color_code = 32, bold = FALSE, newline = FALSE) {
  style_code <- if (bold) paste0("1;", color_code) else as.character(color_code)
  msg <- paste0("\033[", style_code, "m", txt, "\033[0m")
  if (newline) cat(msg, "\n") else cat(msg)
}

#' Trim Leading and Trailing Newlines
#'
#' Removes any leading and trailing newline (`\\n`) characters from a
#' character string. Useful for cleaning up collected messages or
#' warnings before printing.
#'
#' @param x Character vector or scalar to process.
#'
#' @return A character vector with newlines removed from the start and end.
#'
#' @keywords internal
trim_newlines <- function(x) {
  gsub("(^\\n+|\\n+$)", "", x)
}

#' Warn About Ignored Arguments
#'
#' Internal helper used to emit a message when user-specified arguments
#' are not applicable to the current method (e.g., alpha vs. lambda mode).
#'
#' @param usr_sup Character vector of argument names that were
#'   explicitly supplied by the user.
#' @param ign_nms Character vector of argument names that are not
#'   applicable in the current context.
#'
#' @details
#' This function prints an informative message rather than a warning,
#' indicating that some arguments have been ignored intentionally.
#'
#' @return Invisibly returns `NULL`. Called for its side effect of printing
#'   a message.
#' 
#' @keywords internal
check_ignored <- function(usr_sup, ign_nms) {
  bad <- intersect(usr_sup, ign_nms)
  if (length(bad)) {
    message(sprintf(
      "Argument(s) %s not applicable and ignored.",
      paste0(shQuote(bad), collapse = ", ")
    ))
  }
}