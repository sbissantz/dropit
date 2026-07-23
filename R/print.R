#' Print a Colored Message to the Console
#'
#' Prints a message using ANSI color codes. Supports the standard 16-color and
#' extended 256-color modes, plus bold styling. Some consoles (e.g. RGui on
#' Windows) do not support ANSI colors.
#'
#' @param txt Character scalar. The message text to print.
#' @param color_code Integer or character. ANSI color code
#'   (e.g., `31` = red, `32` = green, `"38;5;244"` = gray256). Defaults to 32.
#' @param bold Logical; if `TRUE`, applies bold formatting. Default `FALSE`.
#' @param newline Logical; if `TRUE`, prints a newline after the message.
#'   Default `FALSE`.
#'
#' @return Invisibly `NULL`; called for the side effect of printing.
#' @keywords internal
colormsg <- function(txt, color_code = 32, bold = FALSE, newline = FALSE) {
  style_code <- if (bold) paste0("1;", color_code) else as.character(color_code)
  msg <- paste0("\033[", style_code, "m", txt, "\033[0m")
  if (newline) cat(msg, "\n") else cat(msg)
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
