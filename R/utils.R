# ============================================================================
# vastRai - Shared utility functions
# ============================================================================

#' Null-coalescing operator
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x


#' Escape double quotes in strings for code generation
#' @noRd
escape_quotes <- function(x) {

  gsub('"', '\\\\"', x)
}


#' Format a strength value for R code generation
#' @noRd
format_strength <- function(strength) {
  if (is.numeric(strength)) {
    as.character(strength)
  } else {
    sprintf('"%s"', escape_quotes(as.character(strength)))
  }
}


#' Format a value (numeric or character) for R code generation
#' @noRd
format_value <- function(value) {
  if (is.numeric(value)) {
    as.character(value)
  } else {
    sprintf('"%s"', escape_quotes(as.character(value)))
  }
}


#' Sanitize a string for use as a file/directory name
#'
#' Replaces spaces with underscores, removes special characters,
#' and truncates to a maximum length.
#'
#' @param x Character string to sanitize.
#' @param max_length Maximum length of the result.
#' @return A sanitized string safe for file system use.
#' @noRd
sanitize_filename <- function(x, max_length = 40) {
  x <- gsub("\\s+", "_", x)             # spaces to underscores
  x <- gsub("[^A-Za-z0-9_.-]", "", x)   # remove special chars
  x <- gsub("_+", "_", x)               # collapse multiple underscores
  x <- gsub("^_|_$", "", x)             # trim leading/trailing underscores
  if (nchar(x) > max_length) {
    x <- substr(x, 1, max_length)
    x <- gsub("_$", "", x)              # don't end on underscore after truncation
  }
  if (nchar(x) == 0) x <- "vast_report"
  x
}
