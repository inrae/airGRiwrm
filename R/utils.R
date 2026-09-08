#' Suppress warnings matching a specific regex pattern
#'
#' This function evaluates an expression while suppressing any warnings
#' that match a given regular expression pattern.
#' @param expr An expression to evaluate.
#' @param pattern A regular expression pattern to match warning messages.
#' @return The result of the evaluated expression with specified warnings suppressed.
#' @noRd
#'
suppressWarningsRegex <- function(expr, pattern) {
  withCallingHandlers(
    expr,
    warning = function(w) {
      if (grepl(pattern, conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )
}
