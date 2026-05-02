#' Print Method for htna Path/Pattern Objects
#'
#' Shared print method for objects returned by [extract_meta_paths()]
#' (class `htna_meta_paths`). Dispatches on the `htna_paths` parent class.
#' @keywords internal
#'
#' @param x An `htna_paths` object.
#' @param n Maximum number of rows to print. Default `10`.
#' @param ... Unused.
#'
#' @return The input object, invisibly.
#' @export
print.htna_paths <- function(x, n = 10L, ...) {
  level <- attr(x, "level") %||% "paths"
  header <- switch(level,
    meta  = "Meta-paths (type-level)",
    state = "Patterns (state-level)",
    "Paths"
  )
  n_seq <- attr(x, "n_sequences") %||% NA_integer_
  cat(header, " over ", format(n_seq), " sequences\n", sep = "")
  if (!nrow(x)) {
    cat("(no rows met the filters)\n")
    return(invisible(x))
  }
  uniq_len <- sort(unique(x$length))
  uniq_gap <- sort(unique(x$gap))
  cat("Rows: ", nrow(x),
      " | Lengths: ", paste(uniq_len, collapse = ", "),
      " | Gaps: ",    paste(uniq_gap, collapse = ", "),
      "\n", sep = "")
  shown <- utils::head(x, n)
  body <- data.frame(
    schema    = shown$schema,
    length    = shown$length,
    gap       = shown$gap,
    count     = shown$count,
    n_seq     = shown$n_sequences,
    support   = sprintf("%.3f", shown$support),
    frequency = sprintf("%.3f", shown$frequency),
    lift      = sprintf("%.2f", shown$lift),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  print(body, row.names = FALSE)
  if (nrow(x) > n) cat("... (", nrow(x) - n, " more)\n", sep = "")
  invisible(x)
}
