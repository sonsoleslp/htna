#' Print a table of paths or meta-paths
#'
#' @description
#' `print()` prints the result of [extract_meta_paths()]: a header with the
#' level (concrete paths or meta-paths), the template if one was given, the
#' number of sessions, the lengths and gaps counted, and then the first `n`
#' rows with support, frequency and lift rounded for reading. It returns `x`
#' invisibly. The result is itself a data frame, so all rows and full
#' precision are available with [utils::head()], [subset()] or
#' `as.data.frame()`.
#'
#' @param x A result of [extract_meta_paths()].
#' @param n Number of rows to print (default 10).
#' @param ... Unused.
#'
#' @return `x`, invisibly.
#'
#' @seealso [extract_meta_paths()].
#' @examples
#' data(human_ai)
#' net <- build_htna(human_ai, actor_type = "actor_type")
#' paths <- extract_meta_paths(net, length = 3)
#' print(paths, n = 5)
#' @export
print.htna_paths <- function(x, n = 10L, ...) {
  level  <- attr(x, "level") %||% "paths"
  schema <- attr(x, "schema")
  header <- switch(level,
    state = if (is.null(schema)) "Patterns (state-level)"
            else sprintf("State-level instances of schema '%s'", schema),
    type  = if (is.null(schema)) "Meta-paths (type-level)"
            else sprintf("Type-level meta-paths matching '%s'", schema),
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
  if (!is.null(shown$meta_schema)) {
    body <- cbind(body[, "schema", drop = FALSE],
                  meta_schema = shown$meta_schema,
                  body[, setdiff(names(body), "schema"), drop = FALSE])
  }
  print(body, row.names = FALSE)
  if (nrow(x) > n) cat("... (", nrow(x) - n, " more)\n", sep = "")
  invisible(x)
}
