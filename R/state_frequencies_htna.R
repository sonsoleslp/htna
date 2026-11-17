#' Count the codes of a network over all actors
#'
#' @description
#' `state_frequencies_htna()` counts the codes of a network over all actors:
#' every code in the sessions of an htna network, or in raw sequence data,
#' in decreasing order of frequency. The pooled table shows which codes
#' dominate the interaction as a whole, whichever actor produces them. Each
#' code's proportion is its share of all events, so the shares of the
#' different actors can be compared directly.
#'
#' `state_frequencies_htna()` runs Nestimate's `state_frequencies()` and
#' returns a data frame with one row per code.
#'
#' The table has no actor column; [frequencies_htna()] reports each code
#' under its actor, with shares within the actor.
#'
#' @param data An htna network from [build_htna()], or sequence data (a
#'   wide data frame or a list of character vectors).
#' @param ... Unused.
#'
#' @return A data frame with one row per code, sorted by decreasing count,
#'   and columns `state`, `count` and `proportion` (the share of all
#'   events, rounded to four decimals).
#'
#' @seealso [frequencies_htna()], [plot_frequencies_htna()].
#' @examples
#' data(human_ai)
#' net <- build_htna(human_ai, actor_type = "actor_type")
#' state_frequencies_htna(net)
#' @export
state_frequencies_htna <- function(data, ...) {
  Nestimate::state_frequencies(data, ...)
}
