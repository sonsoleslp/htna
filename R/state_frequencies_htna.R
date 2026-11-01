#' Tidy State Frequency Table
#'
#' htna-named wrapper for [Nestimate::state_frequencies()]. Returns a
#' tidy data frame with `state`, `count`, and `proportion` columns
#' summarising the within-network state vocabulary, pooled across actors.
#'
#' Companion to [plot_frequencies_htna()]. Unlike [frequencies_htna()],
#' the table carries no actor column; use [frequencies_htna()] when each
#' state should be reported under its actor type.
#'
#' Suffixed `_htna` to avoid clashing with
#' `Nestimate::state_frequencies()` when both packages are loaded.
#'
#' @param data An htna network from [build_htna()] (its session sequences
#'   are counted), or raw sequence data. See
#'   [Nestimate::state_frequencies()].
#' @param ... Additional arguments passed to
#'   [Nestimate::state_frequencies()]. See that function for details.
#'
#' @return A data frame with one row per state, sorted by decreasing
#'   count, and columns `state`, `count` and `proportion`. See
#'   [Nestimate::state_frequencies()].
#'
#' @seealso [plot_frequencies_htna()], [state_distribution_htna()].
#' @examples
#' data(human_ai)
#' net <- build_htna(human_ai, actor_type = "actor_type")
#' state_frequencies_htna(net)
#' @export
state_frequencies_htna <- function(data, ...) {
  Nestimate::state_frequencies(data, ...)
}
