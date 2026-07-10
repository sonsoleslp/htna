#' Tidy State Frequency Table
#'
#' htna-named alias of [Nestimate::state_frequencies()]. Returns a
#' tidy data frame with `state`, `count`, and `proportion` columns
#' summarising the within-network state vocabulary.
#'
#' Companion to [plot_frequencies_htna()] (which is htna-aware
#' via an explicit S3 method). `state_frequencies_htna()` itself
#' operates on the raw sequence data and is the data side of the same
#' family used by the htna tutorials.
#'
#' Suffixed `_htna` to avoid clashing with
#' `Nestimate::state_frequencies()` when both packages are loaded.
#'
#' @param data Raw sequence data summarised into a state-frequency
#'   table. See [Nestimate::state_frequencies()].
#' @param ... Additional arguments passed to
#'   [Nestimate::state_frequencies()]. See that function for details.
#'
#' @return A data frame. See [Nestimate::state_frequencies()] for
#'   details.
#'
#' @seealso [plot_frequencies_htna()], [state_distribution_htna()].
#' @examples
#' \donttest{
#' data(human_ai)
#' net <- build_htna(human_ai, actor_type = "actor_type")
#' state_frequencies_htna(net$data)
#' }
#' @export
state_frequencies_htna <- function(data, ...) {
  Nestimate::state_frequencies(data, ...)
}
