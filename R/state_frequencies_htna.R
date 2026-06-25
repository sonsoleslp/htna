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
#' @inheritParams Nestimate::state_frequencies
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
state_frequencies_htna <- Nestimate::state_frequencies
