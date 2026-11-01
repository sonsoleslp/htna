#' Tidy Per-Actor Frequency Table for an htna Network
#'
#' Returns the within-network state frequency table, partitioned by
#' actor. One row per `(actor, state)` pair, with `count` and
#' `proportion` columns. The data side of [plot_frequencies_htna()].
#'
#' Internally a thin wrapper around the htna S3 method shipped by
#' Nestimate (`state_distribution.htna`); exposed under the
#' `frequencies_htna()` name as the canonical "give me the frequency
#' table for this network" entry point.
#'
#' Suffixed `_htna` to avoid clashing with `Nestimate::frequencies()`
#' (which takes raw long-format data and a column-name spec) when
#' both packages are loaded. If you have raw data rather than an htna
#' network, call `Nestimate::frequencies()` directly.
#'
#' @param x An htna network from [build_htna()].
#'
#' @return A data frame with one row per `(actor, state)` pair and
#'   columns `group` (the actor type), `state`, `count` and `proportion`
#'   (the state's share of its own actor's events, so proportions sum to
#'   1 within each actor).
#'
#' @seealso [plot_frequencies_htna()] for the rendered version,
#'   [state_distribution_htna()] (same function under the upstream
#'   name), [mosaic_plot_htna()].
#' @examples
#' data(human_ai)
#' net <- build_htna(human_ai, actor_type = "actor_type")
#' frequencies_htna(net)
#' @export
frequencies_htna <- function(x) {
  if (!inherits(x, "htna")) {
    stop("`frequencies_htna()` needs an htna network from build_htna().",
         call. = FALSE)
  }
  Nestimate::state_distribution(x)
}
