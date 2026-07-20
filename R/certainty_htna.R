#' Closed-form Edge Certainty of a Network
#'
#' htna-named wrapper for [Nestimate::certainty()]. The closed-form
#' counterpart of [bootstrap_htna()]: rather than resampling, it derives
#' analytic confidence intervals and an inference verdict for each edge
#' weight, which is fast enough to run where a full bootstrap would be
#' costly.
#'
#' Works on htna networks and grouped htna networks directly.
#'
#' Suffixed `_htna` to sit alongside [bootstrap_htna()] and
#' [reliability_htna()] in the htna confirmatory-analysis family.
#'
#' @param x An htna network from [build_htna()] (or other object
#'   accepted by [Nestimate::certainty()]).
#' @param ... Additional arguments passed to [Nestimate::certainty()],
#'   such as `prior`, `ci_level`, `inference`, `consistency_range`, and
#'   `edge_threshold`. See that function for the full, current argument
#'   list.
#'
#' @return An object of class `net_certainty` (also inheriting
#'   `net_bootstrap`). See [Nestimate::certainty()] for the full
#'   component list and the corresponding `plot()` method.
#'
#' @seealso [bootstrap_htna()] for the resampling analogue,
#'   [reliability_htna()], [casedrop_reliability_htna()].
#' @examples
#' \donttest{
#' data(human_ai)
#' net <- build_htna(human_ai, actor_type = "actor_type")
#' certainty_htna(net)
#' }
#' @export
certainty_htna <- function(x, ...) {
  Nestimate::certainty(x, ...)
}
