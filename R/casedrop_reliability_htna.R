#' Edge-Weight Case-Dropping Stability
#'
#' htna-named alias of [Nestimate::casedrop_reliability()]. Computes
#' the CS-coefficient for the edge-weight vector of a network: the
#' maximum proportion of cases (rows of `x$data`) that can be dropped
#' while the flattened edge-weight vector of the re-estimated network
#' still correlates with the original above `threshold` in at least
#' `certainty` of iterations.
#'
#' Works on htna networks and grouped htna networks directly.
#'
#' Suffixed `_htna` to avoid clashing with
#' `Nestimate::casedrop_reliability()` when both packages are loaded.
#'
#' @param x An htna network or grouped htna network. See
#'   [Nestimate::casedrop_reliability()].
#' @param ... Additional arguments passed to
#'   [Nestimate::casedrop_reliability()], such as `iter`, `drop_prop`,
#'   `threshold`, `certainty`, `method`, `include_diag`, and `seed`. See
#'   that function for the full, current argument list.
#'
#' @return An object of class `net_casedrop_reliability` (single
#'   network) or `net_casedrop_reliability_group` (grouped htna). See
#'   [Nestimate::casedrop_reliability()] for the full component list
#'   and the corresponding `plot()` method.
#'
#' @seealso [reliability_htna()], [centrality_stability_htna()],
#'   [bootstrap_htna()].
#' @examples
#' \donttest{
#' data(human_ai)
#' net <- build_htna(human_ai, actor_type = "actor_type")
#' casedrop_reliability_htna(net, iter = 20, seed = 1)
#' }
#' @export
casedrop_reliability_htna <- function(x, ...) {
  Nestimate::casedrop_reliability(x, ...)
}
