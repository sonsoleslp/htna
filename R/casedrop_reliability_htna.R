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
#' @inheritParams Nestimate::casedrop_reliability
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
#' data(human_long, ai_long, package = "Nestimate")
#' net <- build_htna(list(Human = human_long, AI = ai_long))
#' casedrop_reliability_htna(net, iter = 50, seed = 1)
#' }
#' @export
casedrop_reliability_htna <- Nestimate::casedrop_reliability
