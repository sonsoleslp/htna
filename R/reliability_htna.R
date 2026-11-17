#' Estimate the split-half reliability of a heterogeneous transition network
#'
#' @description
#' `reliability_htna()` estimates the split-half reliability of one or more
#' htna networks: how far each network would look the same if it had been
#' estimated from a different half of the sessions. The sessions are split
#' at random into two halves many times, a network is estimated from each
#' half, and the two networks are compared: the mean, median and maximum
#' absolute difference of their edge weights and the correlation of their
#' weights. High correlations and small deviations over the splits indicate
#' that the estimated structure does not depend on which sessions happened
#' to be observed.
#'
#' When several networks are given (for example, built with different
#' methods from the same data), each is assessed with the same splits.
#' `reliability_htna()` runs Nestimate's `network_reliability()`, and the
#' networks re-estimated on the splits keep the actor partition. It returns
#' an `htna_reliability` object.
#'
#' For the stability of the weights when increasing shares of the sessions
#' are dropped use [casedrop_reliability_htna()]; for edge-level intervals
#' use [bootstrap_htna()].
#'
#' @param ... One or more htna networks from [build_htna()].
#' @param iter Number of random splits (default 1000).
#' @param split Proportion of the sessions in the first half (default 0.5).
#' @param scale Scaling of the weights before they are compared: `"none"`
#'   (default), `"minmax"`, `"standardize"` or `"proportion"`.
#' @param seed Random seed (default `NULL`, no seed is set).
#'
#' @return An object of class `c("htna_reliability", "net_reliability")`
#'   with `print()`, `summary()` and `plot()` methods. `summary()` returns
#'   a data frame with one row per network and metric and columns `model`,
#'   `metric` (`mean_dev`, `median_dev`, `cor`, `max_dev`), `mean` and
#'   `sd`.
#'
#' @section Conditions:
#' An error of class `htna_empty_input` when no network is given, and of
#' class `htna_bad_input` when an input is not an htna network.
#'
#' @seealso [casedrop_reliability_htna()], [bootstrap_htna()],
#'   [centrality_stability_htna()].
#'
#' @examples
#' data(human_ai)
#' net <- build_htna(human_ai, actor_type = "actor_type")
#' rel <- reliability_htna(net, iter = 30, seed = 1)
#' rel
#' summary(rel)
#'
#' @export
reliability_htna <- function(..., iter = 1000L, split = 0.5,
                             scale = "none", seed = NULL) {
  nets <- list(...)
  if (length(nets) == 0L) {
    .htna_stop("`reliability_htna()` needs at least one htna network.",
               class = "htna_empty_input")
  }
  ok <- vapply(nets, function(n) inherits(n, "htna"), logical(1L))
  if (!all(ok)) {
    .htna_stop("All inputs to `reliability_htna()` must be htna networks ",
               "built by build_htna().")
  }

  rel <- Nestimate::network_reliability(..., iter = iter, split = split,
                                        scale = scale, seed = seed)

  # Re-inject the htna actor partition onto each model. Nestimate
  # builds models via build_network() which strips the partition;
  # we restore it slot-for-slot from the matching input network.
  if (!is.null(rel$models) && length(rel$models) == length(nets)) {
    rel$models <- Map(function(m, src) {
      if (is.null(m)) return(m)
      if (is.null(m$nodes$groups) && !is.null(src$nodes$groups)) {
        m$nodes$groups <- src$nodes$groups
      }
      if (is.null(m$node_groups) && !is.null(src$node_groups)) {
        m$node_groups <- src$node_groups
      }
      if (is.null(m$actor_levels) && !is.null(src$actor_levels)) {
        m$actor_levels <- src$actor_levels
      }
      if (!inherits(m, "htna")) {
        class(m) <- c("htna", class(m))
      }
      m
    }, rel$models, nets)
  }

  if (!inherits(rel, "htna_reliability")) {
    class(rel) <- c("htna_reliability", class(rel))
  }
  rel
}
