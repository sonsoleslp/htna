#' Remove weak edges from a heterogeneous transition network
#'
#' @description
#' `prune_htna()` removes weak edges from a heterogeneous transition network
#' so that the plot and the descriptive measures focus on the transitions
#' that carry the process. A relative transition network estimated from
#' many sessions is usually dense: almost every code is followed by almost
#' every other code at least once, and most of these transitions are rare.
#' Because the weights are kept in the network and only set aside, pruning
#' can be undone or re-applied.
#'
#' `prune_htna()` sets to zero the edges that fail a rule: weights below a
#' fixed `threshold` (`method = "threshold"`, the default), the `lowest`
#' share of the weights (`method = "lowest"`), or edges that are not
#' supported by a bootstrap (`method = "bootstrap"`, with `boot` from
#' [bootstrap_htna()]). It runs Nestimate's `net_prune()` with the actor
#' partition kept, so [plot_htna()] and the other functions work on the
#' pruned network unchanged. It returns an htna network with the removed
#' edges set to zero and the rule recorded.
#'
#' [deprune_htna()] restores the removed edges, [reprune_htna()] removes
#' them again, and [pruning_details_htna()] lists them.
#'
#' @param x An htna network from [build_htna()].
#' @param ... Pruning rule: `method` (`"threshold"`, default, `"lowest"`
#'   or `"bootstrap"`), `threshold` (default 0.1), `lowest` (default 0.05),
#'   `level` (default 0.5) and `boot` (a bootstrap result for
#'   `method = "bootstrap"`).
#'
#' @return An htna network of the same class and structure as
#'   [build_htna()] returns, with the removed edges set to zero in its
#'   weight matrix and the rule recorded for [deprune_htna()],
#'   [reprune_htna()] and [pruning_details_htna()].
#'
#' @seealso [deprune_htna()], [reprune_htna()], [pruning_details_htna()],
#'   [bootstrap_htna()].
#' @examples
#' data(human_ai)
#' net <- build_htna(human_ai, actor_type = "actor_type")
#' pruned <- prune_htna(net, threshold = 0.05)
#' summary(pruned)
#' @export
prune_htna <- function(x, ...) {
  Nestimate::net_prune(x, ...)
}
