#' Restore the edges removed by pruning
#'
#' @description
#' `deprune_htna()` restores the edges that [prune_htna()] removed: it
#' returns the network with every removed edge put back at its original
#' weight. Pruning sets weak edges aside rather than deleting them, so
#' restoring them gives back the full network, for example to compare the
#' pruned and the unpruned structure or to apply a different rule.
#'
#' `deprune_htna()` runs Nestimate's `net_deprune()` with the actor
#' partition kept. The rule stays recorded, so [reprune_htna()] can apply it
#' again.
#'
#' @param x An htna network pruned with [prune_htna()].
#' @param ... Unused.
#'
#' @return An htna network with the original weight matrix.
#'
#' @seealso [prune_htna()], [reprune_htna()], [pruning_details_htna()].
#' @examples
#' data(human_ai)
#' net <- build_htna(human_ai, actor_type = "actor_type")
#' pruned <- prune_htna(net, threshold = 0.05)
#' deprune_htna(pruned)
#' @export
deprune_htna <- function(x, ...) {
  Nestimate::net_deprune(x, ...)
}
