#' Re-apply a recorded pruning rule
#'
#' @description
#' `reprune_htna()` re-applies the recorded pruning rule of a network and
#' returns the pruned network with its actor partition. A network that has
#' been pruned and then restored with [deprune_htna()] still carries the
#' pruning rule; re-applying it removes the same weak edges again without
#' restating the method and threshold.
#'
#' `reprune_htna()` runs Nestimate's `net_reprune()` and returns an htna
#' network with the edges that fail the recorded rule set to zero.
#'
#' @param x An htna network that was pruned with [prune_htna()] and
#'   restored with [deprune_htna()].
#' @param ... Unused.
#'
#' @return An htna network with the edges that fail the recorded rule set
#'   to zero.
#'
#' @seealso [prune_htna()], [deprune_htna()], [pruning_details_htna()].
#' @examples
#' data(human_ai)
#' net <- build_htna(human_ai, actor_type = "actor_type")
#' pruned <- prune_htna(net, threshold = 0.05)
#' reprune_htna(deprune_htna(pruned))
#' @export
reprune_htna <- function(x, ...) {
  Nestimate::net_reprune(x, ...)
}
