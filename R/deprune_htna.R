#' Restore All Edges of a Pruned Transition Network
#'
#' htna-named wrapper for [Nestimate::net_deprune()]. Reverses a
#' [prune_htna()] step, restoring the original (unpruned) edge set.
#'
#' The actor partition (`$node_groups`, `$actor_levels`, `htna` class)
#' is preserved, so the restored network stays htna-aware.
#'
#' Suffixed `_htna` to avoid clashing with the tna verb `deprune()` and
#' to sit alongside the rest of the htna API.
#'
#' @param x A pruned htna network (or other object accepted by
#'   [Nestimate::net_deprune()]); S3 dispatch on `x` is preserved.
#' @param ... Additional arguments passed to [Nestimate::net_deprune()].
#'   See that function for details.
#'
#' @return The depruned htna network. See [Nestimate::net_deprune()].
#'
#' @seealso [prune_htna()], [reprune_htna()], [pruning_details_htna()].
#' @examples
#' \donttest{
#' data(human_ai)
#' net <- build_htna(human_ai, actor_type = "actor_type")
#' pruned <- prune_htna(net, method = "threshold", threshold = 0.05)
#' deprune_htna(pruned)
#' }
#' @export
deprune_htna <- function(x, ...) {
  Nestimate::net_deprune(x, ...)
}
