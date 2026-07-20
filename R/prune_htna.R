#' Prune Weak Edges from a Transition Network
#'
#' htna-named wrapper for [Nestimate::net_prune()]. Removes edges that
#' fall below a weight/threshold rule, returning a pruned network of the
#' same shape.
#'
#' The actor partition (`$node_groups`, `$actor_levels`, `htna` class)
#' is preserved on the pruned network, so [plot_htna()] and the other
#' htna verbs keep working on the result.
#'
#' Suffixed `_htna` to avoid clashing with the tna verb `prune()` and to
#' sit alongside the rest of the htna API.
#'
#' @param x An htna network from [build_htna()] (or other object
#'   accepted by [Nestimate::net_prune()]); S3 dispatch on `x` is
#'   preserved.
#' @param ... Additional arguments passed to [Nestimate::net_prune()],
#'   such as `method`, `threshold`, `lowest`, `level`, and `boot`. See
#'   that function for the full, current argument list.
#'
#' @return A pruned htna network. See [Nestimate::net_prune()] for
#'   details.
#'
#' @seealso [deprune_htna()], [reprune_htna()], [pruning_details_htna()],
#'   [build_htna()].
#' @examples
#' \donttest{
#' data(human_ai)
#' net <- build_htna(human_ai, actor_type = "actor_type")
#' prune_htna(net, method = "threshold", threshold = 0.05)
#' }
#' @export
prune_htna <- function(x, ...) {
  Nestimate::net_prune(x, ...)
}
