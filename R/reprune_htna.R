#' Re-apply a Previous Pruning Rule
#'
#' htna-named wrapper for [Nestimate::net_reprune()]. Re-applies the
#' pruning rule recorded on a network (e.g. after the weights have been
#' re-estimated), without having to restate the rule.
#'
#' The actor partition (`$node_groups`, `$actor_levels`, `htna` class)
#' is preserved, so the re-pruned network stays htna-aware.
#'
#' Suffixed `_htna` to avoid clashing with the tna verb `reprune()` and
#' to sit alongside the rest of the htna API.
#'
#' @param x An htna network carrying a prior pruning rule (or other
#'   object accepted by [Nestimate::net_reprune()]); S3 dispatch on `x`
#'   is preserved.
#' @param ... Additional arguments passed to [Nestimate::net_reprune()].
#'   See that function for details.
#'
#' @return The re-pruned htna network. See [Nestimate::net_reprune()].
#'
#' @seealso [prune_htna()], [deprune_htna()], [pruning_details_htna()].
#' @examples
#' \donttest{
#' data(human_ai)
#' net <- build_htna(human_ai, actor_type = "actor_type")
#' pruned <- prune_htna(net, method = "threshold", threshold = 0.05)
#' reprune_htna(deprune_htna(pruned))
#' }
#' @export
reprune_htna <- function(x, ...) {
  Nestimate::net_reprune(x, ...)
}
