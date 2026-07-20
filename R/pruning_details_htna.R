#' Tidy Report of a Network's Pruning
#'
#' htna-named wrapper for [Nestimate::net_pruning_details()]. Returns a
#' tidy data frame describing which edges were kept or removed by a
#' [prune_htna()] step and why.
#'
#' Suffixed `_htna` to avoid clashing with the tna verb
#' `pruning_details()` and to sit alongside the rest of the htna API.
#'
#' @param x A pruned htna network (or other object accepted by
#'   [Nestimate::net_pruning_details()]); S3 dispatch on `x` is
#'   preserved.
#' @param ... Additional arguments passed to
#'   [Nestimate::net_pruning_details()]. See that function for details.
#'
#' @return A data frame, one row per edge. See
#'   [Nestimate::net_pruning_details()] for details.
#'
#' @seealso [prune_htna()], [deprune_htna()], [reprune_htna()].
#' @examples
#' \donttest{
#' data(human_ai)
#' net <- build_htna(human_ai, actor_type = "actor_type")
#' pruned <- prune_htna(net, method = "threshold", threshold = 0.05)
#' pruning_details_htna(pruned)
#' }
#' @export
pruning_details_htna <- function(x, ...) {
  Nestimate::net_pruning_details(x, ...)
}
