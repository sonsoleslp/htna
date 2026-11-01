#' Tidy Report of a Network's Pruning
#'
#' htna-named wrapper for [Nestimate::net_pruning_details()]. Returns a
#' tidy data frame of the edges removed by a [prune_htna()] step, with the
#' pruning rule that removed them recorded as attributes.
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
#' @return A data frame of class `c("net_pruning_details", "data.frame")`
#'   with one row per removed edge and columns `from`, `to` and `weight`
#'   (the weight the edge had before pruning). The rule is stored in the
#'   attributes `method`, `threshold`, `lowest`, `cut_off`, `num_removed`
#'   and `num_retained`. See [Nestimate::net_pruning_details()].
#'
#' @seealso [prune_htna()], [deprune_htna()], [reprune_htna()].
#' @examples
#' data(human_ai)
#' net <- build_htna(human_ai, actor_type = "actor_type")
#' pruned <- prune_htna(net, method = "threshold", threshold = 0.05)
#' pruning_details_htna(pruned)
#' @export
pruning_details_htna <- function(x, ...) {
  Nestimate::net_pruning_details(x, ...)
}
