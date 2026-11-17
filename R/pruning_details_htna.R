#' List the edges removed by pruning
#'
#' @description
#' `pruning_details_htna()` lists the edges that [prune_htna()] removed:
#' which transitions were set aside and how strong they were. Pruning
#' changes what a network shows, so the removed edges should be reported
#' alongside the pruned network.
#'
#' `pruning_details_htna()` runs Nestimate's `net_pruning_details()` and
#' returns a data frame with one row per removed edge and the weight each
#' had before pruning. The pruning rule and the numbers of removed and
#' retained edges are printed with the table.
#'
#' @param x An htna network pruned with [prune_htna()].
#' @param ... Unused.
#'
#' @return A data frame of class `c("net_pruning_details", "data.frame")`
#'   with one row per removed edge and columns `from`, `to` and `weight`.
#'   The rule is kept in the attributes `method`, `threshold`, `lowest`,
#'   `cut_off`, `num_removed` and `num_retained`, and is shown by
#'   `print()`.
#'
#' @seealso [prune_htna()], [deprune_htna()], [reprune_htna()].
#' @examples
#' data(human_ai)
#' net <- build_htna(human_ai, actor_type = "actor_type")
#' pruned <- prune_htna(net, threshold = 0.05)
#' pruning_details_htna(pruned)
#' @export
pruning_details_htna <- function(x, ...) {
  Nestimate::net_pruning_details(x, ...)
}
