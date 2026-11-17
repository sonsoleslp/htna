#' Tabulate the edges and nodes of an htna network
#'
#' @description
#' `as.data.frame()` returns the edges or the nodes of a heterogeneous
#' transition network as a tidy data frame in which every row carries the
#' actor type of its codes.
#' The edge table lists every non-zero transition with its weight and the
#' actor types of its source and target, so the within-actor and
#' between-actor transitions (for example Human to AI) can be read, counted
#' or filtered directly. The node table lists every code with its actor type.
#'
#' For a grouped network the tables of all cohorts are stacked with a
#' leading `group` column. Self-transitions (the diagonal of the weight
#' matrix) are kept. For an undirected network (`method = "co_occurrence"`)
#' each pair of codes is listed once.
#'
#' @param x An htna network from [build_htna()] or an `htna_group`.
#' @param row.names,optional Ignored; present for the generic.
#' @param what `"edges"` (default) or `"nodes"`.
#' @param ... Unused.
#'
#' @return A data frame. For `what = "edges"`: one row per non-zero edge,
#'   with columns `from`, `to`, `weight`, `source_actor` and `target_actor`
#'   (factors in the network's actor order), sorted by source then target
#'   in node order. For `what = "nodes"`: one row per node, with columns
#'   `node` and `actor`. Grouped networks add a first column `group`.
#'
#' @seealso [build_htna()], [summary.htna()] for counts per actor pair.
#'
#' @examples
#' data(human_ai)
#' net <- build_htna(human_ai, actor_type = "actor_type")
#' head(as.data.frame(net))
#' as.data.frame(net, what = "nodes")
#' @export
as.data.frame.htna <- function(x, row.names = NULL, optional = FALSE, ...,
                               what = c("edges", "nodes")) {
  what <- match.arg(what)
  partition <- .htna_node_partition(x, "x")
  actor <- factor(unname(partition$lookup), levels = partition$actor_levels)
  if (identical(what, "nodes")) {
    return(data.frame(node = partition$nodes, actor = actor,
                      stringsAsFactors = FALSE))
  }
  W <- x$weights
  keep <- W != 0 & !is.na(W)
  if (isFALSE(x$directed)) keep <- keep & upper.tri(W, diag = TRUE)
  idx <- which(keep, arr.ind = TRUE)
  idx <- idx[order(idx[, 1L], idx[, 2L]), , drop = FALSE]
  data.frame(
    from = partition$nodes[idx[, 1L]],
    to = partition$nodes[idx[, 2L]],
    weight = W[idx],
    source_actor = actor[idx[, 1L]],
    target_actor = actor[idx[, 2L]],
    stringsAsFactors = FALSE
  )
}

#' @rdname as.data.frame.htna
#' @export
as.data.frame.htna_group <- function(x, row.names = NULL, optional = FALSE,
                                     ..., what = c("edges", "nodes")) {
  what <- match.arg(what)
  .stack_by(x, "group", function(item) {
    as.data.frame.htna(item, what = what)
  })
}
