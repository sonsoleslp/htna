#' Convert an htna network to an igraph graph
#'
#' @description
#' `as.igraph()` converts a heterogeneous transition network to an 'igraph'
#' graph and keeps its actor partition. Many network measures and algorithms
#' are available for 'igraph' graphs; because the partition is kept, a
#' measure computed in 'igraph' can still be summarized by actor type, for
#' example to compare the centrality of human and AI codes.
#'
#' `as.igraph()` creates a weighted graph from the transition weights, stores
#' the actor type of every vertex as the vertex attribute `actor_type`, the
#' ordered actor types as the graph attribute `actor_levels` and the
#' estimation method as `htna_method`. A grouped network is converted cohort
#' by cohort; each graph also records its cohort name in the graph attribute
#' `cohort`. It returns an `igraph` graph, or a list of graphs, one per
#' cohort, for a grouped network.
#'
#' For the edge and node tables of the network without 'igraph', use
#' [as.data.frame.htna()].
#'
#' @param x An htna network from [build_htna()] or an `htna_group`.
#' @param mode Graph mode. Default `NULL`: `"directed"` for a directed
#'   network, `"undirected"` otherwise.
#' @param weighted Use the transition weights as edge weights. Default
#'   `TRUE`.
#' @param diag Keep self-transitions as loops. Default `TRUE`.
#' @param ... Further arguments to [igraph::graph_from_adjacency_matrix()].
#'
#' @return An `igraph` graph for an htna network. For an `htna_group`, a named
#'   list of `igraph` graphs, one per cohort, of class
#'   `c("htna_igraph_group", "list")`.
#'
#' @section Conditions:
#' `htna_bad_input` when `x` is not an htna network or an argument is
#' invalid; `htna_unassigned_node` when a node has no actor type.
#'
#' @seealso [build_htna()], [as.data.frame.htna()], [centralities_htna()]
#'
#' @examples
#' data(human_ai)
#' net <- build_htna(human_ai, actor_type = "actor_type")
#' graph <- igraph::as.igraph(net)
#' igraph::vertex_attr(graph, "actor_type")
#'
#' @importFrom igraph as.igraph
#' @export
as.igraph.htna <- function(x, mode = NULL, weighted = TRUE, diag = TRUE, ...) {
  if (!inherits(x, "htna")) {
    .htna_stop("`x` must be an `htna` network.")
  }
  W <- x$weights
  if (!is.matrix(W) || !is.numeric(W) || nrow(W) != ncol(W) ||
      is.null(rownames(W)) || is.null(colnames(W)) ||
      !identical(rownames(W), colnames(W))) {
    .htna_stop("`x$weights` must be a named numeric square matrix with identical ",
               "row and column order.")
  }
  if (is.null(mode)) {
    mode <- if (isTRUE(x$directed)) "directed" else "undirected"
  }
  if (!is.character(mode) || length(mode) != 1L || is.na(mode)) {
    .htna_stop("`mode` must be `NULL` or one character value.")
  }
  if (!is.logical(weighted) || length(weighted) != 1L || is.na(weighted)) {
    .htna_stop("`weighted` must be one non-missing logical value.")
  }
  if (!is.logical(diag) || length(diag) != 1L || is.na(diag)) {
    .htna_stop("`diag` must be one non-missing logical value.")
  }

  partition <- .htna_node_partition(x, "x")
  graph <- igraph::graph_from_adjacency_matrix(
    W, mode = mode, weighted = weighted, diag = diag, ...
  )
  vertex_names <- igraph::V(graph)$name
  actor_type <- unname(partition$lookup[vertex_names])
  graph <- igraph::set_vertex_attr(
    graph, "actor_type", value = as.character(actor_type)
  )
  graph <- igraph::set_graph_attr(
    graph, "actor_levels", as.character(partition$actor_levels)
  )
  graph <- igraph::set_graph_attr(
    graph, "htna_method", x$method %||% NA_character_
  )
  graph
}

#' @rdname as.igraph.htna
#' @export
as.igraph.htna_group <- function(x, mode = NULL, weighted = TRUE,
                                 diag = TRUE, ...) {
  if (!inherits(x, "htna_group")) {
    .htna_stop("`x` must be an `htna_group`.")
  }
  cohort_names <- names(x) %||% as.character(seq_along(x))
  out <- lapply(seq_along(x), function(i) {
    graph <- igraph::as.igraph(
      x[[i]], mode = mode, weighted = weighted, diag = diag, ...
    )
    igraph::set_graph_attr(graph, "cohort", cohort_names[i])
  })
  names(out) <- cohort_names
  structure(
    out,
    class = c("htna_igraph_group", "list"),
    actor_levels = attr(x, "actor_levels") %||%
      if (length(x)) x[[1L]]$actor_levels else character(0L)
  )
}
