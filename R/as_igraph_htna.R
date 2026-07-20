#' Convert an HTNA Network to igraph
#'
#' Converts an `htna` network to an [igraph::igraph] object while preserving
#' its node-to-actor partition. The actor type is stored as the vertex
#' attribute `actor_type`, and the canonical actor ordering is stored as the
#' graph attribute `actor_levels`.
#'
#' Passing an `htna_group` converts every cohort and returns a named list. Each
#' graph additionally stores its cohort name in the graph attribute `cohort`.
#'
#' @param x An `htna` network or `htna_group`.
#' @param mode Character igraph adjacency mode. The default, `NULL`, uses
#'   `"directed"` when `x$directed` is true and `"undirected"` otherwise.
#' @param weighted Passed to [igraph::graph_from_adjacency_matrix()]. Default
#'   `TRUE`.
#' @param diag Include diagonal/self-loop entries? Default `TRUE`.
#' @param ... Additional arguments passed to
#'   [igraph::graph_from_adjacency_matrix()].
#'
#' @return An `igraph` object for an `htna`, or a named list of `igraph`
#'   objects inheriting from `htna_igraph_group` for an `htna_group`.
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
    stop("`x` must be an `htna` network.", call. = FALSE)
  }
  W <- x$weights
  if (!is.matrix(W) || !is.numeric(W) || nrow(W) != ncol(W) ||
      is.null(rownames(W)) || is.null(colnames(W)) ||
      !identical(rownames(W), colnames(W))) {
    stop("`x$weights` must be a named numeric square matrix with identical ",
         "row and column order.", call. = FALSE)
  }
  if (is.null(mode)) {
    mode <- if (isTRUE(x$directed)) "directed" else "undirected"
  }
  if (!is.character(mode) || length(mode) != 1L || is.na(mode)) {
    stop("`mode` must be `NULL` or one character value.", call. = FALSE)
  }
  if (!is.logical(weighted) || length(weighted) != 1L || is.na(weighted)) {
    stop("`weighted` must be one non-missing logical value.", call. = FALSE)
  }
  if (!is.logical(diag) || length(diag) != 1L || is.na(diag)) {
    stop("`diag` must be one non-missing logical value.", call. = FALSE)
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
    stop("`x` must be an `htna_group`.", call. = FALSE)
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
