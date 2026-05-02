#' Edge Betweenness Network
#'
#' Computes the edge betweenness of every existing edge in an htna
#' network and returns a copy whose `$weights` slot stores those
#' betweenness scores instead of the original transition weights. The
#' actor partition is preserved, so the result can be plotted with
#' [plot_htna()] to visualise which edges carry the most shortest-path
#' traffic.
#'
#' Mirrors `tna::betweenness_network()`: edge weights are inverted to
#' costs by default (`invert = TRUE`) -- in transition networks a
#' larger transition probability means the edge is "easier" and so the
#' equivalent path cost is smaller.
#'
#' @param x An htna network from [build_htna()].
#' @param directed If `TRUE` (default), shortest paths follow edge
#'   direction.
#' @param invert  If `TRUE` (default), use `1 / weight` as the edge
#'   cost when computing shortest paths.
#'
#' @return A copy of `x` whose `$weights` matrix entries are
#'   edge-betweenness scores at every position where the original
#'   network had a non-zero transition. Class is
#'   `c("htna_edge_betweenness", class(x))`.
#'
#' @seealso [centralities()] for node-level centrality measures,
#'   [tna::betweenness_network()] for the tna equivalent.
#'
#' @examples
#' \dontrun{
#' data(human_long, ai_long, package = "Nestimate")
#' net <- build_htna(list(Human = human_long, AI = ai_long))
#' eb  <- edge_betweenness_htna(net)
#' plot_htna(eb)                 # edge thickness = betweenness score
#' }
#'
#' @export
edge_betweenness_htna <- function(x, directed = TRUE, invert = TRUE) {
  if (!inherits(x, "htna")) {
    stop("`x` must be an htna network from build_htna().", call. = FALSE)
  }
  if (!requireNamespace("igraph", quietly = TRUE)) {  # nocov start
    stop("`edge_betweenness_htna()` needs the 'igraph' package. ",
         "Install it with `install.packages(\"igraph\")`.", call. = FALSE)
  }                                                    # nocov end

  W <- .weights_of(x)
  g <- igraph::graph_from_adjacency_matrix(W, mode = "directed",
                                           weighted = TRUE)
  w <- if (isTRUE(invert)) 1 / igraph::E(g)$weight else igraph::E(g)$weight
  igraph::E(g)$weight <- w
  bet <- igraph::edge_betweenness(g, directed = directed)

  # tna's column-major fill: transpose, write into non-zero cells, transpose back.
  weights_t <- t(W)
  weights_t[weights_t > 0] <- bet
  res <- x
  res$weights <- t(weights_t)
  class(res) <- c("htna_edge_betweenness", class(res))
  res
}

#' Plot the Edge Betweenness Network
#'
#' Computes [edge_betweenness_htna()] and renders it with the htna
#' actor styling (multi-actor circular layout, [htna_palette] node
#' colours, actor legend). Edges are scaled by their betweenness
#' score, so the most-traveled shortest-path edges stand out.
#'
#' @param x An htna network from [build_htna()] or, equivalently, the
#'   result of [edge_betweenness_htna()].
#' @param directed,invert Forwarded to [edge_betweenness_htna()] when
#'   `x` is a plain htna network. Ignored when `x` already inherits
#'   from `htna_edge_betweenness`.
#' @param ... Forwarded to [plot_htna()] (e.g. `minimum`, `layout`,
#'   `group_colors`).
#'
#' @return The value returned by [plot_htna()] (invisibly).
#' @seealso [edge_betweenness_htna()], [plot_htna()].
#'
#' @examples
#' \dontrun{
#' data(human_long, ai_long, package = "Nestimate")
#' net <- build_htna(list(Human = human_long, AI = ai_long))
#' plot_edge_betweenness_htna(net)
#' plot_edge_betweenness_htna(net, minimum = 5)
#' }
#'
#' @export
plot_edge_betweenness_htna <- function(x,
                                       directed = TRUE,
                                       invert   = TRUE,
                                       ...) {
  eb <- if (inherits(x, "htna_edge_betweenness")) x else
          edge_betweenness_htna(x, directed = directed, invert = invert)
  args <- list(...)
  if (is.null(args$minimum)) args$minimum <- 0
  do.call(plot_htna, c(list(eb), args))
}
