#' Compute the edge betweenness of an htna network
#'
#' @description
#' `edge_betweenness_htna()` computes the edge betweenness of an htna
#' network. The betweenness of an edge is the number of shortest paths
#' between pairs of codes that pass through it. Edges with high betweenness
#' are the bridges of the interaction: transitions through which much of
#' the flow between otherwise distant codes must pass. In a heterogeneous
#' network the high-betweenness edges are often hand-overs between actors,
#' which shows which exchanges hold the interaction together.
#'
#' `edge_betweenness_htna()` computes the betweenness of every non-zero edge
#' and returns a copy of the network whose edge weights are these scores,
#' with the actor partition kept, so the result can be drawn with
#' [plot_htna()] and tabulated with [as.data.frame.htna()]. By default a
#' larger transition probability is a shorter distance (`invert = TRUE`), as
#' in transition network analysis.
#'
#' To draw the result directly use [plot_edge_betweenness_htna()]; for
#' node-level betweenness use [centralities_htna()].
#'
#' @param x An htna network from [build_htna()].
#' @param directed Follow edge direction in shortest paths. Default `TRUE`.
#' @param invert Use `1 / weight` as the length of an edge. Default `TRUE`.
#'
#' @return An htna network of class `c("htna_edge_betweenness", class(x))`
#'   whose edge weights are edge-betweenness scores at every position where
#'   `x` has a non-zero transition. `as.data.frame()` returns one row per
#'   edge with its score in `weight`.
#'
#' @section Conditions:
#' `htna_bad_input` when `x` is not an htna network.
#'
#' @seealso [plot_edge_betweenness_htna()], [centralities_htna()],
#'   [as.data.frame.htna()]
#'
#' @examples
#' data(human_ai)
#' net <- build_htna(human_ai, actor_type = "actor_type")
#' eb  <- edge_betweenness_htna(net)
#' head(as.data.frame(eb))
#'
#' @export
edge_betweenness_htna <- function(x, directed = TRUE, invert = TRUE) {
  if (!inherits(x, "htna")) {
    .htna_stop("`x` must be an htna network from build_htna().")
  }

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

#' Plot the edge betweenness of an htna network
#'
#' @description
#' `plot_edge_betweenness_htna()` plots the edge-betweenness network of an
#' htna network: each transition is drawn with a width given by the number
#' of shortest paths that pass through it, in the layout and actor styling
#' of [plot_htna()]. The widest edges are the bridges of the interaction,
#' the transitions through which the flow between the codes of the actors
#' is channeled.
#'
#' `plot_edge_betweenness_htna()` computes the scores with
#' [edge_betweenness_htna()] (unless `x` already holds them) and draws all
#' edges with a non-zero score, unless `minimum` is given. It is called for
#' the plot and returns, invisibly, the value of [plot_htna()].
#'
#' @param x An htna network, or a result of [edge_betweenness_htna()].
#' @param directed,invert Passed to [edge_betweenness_htna()] when `x` is an
#'   htna network; ignored otherwise. Defaults `TRUE`.
#' @param ... Further arguments of [plot_htna()], such as `minimum`,
#'   `layout` or `group_colors`.
#'
#' @return Called for the plot. Invisibly, the value of [plot_htna()].
#'
#' @section Conditions:
#' `htna_bad_input` when `x` is not an htna network.
#'
#' @seealso [edge_betweenness_htna()], [plot_htna()]
#'
#' @examples
#' data(human_ai)
#' net <- build_htna(human_ai, actor_type = "actor_type")
#' plot_edge_betweenness_htna(net)
#' plot_edge_betweenness_htna(net, minimum = 5)
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
