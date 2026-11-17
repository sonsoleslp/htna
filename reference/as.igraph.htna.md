# Convert an htna network to an igraph graph

`as.igraph()` converts a heterogeneous transition network to an 'igraph'
graph and keeps its actor partition. Many network measures and
algorithms are available for 'igraph' graphs; because the partition is
kept, a measure computed in 'igraph' can still be summarized by actor
type, for example to compare the centrality of human and AI codes.

`as.igraph()` creates a weighted graph from the transition weights,
stores the actor type of every vertex as the vertex attribute
`actor_type`, the ordered actor types as the graph attribute
`actor_levels` and the estimation method as `htna_method`. A grouped
network is converted cohort by cohort; each graph also records its
cohort name in the graph attribute `cohort`. It returns an `igraph`
graph, or a list of graphs, one per cohort, for a grouped network.

For the edge and node tables of the network without 'igraph', use
[`as.data.frame.htna()`](https://sonsoles.me/htna/reference/as.data.frame.htna.md).

## Usage

``` r
# S3 method for class 'htna'
as.igraph(x, mode = NULL, weighted = TRUE, diag = TRUE, ...)

# S3 method for class 'htna_group'
as.igraph(x, mode = NULL, weighted = TRUE, diag = TRUE, ...)
```

## Arguments

- x:

  An htna network from
  [`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md) or
  an `htna_group`.

- mode:

  Graph mode. Default `NULL`: `"directed"` for a directed network,
  `"undirected"` otherwise.

- weighted:

  Use the transition weights as edge weights. Default `TRUE`.

- diag:

  Keep self-transitions as loops. Default `TRUE`.

- ...:

  Further arguments to
  [`igraph::graph_from_adjacency_matrix()`](https://r.igraph.org/reference/graph_from_adjacency_matrix.html).

## Value

An `igraph` graph for an htna network. For an `htna_group`, a named list
of `igraph` graphs, one per cohort, of class
`c("htna_igraph_group", "list")`.

## Conditions

`htna_bad_input` when `x` is not an htna network or an argument is
invalid; `htna_unassigned_node` when a node has no actor type.

## See also

[`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md),
[`as.data.frame.htna()`](https://sonsoles.me/htna/reference/as.data.frame.htna.md),
[`centralities_htna()`](https://sonsoles.me/htna/reference/centralities_htna.md)

## Examples

``` r
data(human_ai)
net <- build_htna(human_ai, actor_type = "actor_type")
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions), 'actor_type' (24 sessions)
graph <- igraph::as.igraph(net)
igraph::vertex_attr(graph, "actor_type")
#>  [1] "AI"    "Human" "AI"    "AI"    "Human" "Human" "AI"    "Human" "AI"   
#> [10] "AI"    "Human" "Human"
```
