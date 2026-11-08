# Convert an HTNA Network to igraph

Converts an `htna` network to an igraph::igraph object while preserving
its node-to-actor partition. The actor type is stored as the vertex
attribute `actor_type`, and the canonical actor ordering is stored as
the graph attribute `actor_levels`.

## Usage

``` r
# S3 method for class 'htna'
as.igraph(x, mode = NULL, weighted = TRUE, diag = TRUE, ...)

# S3 method for class 'htna_group'
as.igraph(x, mode = NULL, weighted = TRUE, diag = TRUE, ...)
```

## Arguments

- x:

  An `htna` network or `htna_group`.

- mode:

  Character igraph adjacency mode. The default, `NULL`, uses
  `"directed"` when `x$directed` is true and `"undirected"` otherwise.

- weighted:

  Passed to
  [`igraph::graph_from_adjacency_matrix()`](https://r.igraph.org/reference/graph_from_adjacency_matrix.html).
  Default `TRUE`.

- diag:

  Include diagonal/self-loop entries? Default `TRUE`.

- ...:

  Additional arguments passed to
  [`igraph::graph_from_adjacency_matrix()`](https://r.igraph.org/reference/graph_from_adjacency_matrix.html).

## Value

An `igraph` object for an `htna`, or a named list of `igraph` objects
inheriting from `htna_igraph_group` for an `htna_group`.

## Details

Passing an `htna_group` converts every cohort and returns a named list.
Each graph additionally stores its cohort name in the graph attribute
`cohort`.

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
