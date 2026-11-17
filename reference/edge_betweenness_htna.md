# Compute the edge betweenness of an htna network

`edge_betweenness_htna()` computes the edge betweenness of an htna
network. The betweenness of an edge is the number of shortest paths
between pairs of codes that pass through it. Edges with high betweenness
are the bridges of the interaction: transitions through which much of
the flow between otherwise distant codes must pass. In a heterogeneous
network the high-betweenness edges are often hand-overs between actors,
which shows which exchanges hold the interaction together.

`edge_betweenness_htna()` computes the betweenness of every non-zero
edge and returns a copy of the network whose edge weights are these
scores, with the actor partition kept, so the result can be drawn with
[`plot_htna()`](https://sonsoles.me/htna/reference/plot_htna.md) and
tabulated with
[`as.data.frame.htna()`](https://sonsoles.me/htna/reference/as.data.frame.htna.md).
By default a larger transition probability is a shorter distance
(`invert = TRUE`), as in transition network analysis.

To draw the result directly use
[`plot_edge_betweenness_htna()`](https://sonsoles.me/htna/reference/plot_edge_betweenness_htna.md);
for node-level betweenness use
[`centralities_htna()`](https://sonsoles.me/htna/reference/centralities_htna.md).

## Usage

``` r
edge_betweenness_htna(x, directed = TRUE, invert = TRUE)
```

## Arguments

- x:

  An htna network from
  [`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md).

- directed:

  Follow edge direction in shortest paths. Default `TRUE`.

- invert:

  Use `1 / weight` as the length of an edge. Default `TRUE`.

## Value

An htna network of class `c("htna_edge_betweenness", class(x))` whose
edge weights are edge-betweenness scores at every position where `x` has
a non-zero transition.
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) returns
one row per edge with its score in `weight`.

## Conditions

`htna_bad_input` when `x` is not an htna network.

## See also

[`plot_edge_betweenness_htna()`](https://sonsoles.me/htna/reference/plot_edge_betweenness_htna.md),
[`centralities_htna()`](https://sonsoles.me/htna/reference/centralities_htna.md),
[`as.data.frame.htna()`](https://sonsoles.me/htna/reference/as.data.frame.htna.md)

## Examples

``` r
data(human_ai)
net <- build_htna(human_ai, actor_type = "actor_type")
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions), 'actor_type' (24 sessions)
eb  <- edge_betweenness_htna(net)
head(as.data.frame(eb))
#>    from      to weight source_actor target_actor
#> 1   Ask    Plan     24           AI           AI
#> 2 Check     Ask      2        Human           AI
#> 3 Check Execute      5        Human           AI
#> 4 Check  Repair      1        Human           AI
#> 5 Check  Report      1        Human           AI
#> 6 Check Specify      2        Human        Human
```
