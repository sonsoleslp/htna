# Edge Betweenness Network

Computes the edge betweenness of every existing edge in an htna network
and returns a copy whose `$weights` slot stores those betweenness scores
instead of the original transition weights. The actor partition is
preserved, so the result can be plotted with
[`plot_htna()`](https://sonsoles.me/htna/reference/plot_htna.md) to
visualise which edges carry the most shortest-path traffic.

## Usage

``` r
edge_betweenness_htna(x, directed = TRUE, invert = TRUE)
```

## Arguments

- x:

  An htna network from
  [`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md).

- directed:

  If `TRUE` (default), shortest paths follow edge direction.

- invert:

  If `TRUE` (default), use `1 / weight` as the edge cost when computing
  shortest paths.

## Value

A copy of `x` whose `$weights` matrix entries are edge-betweenness
scores at every position where the original network had a non-zero
transition. Class is `c("htna_edge_betweenness", class(x))`.

## Details

Mirrors
[`tna::betweenness_network()`](http://sonsoles.me/tna/reference/betweenness_network.md):
edge weights are inverted to costs by default (`invert = TRUE`) – in
transition networks a larger transition probability means the edge is
"easier" and so the equivalent path cost is smaller.

## See also

[`centralities_htna()`](https://sonsoles.me/htna/reference/centralities_htna.md)
for node-level centrality measures,
[`tna::betweenness_network()`](http://sonsoles.me/tna/reference/betweenness_network.md)
for the tna equivalent.

## Examples

``` r
if (FALSE) { # \dontrun{
data(human_long, ai_long, package = "Nestimate")
net <- build_htna(list(Human = human_long, AI = ai_long))
eb  <- edge_betweenness_htna(net)
plot_htna(eb)                 # edge thickness = betweenness score
} # }
```
