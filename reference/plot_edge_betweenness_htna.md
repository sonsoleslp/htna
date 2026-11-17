# Plot the edge betweenness of an htna network

`plot_edge_betweenness_htna()` plots the edge-betweenness network of an
htna network: each transition is drawn with a width given by the number
of shortest paths that pass through it, in the layout and actor styling
of [`plot_htna()`](https://sonsoles.me/htna/reference/plot_htna.md). The
widest edges are the bridges of the interaction, the transitions through
which the flow between the codes of the actors is channeled.

`plot_edge_betweenness_htna()` computes the scores with
[`edge_betweenness_htna()`](https://sonsoles.me/htna/reference/edge_betweenness_htna.md)
(unless `x` already holds them) and draws all edges with a non-zero
score, unless `minimum` is given. It is called for the plot and returns,
invisibly, the value of
[`plot_htna()`](https://sonsoles.me/htna/reference/plot_htna.md).

## Usage

``` r
plot_edge_betweenness_htna(x, directed = TRUE, invert = TRUE, ...)
```

## Arguments

- x:

  An htna network, or a result of
  [`edge_betweenness_htna()`](https://sonsoles.me/htna/reference/edge_betweenness_htna.md).

- directed, invert:

  Passed to
  [`edge_betweenness_htna()`](https://sonsoles.me/htna/reference/edge_betweenness_htna.md)
  when `x` is an htna network; ignored otherwise. Defaults `TRUE`.

- ...:

  Further arguments of
  [`plot_htna()`](https://sonsoles.me/htna/reference/plot_htna.md), such
  as `minimum`, `layout` or `group_colors`.

## Value

Called for the plot. Invisibly, the value of
[`plot_htna()`](https://sonsoles.me/htna/reference/plot_htna.md).

## Conditions

`htna_bad_input` when `x` is not an htna network.

## See also

[`edge_betweenness_htna()`](https://sonsoles.me/htna/reference/edge_betweenness_htna.md),
[`plot_htna()`](https://sonsoles.me/htna/reference/plot_htna.md)

## Examples

``` r
data(human_ai)
net <- build_htna(human_ai, actor_type = "actor_type")
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions), 'actor_type' (24 sessions)
plot_edge_betweenness_htna(net)

plot_edge_betweenness_htna(net, minimum = 5)

```
