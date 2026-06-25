# Plot the Edge Betweenness Network

Computes
[`edge_betweenness_htna()`](https://sonsoles.me/htna/reference/edge_betweenness_htna.md)
and renders it with the htna actor styling (multi-actor circular layout,
[htna_palette](https://sonsoles.me/htna/reference/htna_palette.md) node
colours, actor legend). Edges are scaled by their betweenness score, so
the most-traveled shortest-path edges stand out.

## Usage

``` r
plot_edge_betweenness_htna(x, directed = TRUE, invert = TRUE, ...)
```

## Arguments

- x:

  An htna network from
  [`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md) or,
  equivalently, the result of
  [`edge_betweenness_htna()`](https://sonsoles.me/htna/reference/edge_betweenness_htna.md).

- directed, invert:

  Forwarded to
  [`edge_betweenness_htna()`](https://sonsoles.me/htna/reference/edge_betweenness_htna.md)
  when `x` is a plain htna network. Ignored when `x` already inherits
  from `htna_edge_betweenness`.

- ...:

  Forwarded to
  [`plot_htna()`](https://sonsoles.me/htna/reference/plot_htna.md) (e.g.
  `minimum`, `layout`, `group_colors`).

## Value

The value returned by
[`plot_htna()`](https://sonsoles.me/htna/reference/plot_htna.md)
(invisibly).

## See also

[`edge_betweenness_htna()`](https://sonsoles.me/htna/reference/edge_betweenness_htna.md),
[`plot_htna()`](https://sonsoles.me/htna/reference/plot_htna.md).

## Examples

``` r
# \donttest{
data(human_ai)
net <- build_htna(human_ai, actor_type = "actor_type")
#> Warning: A network with one long sequence is not recommended and can't be validated using bootstrap and other confirmatory testings.
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions), 'actor_type' (24 sessions)
plot_edge_betweenness_htna(net)

plot_edge_betweenness_htna(net, minimum = 5)

# }
```
