# Plot Centrality Measures

Faceted bar plot of node-level centralities, one panel per measure.
Mirrors the look of
[`tna::plot.tna_centralities()`](http://sonsoles.me/tna/reference/plot.tna_centralities.md).

## Usage

``` r
plot_centralities(
  x,
  measures = c("OutStrength", "InStrength", "ClosenessIn", "ClosenessOut", "Closeness",
    "Betweenness", "BetweennessRSP", "Diffusion", "Clustering"),
  by = c("state", "group"),
  reorder = TRUE,
  ncol = 3,
  scales = c("free_x", "fixed"),
  colors = NULL,
  labels = TRUE,
  ...
)
```

## Arguments

- x:

  An htna network, `htna_group`, or `htna_centralities` data frame from
  [`centralities_htna()`](https://sonsoles.me/htna/reference/centralities_htna.md).

- measures:

  Centralities to plot. Default: all nine.

- by:

  `"state"` (default) gives each node its own colour; `"group"` colours
  by actor group (Human, AI, …) using
  [htna_palette](https://sonsoles.me/htna/reference/htna_palette.md).

- reorder:

  If `TRUE`, sort nodes by value within each measure.

- ncol:

  Number of facet columns. Default `3`.

- scales:

  Facet scaling: `"free_x"` (default) or `"fixed"`.

- colors:

  Optional fill colours, recycled per group/node.

- labels:

  If `TRUE` (default), draw the value next to each bar.

- ...:

  Forwarded to
  [`centralities_htna()`](https://sonsoles.me/htna/reference/centralities_htna.md)
  when computing on the fly.

## Value

A ggplot object.

## Details

Accepts an htna network, an `htna_group`, or a data frame produced by
[`centralities_htna()`](https://sonsoles.me/htna/reference/centralities_htna.md).
For groups, bars are coloured by group within each panel.

## See also

[`centralities_htna()`](https://sonsoles.me/htna/reference/centralities_htna.md).

## Examples

``` r
# \donttest{
data(human_ai)
net <- build_htna(human_ai, actor_type = "actor_type")
#> Warning: A network with one long sequence is not recommended and can't be validated using bootstrap and other confirmatory testings.
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions), 'actor_type' (24 sessions)
plot_centralities(net)
#> Note: Weights inverted (1/w^1) for path-based measures (invert_weights=TRUE). Higher weights = shorter paths.
#> Note: Weights inverted (1/w^1) for path-based measures (invert_weights=TRUE). Higher weights = shorter paths.
#> Note: Weights inverted (1/w^1) for path-based measures (invert_weights=TRUE). Higher weights = shorter paths.
#> Note: Weights inverted (1/w^1) for path-based measures (invert_weights=TRUE). Higher weights = shorter paths.


grp <- build_htna(human_ai, actor_type = "actor_type", group = "phase")
#> Warning: A network with one long sequence is not recommended and can't be validated using bootstrap and other confirmatory testings.
#> Metadata aggregated per session: ties resolved by first occurrence in 'cluster' (24 sessions), 'actor_type' (9 sessions)
#> Warning: A network with one long sequence is not recommended and can't be validated using bootstrap and other confirmatory testings.
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (18 sessions), 'actor_type' (15 sessions)
plot_centralities(grp)
#> Note: Weights inverted (1/w^1) for path-based measures (invert_weights=TRUE). Higher weights = shorter paths.
#> Note: Weights inverted (1/w^1) for path-based measures (invert_weights=TRUE). Higher weights = shorter paths.
#> Note: Weights inverted (1/w^1) for path-based measures (invert_weights=TRUE). Higher weights = shorter paths.
#> Note: Weights inverted (1/w^1) for path-based measures (invert_weights=TRUE). Higher weights = shorter paths.
#> Note: Weights inverted (1/w^1) for path-based measures (invert_weights=TRUE). Higher weights = shorter paths.
#> Note: Weights inverted (1/w^1) for path-based measures (invert_weights=TRUE). Higher weights = shorter paths.
#> Note: Weights inverted (1/w^1) for path-based measures (invert_weights=TRUE). Higher weights = shorter paths.
#> Note: Weights inverted (1/w^1) for path-based measures (invert_weights=TRUE). Higher weights = shorter paths.

# }
```
