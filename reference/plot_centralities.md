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
if (FALSE) { # \dontrun{
net <- build_htna(list(Human = human_long, AI = ai_long))
plot_centralities(net)

grp <- build_htna(list(Human = human_long, AI = ai_long),
                  group = "cluster")
plot_centralities(grp)
} # }
```
