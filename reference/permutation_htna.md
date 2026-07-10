# Permutation Test for Network Differences

htna-named alias of
[`Nestimate::permutation()`](https://saqr.me/Nestimate/reference/permutation.html).
Tests whether observed edge-weight differences between two networks (or
all pairwise differences within a `netobject_group`) exceed what would
be expected under a null of identical generating processes.

## Usage

``` r
permutation_htna(
  x,
  y = NULL,
  iter = 1000L,
  alpha = 0.05,
  paired = FALSE,
  adjust = "none",
  measures = NULL,
  nlambda = 50L,
  seed = NULL
)
```

## Arguments

- x:

  A `netobject` (from
  [`build_network`](https://saqr.me/Nestimate/reference/build_network.html))
  or a
  [`net_edge_betweenness`](https://saqr.me/Nestimate/reference/net_edge_betweenness.html)
  object.

- y:

  A `netobject` (from
  [`build_network`](https://saqr.me/Nestimate/reference/build_network.html))
  or a
  [`net_edge_betweenness`](https://saqr.me/Nestimate/reference/net_edge_betweenness.html)
  object. Must use the same method and have the same nodes as `x`.

- iter:

  Integer. Number of permutation iterations (default: 1000).

- alpha:

  Numeric. Significance level (default: 0.05).

- paired:

  Logical. If `TRUE`, permute within pairs (requires equal number of
  observations in `x` and `y`). Default: FALSE.

- adjust:

  Character. p-value adjustment method passed to
  [`p.adjust`](https://rdrr.io/r/stats/p.adjust.html) (default:
  `"none"`). Common choices: `"holm"`, `"BH"`, `"bonferroni"`.

- measures:

  Character vector of centrality measures to permutation-test in
  addition to the edges, or `"all"` for every built-in measure. Default
  `NULL` (edges only). When supplied, the result gains a `$centralities`
  block matching the layout of `tna::permutation_test(measures = )`: per
  state and measure it reports the observed difference, an effect size
  (difference / SD of the permutation null), and a permutation p-value,
  all using the same permuted networks as the edge test. Not supported
  for `net_edge_betweenness` inputs.

- nlambda:

  Integer. Number of lambda values for the `glassopath` regularisation
  path (only used when `method = "glasso"`). Higher values give finer
  lambda resolution at the cost of speed. Default: 50.

- seed:

  Integer or NULL. RNG seed for reproducibility.

## Value

An object of class `net_permutation` (single pair) or
`net_permutation_group` (multiple pairs). See
[`Nestimate::permutation()`](https://saqr.me/Nestimate/reference/permutation.html)
for the full slot list.

## Details

Works on htna networks: the actor partition (`$node_groups`,
`$actor_levels`, `htna` class) is preserved on `result$x` / `result$y`,
so
[`plot_htna_diff()`](https://sonsoles.me/htna/reference/plot_htna_diff.md)
can render the result with htna's colour and layout conventions.

Suffixed `_htna` to avoid clashing with
[`Nestimate::permutation()`](https://saqr.me/Nestimate/reference/permutation.html)
when both packages are loaded.

## See also

[`plot_htna_diff()`](https://sonsoles.me/htna/reference/plot_htna_diff.md)
to plot the result.

## Examples

``` r
# \donttest{
data(human_ai)
grp <- build_htna(human_ai, actor_type = "actor_type", group = "phase")
#> Warning: A network with one long sequence is not recommended and can't be validated using bootstrap and other confirmatory testings.
#> Metadata aggregated per session: ties resolved by first occurrence in 'cluster' (24 sessions), 'actor_type' (9 sessions)
#> Warning: A network with one long sequence is not recommended and can't be validated using bootstrap and other confirmatory testings.
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (18 sessions), 'actor_type' (15 sessions)
permutation_htna(grp$Early, grp$Late, iter = 50)
#> Permutation Test:Transition Network (relative probabilities) [directed]
#>   Iterations: 50  |  Alpha: 0.05
#>   Nodes: 12  |  Edges tested: 135  |  Significant: 24
# }
```
