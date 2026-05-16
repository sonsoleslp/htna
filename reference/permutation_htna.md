# Permutation Test for Network Differences

htna-named alias of
[`Nestimate::permutation()`](https://rdrr.io/pkg/Nestimate/man/permutation.html).
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
  nlambda = 50L,
  seed = NULL
)
```

## Arguments

- x:

  A `netobject` (from
  [`build_network`](https://rdrr.io/pkg/Nestimate/man/build_network.html)).

- y:

  A `netobject` (from
  [`build_network`](https://rdrr.io/pkg/Nestimate/man/build_network.html)).
  Must use the same method and have the same nodes as `x`.

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

- nlambda:

  Integer. Number of lambda values for the `glassopath` regularisation
  path (only used when `method = "glasso"`). Higher values give finer
  lambda resolution at the cost of speed. Default: 50.

- seed:

  Integer or NULL. RNG seed for reproducibility.

## Value

An object of class `net_permutation` (single pair) or
`net_permutation_group` (multiple pairs). See
[`Nestimate::permutation()`](https://rdrr.io/pkg/Nestimate/man/permutation.html)
for the full slot list.

## Details

Works on htna networks: the actor partition (`$node_groups`,
`$actor_levels`, `htna` class) is preserved on `result$x` / `result$y`,
so
[`plot_htna_diff()`](https://sonsoles.me/htna/reference/plot_htna_diff.md)
can render the result with htna's colour and layout conventions.

Suffixed `_htna` to avoid clashing with
[`Nestimate::permutation()`](https://rdrr.io/pkg/Nestimate/man/permutation.html)
when both packages are loaded.

## See also

[`plot_htna_diff()`](https://sonsoles.me/htna/reference/plot_htna_diff.md)
to plot the result.

## Examples

``` r
# \donttest{
data(human_long, ai_long, package = "Nestimate")
n <- nrow(human_long)
early <- build_htna(list(Human = human_long[seq_len(n %/% 2), ],
                         AI    = ai_long[seq_len(n %/% 2), ]))
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (25 sessions)
late  <- build_htna(list(Human = human_long[(n %/% 2 + 1):n, ],
                         AI    = ai_long[(n %/% 2 + 1):n, ]))
#> Metadata aggregated per session: ties resolved by first occurrence in 'cluster' (15 sessions)
permutation_htna(early, late, iter = 100)
#> Permutation Test:Transition Network (relative probabilities) [directed]
#>   Iterations: 100  |  Alpha: 0.05
#>   Nodes: 17  |  Edges tested: 272  |  Significant: 59
# }
```
