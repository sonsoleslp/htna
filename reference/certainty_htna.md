# Closed-form Edge Certainty of a Network

htna-named wrapper for
[`Nestimate::certainty()`](https://pak.dynasite.org/Nestimate/reference/certainty.html).
The closed-form counterpart of
[`bootstrap_htna()`](https://sonsoles.me/htna/reference/bootstrap_htna.md):
rather than resampling, it derives analytic confidence intervals and an
inference verdict for each edge weight, which is fast enough to run
where a full bootstrap would be costly.

## Usage

``` r
certainty_htna(x, ...)
```

## Arguments

- x:

  An htna network from
  [`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md) (or
  other object accepted by
  [`Nestimate::certainty()`](https://pak.dynasite.org/Nestimate/reference/certainty.html)).

- ...:

  Additional arguments passed to
  [`Nestimate::certainty()`](https://pak.dynasite.org/Nestimate/reference/certainty.html),
  such as `prior`, `ci_level`, `inference`, `consistency_range`, and
  `edge_threshold`. See that function for the full, current argument
  list.

## Value

An object of class `net_certainty` (also inheriting `net_bootstrap`).
See
[`Nestimate::certainty()`](https://pak.dynasite.org/Nestimate/reference/certainty.html)
for the full component list and the corresponding
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) method.

## Details

Works on htna networks and grouped htna networks directly.

Suffixed `_htna` to sit alongside
[`bootstrap_htna()`](https://sonsoles.me/htna/reference/bootstrap_htna.md)
and
[`reliability_htna()`](https://sonsoles.me/htna/reference/reliability_htna.md)
in the htna confirmatory-analysis family.

## See also

[`bootstrap_htna()`](https://sonsoles.me/htna/reference/bootstrap_htna.md)
for the resampling analogue,
[`reliability_htna()`](https://sonsoles.me/htna/reference/reliability_htna.md),
[`casedrop_reliability_htna()`](https://sonsoles.me/htna/reference/casedrop_reliability_htna.md).

## Examples

``` r
data(human_ai)
net <- build_htna(human_ai, actor_type = "actor_type")
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions), 'actor_type' (24 sessions)
certainty_htna(net)
#>   Edge                   Mean     95% CI          p
#>   -----------------------------------------------
#>   Delegate → Plan      0.600  [0.543, 0.656]  ***
#>   Refine → Specify     0.410  [0.377, 0.445]  ***
#>   Check → Execute      0.410  [0.383, 0.437]  ***
#>   Ask → Plan           0.408  [0.388, 0.428]  ***
#>   Repair → Execute     0.384  [0.326, 0.444]  ** 
#>   ... and 63 more certain edges
#> 
#> Certainty (Dirichlet)  [Transition Network (relative) | directed]
#>   Prior      : Dirichlet(0.50)  |  Nodes : 12
#>   Edges      : 68 certain / 135 total
#>   CI         : 95%  |  Inference: stability  |  CR [0.75, 1.25]
```
