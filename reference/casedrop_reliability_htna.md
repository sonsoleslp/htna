# Edge-Weight Case-Dropping Stability

htna-named wrapper for
[`Nestimate::casedrop_reliability()`](https://pak.dynasite.org/Nestimate/reference/casedrop_reliability.html).
Computes the CS-coefficient for the edge-weight vector of a network: the
maximum proportion of cases (rows of `x$data`) that can be dropped while
the flattened edge-weight vector of the re-estimated network still
correlates with the original above `threshold` in at least `certainty`
of iterations.

## Usage

``` r
casedrop_reliability_htna(x, ...)
```

## Arguments

- x:

  An htna network or grouped htna network. See
  [`Nestimate::casedrop_reliability()`](https://pak.dynasite.org/Nestimate/reference/casedrop_reliability.html).

- ...:

  Additional arguments passed to
  [`Nestimate::casedrop_reliability()`](https://pak.dynasite.org/Nestimate/reference/casedrop_reliability.html),
  such as `iter`, `drop_prop`, `threshold`, `certainty`, `method`,
  `include_diag`, and `seed`. See that function for the full, current
  argument list.

## Value

An object of class `net_casedrop_reliability` (single network) or
`net_casedrop_reliability_group` (grouped htna). See
[`Nestimate::casedrop_reliability()`](https://pak.dynasite.org/Nestimate/reference/casedrop_reliability.html)
for the full component list and the corresponding
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) method.

## Details

Works on htna networks and grouped htna networks directly.

Suffixed `_htna` to avoid clashing with
[`Nestimate::casedrop_reliability()`](https://pak.dynasite.org/Nestimate/reference/casedrop_reliability.html)
when both packages are loaded.

## See also

[`reliability_htna()`](https://sonsoles.me/htna/reference/reliability_htna.md),
[`centrality_stability_htna()`](https://sonsoles.me/htna/reference/centrality_stability_htna.md),
[`bootstrap_htna()`](https://sonsoles.me/htna/reference/bootstrap_htna.md).

## Examples

``` r
data(human_ai)
net <- build_htna(human_ai, actor_type = "actor_type")
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions), 'actor_type' (24 sessions)
casedrop_reliability_htna(net, iter = 20, seed = 1)
#> Edge-weight Case-dropping Stability
#>   Cases (rows of $data) : 429
#>   Edges assessed        : 125 (diagonal excluded)
#>   Iterations / prop     : 20
#>   Correlation method    : spearman
#>   CS-coefficient (r)    : 0.90  (threshold=0.70, certainty=0.95)
#> 
#> Model-level reliability across iterations (mean +/- sd per drop):
#>   drop_prop      p=0.1        p=0.2        p=0.3        p=0.4        p=0.5        p=0.6        p=0.7        p=0.8        p=0.9      
#>   mean|diff|      0.002+- 0.000   0.003+- 0.000   0.004+- 0.000   0.006+- 0.001   0.007+- 0.001   0.008+- 0.001   0.010+- 0.001   0.014+- 0.002   0.020+- 0.003
#>   MAD             0.001+- 0.000   0.002+- 0.000   0.003+- 0.000   0.004+- 0.000   0.004+- 0.000   0.005+- 0.001   0.006+- 0.001   0.008+- 0.001   0.013+- 0.002
#>   cor             0.998+- 0.001   0.996+- 0.001   0.995+- 0.001   0.991+- 0.002   0.987+- 0.003   0.982+- 0.005   0.973+- 0.005   0.958+- 0.008   0.919+- 0.021
#>   max|diff|       0.016+- 0.006   0.024+- 0.008   0.028+- 0.009   0.039+- 0.017   0.052+- 0.016   0.059+- 0.018   0.076+- 0.021   0.101+- 0.039   0.118+- 0.028
```
