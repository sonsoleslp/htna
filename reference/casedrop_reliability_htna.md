# Assess the case-dropping stability of the edge weights

`casedrop_reliability_htna()` measures how far the estimated edge
weights of an htna network depend on a few sessions, by case-dropping
stability (Epskamp et al., 2018). Networks are re-estimated after
dropping increasing proportions of the sessions, and the edge weights of
each reduced network are correlated with those of the full network. The
case-dropping stability (CS) coefficient is the largest proportion of
sessions that can be dropped while the correlation stays above a
threshold (0.7 by default) in a required share (95 percent) of the
resamples; Epskamp et al. (2018) recommend a coefficient of at least
0.25 and preferably above 0.5.

`casedrop_reliability_htna()` applies the procedure to the edge-weight
vector of an htna network, or to every cohort of a grouped network, and
also reports the mean and maximum absolute weight differences at each
drop proportion. It runs Nestimate's `casedrop_reliability()` on the
network and returns a `net_casedrop_reliability` object (a
`net_casedrop_reliability_group` for a grouped network).

For the stability of node centralities use
[`centrality_stability_htna()`](https://sonsoles.me/htna/reference/centrality_stability_htna.md);
for split-half agreement use
[`reliability_htna()`](https://sonsoles.me/htna/reference/reliability_htna.md);
for edge-level confidence intervals use
[`bootstrap_htna()`](https://sonsoles.me/htna/reference/bootstrap_htna.md).

## Usage

``` r
casedrop_reliability_htna(x, ...)
```

## Arguments

- x:

  An htna network from
  [`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md) or
  an `htna_group`.

- ...:

  Settings: `iter` (resamples per drop proportion, default 1000),
  `drop_prop` (default `seq(0.1, 0.9, by = 0.1)`), `threshold` (0.7),
  `certainty` (0.95), `method` (correlation, `"spearman"`),
  `include_diag` (`FALSE`), `seed`, `target` (the edges assessed:
  `"auto"`, `"existing"` or `"all"`; Nestimate 1.0.2 or later) and
  `actor` (drop whole actors instead of sessions; Nestimate 1.0.19 or
  later).

## Value

An object of class `net_casedrop_reliability` (one network) or
`net_casedrop_reliability_group` (a grouped network), with
[`print()`](https://rdrr.io/r/base/print.html),
[`summary()`](https://rdrr.io/r/base/summary.html) and
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) methods;
[`summary()`](https://rdrr.io/r/base/summary.html) returns a data frame
with one row per metric and drop proportion (columns `metric`,
`drop_prop`, `mean`, `sd`, `median`, `mad`, `q025`, `q975`).

## References

Epskamp, S., Borsboom, D., & Fried, E. I. (2018). Estimating
psychological networks and their accuracy: A tutorial paper. *Behavior
Research Methods*, 50(1), 195-212.
[doi:10.3758/s13428-017-0862-1](https://doi.org/10.3758/s13428-017-0862-1)

## See also

[`centrality_stability_htna()`](https://sonsoles.me/htna/reference/centrality_stability_htna.md),
[`reliability_htna()`](https://sonsoles.me/htna/reference/reliability_htna.md),
[`bootstrap_htna()`](https://sonsoles.me/htna/reference/bootstrap_htna.md).

## Examples

``` r
data(human_ai)
net <- build_htna(human_ai, actor_type = "actor_type")
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions), 'actor_type' (24 sessions)
casedrop_reliability_htna(net, iter = 20, seed = 1)
#> Edge-weight Case-dropping Stability
#>   Cases (resampled)     : 429
#>   Edges assessed        : 125 (diagonal excluded)
#>   Iterations / prop     : 20
#>   Correlation method    : spearman
#>   CS-coefficient (r)    : 0.90  (threshold=0.70, certainty=0.95)
#> 
#> Model-level reliability across iterations (mean +/- sd per drop):
#>   drop_prop      p=0.1        p=0.2        p=0.3        p=0.4        p=0.5        p=0.6        p=0.7        p=0.8        p=0.9      
#>   mean|diff|      0.002+- 0.000   0.003+- 0.000   0.004+- 0.000   0.006+- 0.001   0.007+- 0.001   0.008+- 0.001   0.010+- 0.001   0.014+- 0.002   0.020+- 0.003
#>   median|diff|    0.001+- 0.000   0.002+- 0.000   0.003+- 0.000   0.004+- 0.000   0.004+- 0.000   0.005+- 0.001   0.006+- 0.001   0.008+- 0.001   0.013+- 0.002
#>   cor             0.998+- 0.001   0.996+- 0.001   0.995+- 0.001   0.991+- 0.002   0.987+- 0.003   0.982+- 0.005   0.973+- 0.005   0.958+- 0.008   0.919+- 0.021
#>   max|diff|       0.016+- 0.006   0.024+- 0.008   0.028+- 0.009   0.039+- 0.017   0.052+- 0.016   0.059+- 0.018   0.076+- 0.021   0.101+- 0.039   0.118+- 0.028
```
