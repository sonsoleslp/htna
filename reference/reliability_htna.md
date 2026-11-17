# Estimate the split-half reliability of a heterogeneous transition network

`reliability_htna()` estimates the split-half reliability of one or more
htna networks: how far each network would look the same if it had been
estimated from a different half of the sessions. The sessions are split
at random into two halves many times, a network is estimated from each
half, and the two networks are compared: the mean, median and maximum
absolute difference of their edge weights and the correlation of their
weights. High correlations and small deviations over the splits indicate
that the estimated structure does not depend on which sessions happened
to be observed.

When several networks are given (for example, built with different
methods from the same data), each is assessed with the same splits.
`reliability_htna()` runs Nestimate's `network_reliability()`, and the
networks re-estimated on the splits keep the actor partition. It returns
an `htna_reliability` object.

For the stability of the weights when increasing shares of the sessions
are dropped use
[`casedrop_reliability_htna()`](https://sonsoles.me/htna/reference/casedrop_reliability_htna.md);
for edge-level intervals use
[`bootstrap_htna()`](https://sonsoles.me/htna/reference/bootstrap_htna.md).

## Usage

``` r
reliability_htna(..., iter = 1000L, split = 0.5, scale = "none", seed = NULL)
```

## Arguments

- ...:

  One or more htna networks from
  [`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md).

- iter:

  Number of random splits (default 1000).

- split:

  Proportion of the sessions in the first half (default 0.5).

- scale:

  Scaling of the weights before they are compared: `"none"` (default),
  `"minmax"`, `"standardize"` or `"proportion"`.

- seed:

  Random seed (default `NULL`, no seed is set).

## Value

An object of class `c("htna_reliability", "net_reliability")` with
[`print()`](https://rdrr.io/r/base/print.html),
[`summary()`](https://rdrr.io/r/base/summary.html) and
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) methods.
[`summary()`](https://rdrr.io/r/base/summary.html) returns a data frame
with one row per network and metric and columns `model`, `metric`
(`mean_dev`, `median_dev`, `cor`, `max_dev`), `mean` and `sd`.

## Conditions

An error of class `htna_empty_input` when no network is given, and of
class `htna_bad_input` when an input is not an htna network.

## See also

[`casedrop_reliability_htna()`](https://sonsoles.me/htna/reference/casedrop_reliability_htna.md),
[`bootstrap_htna()`](https://sonsoles.me/htna/reference/bootstrap_htna.md),
[`centrality_stability_htna()`](https://sonsoles.me/htna/reference/centrality_stability_htna.md).

## Examples

``` r
data(human_ai)
net <- build_htna(human_ai, actor_type = "actor_type")
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions), 'actor_type' (24 sessions)
rel <- reliability_htna(net, iter = 30, seed = 1)
rel
#> Split-Half Reliability (30 iterations, split = 50%)
#>   Mean Abs. Diff.     mean = 0.0123  sd = 0.0010
#>   Median Abs. Diff.   mean = 0.0074  sd = 0.0009
#>   Pearson             mean = 0.9824  sd = 0.0041
#>   Max Abs. Diff.      mean = 0.0980  sd = 0.0289
summary(rel)
#>      model     metric        mean           sd
#> 1 relative   mean_dev 0.012347097 0.0009948951
#> 2 relative median_dev 0.007373837 0.0008943957
#> 3 relative        cor 0.982368095 0.0041061329
#> 4 relative    max_dev 0.097975625 0.0288536573
```
