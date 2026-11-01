# Centrality Stability for an htna Network

Thin wrapper around
[`Nestimate::centrality_stability()`](https://pak.dynasite.org/Nestimate/reference/centrality_stability.html)
that validates the input is an htna network (or `htna_group`), forwards
the call, and tags the result with an `htna_stability` class so callers
can dispatch on the htna form.

## Usage

``` r
centrality_stability_htna(
  x,
  measures = c("InStrength", "OutStrength", "Betweenness"),
  iter = 1000L,
  drop_prop = seq(0.1, 0.9, by = 0.1),
  threshold = 0.7,
  certainty = 0.95,
  method = "pearson",
  centrality_fn = NULL,
  loops = FALSE,
  seed = NULL
)
```

## Arguments

- x:

  An htna network from
  [`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md) or
  an `htna_group`.

- measures:

  Character vector of centrality measures to assess. Default
  `c("InStrength", "OutStrength", "Betweenness")`: the three measures
  whose values are bit-equal between htna's `cograph` engine and
  Nestimate's default centrality implementation. To assess htna's nine
  measures (including the three that differ between engines:
  `BetweennessRSP`, `Diffusion`, `Clustering`), pass them explicitly
  together with a custom `centrality_fn`.

- iter:

  Integer. Number of resamples per drop proportion. Default `1000`.

- drop_prop:

  Numeric vector. Proportions of sessions to drop. Default
  `seq(0.1, 0.9, by = 0.1)`.

- threshold:

  Numeric in \[0, 1\]. Correlation threshold for the case-dropping
  stability coefficient. Default `0.7`.

- certainty:

  Numeric in \[0, 1\]. Probability of meeting the threshold required for
  a drop proportion to count as stable. Default `0.95`.

- method:

  Correlation method, one of `"pearson"`, `"spearman"`, `"kendall"`.
  Default `"pearson"`.

- centrality_fn:

  Optional function to compute centralities. Default `NULL` (use
  Nestimate's internal implementation). See
  [`Nestimate::centrality_stability()`](https://pak.dynasite.org/Nestimate/reference/centrality_stability.html)
  for the expected signature.

- loops:

  Logical. Whether to retain self-loops. Default `FALSE`.

- seed:

  Optional integer seed for reproducibility. Default `NULL` (no seed
  reset).

## Value

For a single htna network, an object of class
`c("htna_stability", "net_stability")` with the same components as
[`Nestimate::centrality_stability()`](https://pak.dynasite.org/Nestimate/reference/centrality_stability.html):
`cs` (the stability coefficients, one per measure), `correlations`
(per-drop-proportion correlation traces), and the parameters that were
used. [`summary()`](https://rdrr.io/r/base/summary.html) returns the
tidy table: one row per (measure, drop proportion), with columns
`measure`, `drop_prop`, `mean_cor`, `sd_cor` and `prop_above`. See
[`print.htna_stability()`](https://sonsoles.me/htna/reference/print.htna_stability.md)
and
[`plot.htna_stability()`](https://sonsoles.me/htna/reference/plot.htna_stability.md).
For an `htna_group`, a named list of such objects with class
`c("htna_stability_group", "list")`.

## Details

For an `htna_group`, the call is iterated per cohort and the result is a
named list (one `htna_stability` per cohort), with class
`c("htna_stability_group", "list")`.

## References

Epskamp, S., Borsboom, D., & Fried, E. I. (2018). Estimating
psychological networks and their accuracy: A tutorial paper. *Behavior
Research Methods*, 50(1), 195-212.
[doi:10.3758/s13428-017-0862-1](https://doi.org/10.3758/s13428-017-0862-1)

## See also

[`Nestimate::centrality_stability()`](https://pak.dynasite.org/Nestimate/reference/centrality_stability.html),
[`bootstrap_htna()`](https://sonsoles.me/htna/reference/bootstrap_htna.md),
[`reliability_htna()`](https://sonsoles.me/htna/reference/reliability_htna.md).

## Examples

``` r
data(human_ai)
net <- build_htna(human_ai, actor_type = "actor_type")
#> A network with one long sequence is not recommended and can't be validated using bootstrap and other confirmatory testings.
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions), 'actor_type' (24 sessions)
cs  <- centrality_stability_htna(net, iter = 30, seed = 1)
cs            # CS-coefficient per measure
#> HTNA Centrality Stability Analysis
#> ===================================
#> Centrality Stability (30 iterations, threshold = 0.7)
#>   Drop proportions: 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9
#> 
#>   CS-coefficients:
#>     InStrength       0.90
#>     OutStrength      0.90
#>     Betweenness      0.80
summary(cs)   # mean correlation per measure and drop proportion
#>        measure drop_prop  mean_cor       sd_cor prop_above
#> 1   InStrength       0.1 0.9997933 9.399327e-05  1.0000000
#> 2   InStrength       0.2 0.9995121 2.589640e-04  1.0000000
#> 3   InStrength       0.3 0.9989968 5.254436e-04  1.0000000
#> 4   InStrength       0.4 0.9987609 7.592632e-04  1.0000000
#> 5   InStrength       0.5 0.9979411 1.335385e-03  1.0000000
#> 6   InStrength       0.6 0.9969654 1.586312e-03  1.0000000
#> 7   InStrength       0.7 0.9960166 2.181399e-03  1.0000000
#> 8   InStrength       0.8 0.9916548 5.620265e-03  1.0000000
#> 9   InStrength       0.9 0.9832595 8.474270e-03  1.0000000
#> 10 OutStrength       0.1 0.9987117 1.133226e-03  1.0000000
#> 11 OutStrength       0.2 0.9974190 2.067790e-03  1.0000000
#> 12 OutStrength       0.3 0.9960323 2.227121e-03  1.0000000
#> 13 OutStrength       0.4 0.9942741 2.601673e-03  1.0000000
#> 14 OutStrength       0.5 0.9860923 1.021467e-02  1.0000000
#> 15 OutStrength       0.6 0.9847606 8.605913e-03  1.0000000
#> 16 OutStrength       0.7 0.9775272 1.271530e-02  1.0000000
#> 17 OutStrength       0.8 0.9553243 2.549594e-02  1.0000000
#> 18 OutStrength       0.9 0.9259479 4.487740e-02  1.0000000
#> 19 Betweenness       0.1 0.9895928 9.718681e-03  1.0000000
#> 20 Betweenness       0.2 0.9786379 1.764171e-02  1.0000000
#> 21 Betweenness       0.3 0.9747873 1.717430e-02  1.0000000
#> 22 Betweenness       0.4 0.9700564 2.368309e-02  1.0000000
#> 23 Betweenness       0.5 0.9576797 3.076986e-02  1.0000000
#> 24 Betweenness       0.6 0.9564011 2.816820e-02  1.0000000
#> 25 Betweenness       0.7 0.9406976 3.667811e-02  1.0000000
#> 26 Betweenness       0.8 0.9328796 4.775810e-02  1.0000000
#> 27 Betweenness       0.9 0.8714616 7.999922e-02  0.9333333
```
