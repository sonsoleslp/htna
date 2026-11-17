# Assess the case-dropping stability of node centralities

`centrality_stability_htna()` computes the case-dropping stability (CS)
coefficients of the node centralities of an htna network, or of every
cohort of a grouped network (Epskamp et al., 2018). The procedure
re-estimates the network after dropping increasing proportions of the
sessions and correlates the centralities of each reduced network with
those of the full network; the CS coefficient of a measure is the
largest proportion of sessions that can be dropped while the correlation
stays above `threshold` with probability `certainty`. Epskamp et al.
(2018) recommend a coefficient of at least 0.25 and preferably above
0.5.

Centrality measures rank the codes of a network, for example by how much
transition weight flows into them (in-strength) or how often they lie on
shortest paths (betweenness); before such a ranking is interpreted, it
should be shown not to depend on a few sessions.
`centrality_stability_htna()` runs Nestimate's `centrality_stability()`
on the network and returns an `htna_stability` object (an
`htna_stability_group` for a grouped network). Printing the result shows
the coefficients,
[`plot.htna_stability()`](https://sonsoles.me/htna/reference/plot.htna_stability.md)
draws the correlation traces, and
[`as.data.frame.htna_stability()`](https://sonsoles.me/htna/reference/as.data.frame.htna_stability.md)
returns the coefficients or the traces as data frames.

For the stability of the edge weights use
[`casedrop_reliability_htna()`](https://sonsoles.me/htna/reference/casedrop_reliability_htna.md);
for edge-level intervals use
[`bootstrap_htna()`](https://sonsoles.me/htna/reference/bootstrap_htna.md).

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
  seed = NULL,
  ...
)
```

## Arguments

- x:

  An htna network from
  [`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md) or
  an `htna_group`.

- measures:

  Centrality measures to assess (default
  `c("InStrength", "OutStrength", "Betweenness")`).

- iter:

  Resamples per drop proportion (default 1000).

- drop_prop:

  Proportions of sessions to drop (default `seq(0.1, 0.9, by = 0.1)`).

- threshold:

  Correlation the reduced networks must reach (default 0.7).

- certainty:

  Probability of reaching `threshold` required at a drop proportion
  (default 0.95).

- method:

  Correlation: `"pearson"` (default), `"spearman"` or `"kendall"`.

- centrality_fn:

  A function computing the centralities from a weight matrix; `NULL`
  (default) uses the built-in measures.

- loops:

  Keep self-transitions when computing centralities (default `FALSE`).

- seed:

  Random seed (default `NULL`, no seed is set).

- ...:

  Further arguments passed to
  [`Nestimate::centrality_stability()`](https://pak.dynasite.org/Nestimate/reference/centrality_stability.html),
  for example `actor` (drop whole actors instead of sessions; Nestimate
  1.0.19 or later).

## Value

For a network, an object of class `c("htna_stability", "net_stability")`
holding the CS coefficients (`cs`, one per measure), the correlation
traces and the settings. For an `htna_group`, a named list of such
objects with class `c("htna_stability_group", "list")`.
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) returns
the coefficients (one row per measure) or, with `what = "correlations"`,
the traces (one row per measure and drop proportion);
[`summary()`](https://rdrr.io/r/base/summary.html) returns the traces.

## Conditions

Errors of class `htna_bad_input` when `x` is not an htna network and
`htna_empty_input` when a group is empty.

## References

Epskamp, S., Borsboom, D., & Fried, E. I. (2018). Estimating
psychological networks and their accuracy: A tutorial paper. *Behavior
Research Methods*, 50(1), 195-212.
[doi:10.3758/s13428-017-0862-1](https://doi.org/10.3758/s13428-017-0862-1)

## See also

[`plot.htna_stability()`](https://sonsoles.me/htna/reference/plot.htna_stability.md),
[`as.data.frame.htna_stability()`](https://sonsoles.me/htna/reference/as.data.frame.htna_stability.md),
[`casedrop_reliability_htna()`](https://sonsoles.me/htna/reference/casedrop_reliability_htna.md),
[`bootstrap_htna()`](https://sonsoles.me/htna/reference/bootstrap_htna.md).

## Examples

``` r
data(human_ai)
net <- build_htna(human_ai, actor_type = "actor_type")
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions), 'actor_type' (24 sessions)
cs  <- centrality_stability_htna(net, iter = 30, seed = 1)
cs
#> HTNA centrality stability
#>   30 iterations, threshold = 0.70, drop proportions 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9
#> 
#>   CS-coefficients:
#>     InStrength       0.90
#>     OutStrength      0.90
#>     Betweenness      0.80
as.data.frame(cs, what = "correlations")
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
