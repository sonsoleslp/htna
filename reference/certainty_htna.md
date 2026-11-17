# Estimate the closed-form certainty of the edge weights

`certainty_htna()` estimates the sampling uncertainty of the edge
weights of an htna network in closed form, instead of by resampling. The
edge weights of a relative transition network are estimated proportions,
so their uncertainty can be derived analytically: for each edge the
closed-form model gives a mean, a standard deviation, a confidence
interval, a consistency range around the observed weight and a verdict
on whether the edge is supported by the data. It serves the same purpose
as a bootstrap, showing which edges are estimated precisely enough to
interpret, at a fraction of its cost.

`certainty_htna()` computes these quantities for an htna network or for
every cohort of a grouped network. With `inference = "stability"` an
edge is supported when its interval lies within the consistency range;
with `"threshold"` when it lies above `edge_threshold`. It runs
Nestimate's `certainty()` on the network and returns a `net_certainty`
object (a `net_certainty_group` for a grouped network).

Use
[`bootstrap_htna()`](https://sonsoles.me/htna/reference/bootstrap_htna.md)
for the resampling estimate, which also covers frequency and
co-occurrence networks, and
[`prune_htna()`](https://sonsoles.me/htna/reference/prune_htna.md) with
`method = "bootstrap"` to keep only supported edges.

## Usage

``` r
certainty_htna(x, ...)
```

## Arguments

- x:

  An htna network from
  [`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md) or
  an `htna_group`.

- ...:

  Settings: `prior` (default 0.5), `ci_level` (0.05), `inference`
  (`"stability"` or `"threshold"`), `consistency_range`
  (`c(0.75, 1.25)`) and `edge_threshold`.

## Value

An object of class `c("net_certainty", "net_bootstrap")` with
[`print()`](https://rdrr.io/r/base/print.html),
[`summary()`](https://rdrr.io/r/base/summary.html) and
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) methods.
[`summary()`](https://rdrr.io/r/base/summary.html) returns a data frame
with one row per edge and columns `from`, `to`, `weight`, `mean`, `sd`,
`p_value`, `sig`, `ci_lower`, `ci_upper`, `cr_lower` and `cr_upper`. A
grouped network gives a `net_certainty_group`, a named list with one
result per cohort.

## See also

[`bootstrap_htna()`](https://sonsoles.me/htna/reference/bootstrap_htna.md)
for the resampling estimate,
[`reliability_htna()`](https://sonsoles.me/htna/reference/reliability_htna.md),
[`casedrop_reliability_htna()`](https://sonsoles.me/htna/reference/casedrop_reliability_htna.md).

## Examples

``` r
data(human_ai)
net <- build_htna(human_ai, actor_type = "actor_type")
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions), 'actor_type' (24 sessions)
cert <- certainty_htna(net)
head(summary(cert))
#>   from        to     weight       mean          sd      p_value   sig
#> 1  Ask       Ask 0.01823579 0.01840102 0.002763582 9.693523e-02 FALSE
#> 2  Ask     Check 0.06318914 0.06324027 0.005004900 1.853508e-03  TRUE
#> 3  Ask   Execute 0.02205259 0.02220812 0.003030146 6.787752e-02 FALSE
#> 4  Ask Frustrate 0.11577608 0.11569374 0.006577195 1.766878e-05  TRUE
#> 5  Ask   Inquire 0.06318914 0.06324027 0.005004900 1.853508e-03  TRUE
#> 6  Ask      Plan 0.40882103 0.40799492 0.010105889 1.031223e-25  TRUE
#>     ci_lower   ci_upper   cr_lower   cr_upper
#> 1 0.01338056 0.02419210 0.01367684 0.02279474
#> 2 0.05378625 0.07339399 0.04739186 0.07898643
#> 3 0.01666128 0.02851972 0.01653944 0.02756573
#> 4 0.10311449 0.12888880 0.08683206 0.14472010
#> 5 0.05378625 0.07339399 0.04739186 0.07898643
#> 6 0.38826334 0.42787396 0.30661578 0.51102629
```
