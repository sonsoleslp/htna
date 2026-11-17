# Bootstrap the edges of a heterogeneous transition network

`bootstrap_htna()` bootstraps the edges of a heterogeneous transition
network, or of every cohort of a grouped network, to show which
transitions would reappear in another sample of sessions. The
non-parametric bootstrap resamples the sessions with replacement,
re-estimates the network on each resample, and summarizes the
distribution of every edge weight: its mean, standard deviation,
percentile confidence interval, and a p-value. An edge weight estimated
from a sample of sessions is uncertain, and a plot of the network alone
does not show that uncertainty.

Under the default `inference = "stability"`, the p-value is the share of
resamples (plus one, over the number of resamples plus one) whose weight
falls outside a consistency range around the observed weight (by default
75 to 125 percent of it); an edge with a p-value below `ci_level` is
reproduced by the resamples and is marked significant. The bootstrap
works at the level of individual codes and runs Nestimate's
`bootstrap_network()` with the actor partition kept, so
[`plot_htna_bootstrap()`](https://sonsoles.me/htna/reference/plot_htna_bootstrap.md)
draws the result with the actor layout and colours.
[`bootstrap()`](https://sonsoles.me/htna/reference/bootstrap.md) is the
generic, with a method for htna networks. It returns an `htna_bootstrap`
object, or one per cohort for a grouped network.

For a closed-form estimate of the same quantities use
[`certainty_htna()`](https://sonsoles.me/htna/reference/certainty_htna.md);
to remove unstable edges use
[`prune_htna()`](https://sonsoles.me/htna/reference/prune_htna.md) with
`method = "bootstrap"`; for the stability of centralities use
[`centrality_stability_htna()`](https://sonsoles.me/htna/reference/centrality_stability_htna.md).

## Usage

``` r
bootstrap_htna(x, ...)

# S3 method for class 'htna'
bootstrap(x, ...)
```

## Arguments

- x:

  An htna network from
  [`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md), or
  an `htna_group`, in which case every cohort is bootstrapped.

- ...:

  Settings: `iter` (resamples, default 1000), `ci_level` (0.05),
  `inference` (`"stability"` or `"threshold"`), `consistency_range`
  (`c(0.75, 1.25)`), `edge_threshold`, `seed`, `boundary`, `ci_method`
  and `actor` (resample whole actors instead of sessions).

## Value

For a network, an object of class `c("htna_bootstrap", "net_bootstrap")`
with [`print()`](https://rdrr.io/r/base/print.html),
[`summary()`](https://rdrr.io/r/base/summary.html) and
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) methods.
[`summary()`](https://rdrr.io/r/base/summary.html) returns a data frame
with one row per edge and columns `from`, `to`, `weight`, `mean`, `sd`,
`p_value`, `sig`, `ci_lower`, `ci_upper`, `cr_lower` and `cr_upper`. For
an `htna_group`, a named list of such objects with class
`c("htna_bootstrap_group", "list")`.

## Conditions

An error of class `htna_bad_input` is raised when `x` is neither an htna
network nor a list of them.

## See also

[`plot_htna_bootstrap()`](https://sonsoles.me/htna/reference/plot_htna_bootstrap.md),
[`certainty_htna()`](https://sonsoles.me/htna/reference/certainty_htna.md),
[`prune_htna()`](https://sonsoles.me/htna/reference/prune_htna.md),
[`reliability_htna()`](https://sonsoles.me/htna/reference/reliability_htna.md).

## Examples

``` r
data(human_ai)
net  <- build_htna(human_ai, actor_type = "actor_type")
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions), 'actor_type' (24 sessions)
boot <- bootstrap_htna(net, iter = 50, seed = 1)
boot
#>   Edge                   Mean     95% CI          p
#>   -----------------------------------------------
#>   Delegate → Plan        0.614  [0.563, 0.672]  *  
#>   Refine → Specify       0.414  [0.362, 0.462]  *  
#>   Ask → Plan             0.413  [0.384, 0.441]  *  
#>   Check → Execute        0.410  [0.385, 0.441]  *  
#>   Repair → Execute       0.391  [0.346, 0.447]  *  
#>   ... and 54 more significant edges
#> 
#> Bootstrap Network  [Transition Network (relative) | directed]
#>   Iterations : 50  |  Nodes : 12
#>   Edges      : 59 significant / 135 total
#>   CI         : 95%  |  Inference: stability  |  CR [0.75, 1.25]
head(summary(boot))
#>   from        to     weight       mean          sd    p_value   sig   ci_lower
#> 1  Ask       Ask 0.01823579 0.01815658 0.002568966 0.07843137 FALSE 0.01409162
#> 2  Ask     Check 0.06318914 0.06441535 0.005332088 0.01960784  TRUE 0.05316406
#> 3  Ask   Execute 0.02205259 0.02317822 0.003479632 0.07843137 FALSE 0.01676806
#> 4  Ask Frustrate 0.11577608 0.11536263 0.008098562 0.01960784  TRUE 0.10066673
#> 5  Ask   Inquire 0.06318914 0.06199458 0.005052261 0.01960784  TRUE 0.05260678
#> 6  Ask      Plan 0.40882103 0.41296387 0.016228995 0.01960784  TRUE 0.38405192
#>     ci_upper   cr_lower   cr_upper
#> 1 0.02306251 0.01367684 0.02279474
#> 2 0.07614154 0.04739186 0.07898643
#> 3 0.03058641 0.01653944 0.02756573
#> 4 0.12878731 0.08683206 0.14472010
#> 5 0.07130141 0.04739186 0.07898643
#> 6 0.44053670 0.30661578 0.51102629
```
