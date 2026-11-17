# Tabulate centrality stability results

[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) returns
the results of
[`centrality_stability_htna()`](https://sonsoles.me/htna/reference/centrality_stability_htna.md)
as a tidy data frame; [`summary()`](https://rdrr.io/r/base/summary.html)
of a grouped result returns the stacked correlation table. The CS
coefficients (one row per measure) summarize how many sessions can be
dropped before the centrality ranking changes; the correlation table
(one row per measure and drop proportion) shows the trace behind each
coefficient. For a grouped result the rows of every cohort are stacked
with a leading `group` column.

## Usage

``` r
# S3 method for class 'htna_stability'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  ...,
  what = c("cs", "correlations")
)

# S3 method for class 'htna_stability_group'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  ...,
  what = c("cs", "correlations")
)

# S3 method for class 'htna_stability_group'
summary(object, ...)
```

## Arguments

- x:

  An `htna_stability` or `htna_stability_group`.

- row.names, optional:

  Ignored; present for the generic.

- ...:

  Unused.

- what:

  `"cs"` (default) for the CS coefficients, or `"correlations"` for the
  correlation table.

- object:

  An `htna_stability_group`.

## Value

A data frame. For `what = "cs"`: columns `measure` and `cs`, one row per
measure. For `what = "correlations"`: columns `measure`, `drop_prop`,
`mean_cor`, `sd_cor` and `prop_above` (the share of resamples with a
correlation above the threshold), one row per measure and drop
proportion. Grouped results add a first column `group`.
[`summary()`](https://rdrr.io/r/base/summary.html) of a grouped result
returns the stacked correlation table.

## See also

[`centrality_stability_htna()`](https://sonsoles.me/htna/reference/centrality_stability_htna.md),
[`plot.htna_stability()`](https://sonsoles.me/htna/reference/plot.htna_stability.md)

## Examples

``` r
data(human_ai)
net <- build_htna(human_ai, actor_type = "actor_type")
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions), 'actor_type' (24 sessions)
cs  <- centrality_stability_htna(net, iter = 20, seed = 1)
as.data.frame(cs)
#>       measure  cs
#> 1  InStrength 0.9
#> 2 OutStrength 0.9
#> 3 Betweenness 0.9
as.data.frame(cs, what = "correlations")
#>        measure drop_prop  mean_cor       sd_cor prop_above
#> 1   InStrength       0.1 0.9997693 0.0000829210       1.00
#> 2   InStrength       0.2 0.9995194 0.0002179156       1.00
#> 3   InStrength       0.3 0.9991887 0.0004616232       1.00
#> 4   InStrength       0.4 0.9986174 0.0007577434       1.00
#> 5   InStrength       0.5 0.9978186 0.0011124653       1.00
#> 6   InStrength       0.6 0.9969020 0.0013688548       1.00
#> 7   InStrength       0.7 0.9956556 0.0017957725       1.00
#> 8   InStrength       0.8 0.9898369 0.0071574619       1.00
#> 9   InStrength       0.9 0.9829253 0.0131448139       1.00
#> 10 OutStrength       0.1 0.9986885 0.0012503776       1.00
#> 11 OutStrength       0.2 0.9976329 0.0015312658       1.00
#> 12 OutStrength       0.3 0.9959786 0.0022636920       1.00
#> 13 OutStrength       0.4 0.9920891 0.0050980391       1.00
#> 14 OutStrength       0.5 0.9920660 0.0050921029       1.00
#> 15 OutStrength       0.6 0.9807621 0.0107063031       1.00
#> 16 OutStrength       0.7 0.9780773 0.0193765576       1.00
#> 17 OutStrength       0.8 0.9549291 0.0316177717       1.00
#> 18 OutStrength       0.9 0.9174109 0.0491902262       1.00
#> 19 Betweenness       0.1 0.9901172 0.0101322569       1.00
#> 20 Betweenness       0.2 0.9833133 0.0170060597       1.00
#> 21 Betweenness       0.3 0.9814206 0.0187982189       1.00
#> 22 Betweenness       0.4 0.9699597 0.0258978624       1.00
#> 23 Betweenness       0.5 0.9654079 0.0299003299       1.00
#> 24 Betweenness       0.6 0.9600929 0.0266669865       1.00
#> 25 Betweenness       0.7 0.9395628 0.0457957655       1.00
#> 26 Betweenness       0.8 0.9214871 0.0436274965       1.00
#> 27 Betweenness       0.9 0.8807571 0.0754946982       0.95
```
