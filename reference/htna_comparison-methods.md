# Print, summarize and tabulate an htna network comparison

[`print()`](https://rdrr.io/r/base/print.html),
[`summary()`](https://rdrr.io/r/base/summary.html) and
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) read an
`htna_comparison` object from
[`compare_htna()`](https://sonsoles.me/htna/reference/compare_htna.md),
which describes how two heterogeneous transition networks differ edge by
edge. Beyond the overall similarity and distance measures of the two
weight matrices, the comparison splits the differences by actor-pair
block: the transitions within each actor (for example Human to Human)
and between actors (Human to AI, AI to Human). The actor-pair table
shows where in the actor structure the two networks differ most.

[`print()`](https://rdrr.io/r/base/print.html) shows the overall
measures and the actor-pair table; for a grouped comparison it shows,
for every cohort pair, the correlation and mean absolute difference of
the edge weights and the actor-pair table.
[`summary()`](https://rdrr.io/r/base/summary.html) returns the
actor-pair table.
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) returns
any of the comparison's tables, chosen with `what`; for a grouped
comparison the tables of every pair are stacked with a leading
`comparison` column.

## Usage

``` r
# S3 method for class 'htna_comparison'
print(x, ...)

# S3 method for class 'htna_comparison'
summary(object, ...)

# S3 method for class 'htna_comparison'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  ...,
  what = c("actor_pairs", "edges", "metrics", "network")
)

# S3 method for class 'htna_comparison_group'
print(x, ...)

# S3 method for class 'htna_comparison_group'
summary(object, ...)

# S3 method for class 'htna_comparison_group'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  ...,
  what = c("actor_pairs", "edges", "metrics", "network")
)
```

## Arguments

- x, object:

  An `htna_comparison` or `htna_comparison_group` from
  [`compare_htna()`](https://sonsoles.me/htna/reference/compare_htna.md).

- ...:

  Unused.

- row.names, optional:

  Ignored; present for the generic.

- what:

  Table to return: `"actor_pairs"` (default; one row per directed actor
  pair), `"edges"` (one row per edge of the aligned networks),
  `"metrics"` (one row per similarity or distance measure) or
  `"network"` (one row per network-level metric, with the values of both
  networks).

## Value

[`print()`](https://rdrr.io/r/base/print.html) returns `x` invisibly.
[`summary()`](https://rdrr.io/r/base/summary.html) returns the
actor-pair data frame.
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) returns a
data frame:

- `"actor_pairs"`: `source_actor`, `target_actor`, `n_cells`,
  `n_nonzero_x`, `n_nonzero_y`, `n_changed`, `mean_difference`,
  `mean_absolute_difference`, `rms_difference`,
  `max_absolute_difference`; differences are `x - y`.

- `"edges"`: `source`, `target`, `weight_x`, `weight_y`, the difference
  measures of each edge, `source_actor` and `target_actor`.

- `"metrics"`: `category`, `metric`, `value`.

- `"network"`: `metric`, `x`, `y`.

For a grouped comparison every table has a first column `comparison`
(for example `"Early vs Late"`), and the first-named cohort is `x`.

## See also

[`compare_htna()`](https://sonsoles.me/htna/reference/compare_htna.md),
[`plot.htna_comparison_group()`](https://sonsoles.me/htna/reference/plot.htna_comparison_group.md)

## Examples

``` r
data(human_ai)
grp <- build_htna(human_ai, actor_type = "actor_type", group = "phase")
#> Metadata aggregated per session: ties resolved by first occurrence in 'cluster' (24 sessions), 'actor_type' (9 sessions)
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (18 sessions), 'actor_type' (15 sessions)
comparison <- compare_htna(grp)
comparison
#> HTNA network comparisons: 1 cohort pair
#> 
#> --- Early vs Late ---
#>   Pearson correlation of edge weights: 0.959
#>   Mean absolute difference:            0.0192
#>  source target cells changed mean_diff mean_abs_diff max_abs_diff
#>   Human  Human    36      35   0.00168        0.0159       0.0631
#>   Human     AI    36      36  -0.00168        0.0179       0.0922
#>      AI  Human    36      36  -0.00622        0.0216       0.0571
#>      AI     AI    36      28   0.00622        0.0212       0.1810
summary(comparison)
#>      comparison source_actor target_actor n_cells n_nonzero_x n_nonzero_y
#> 1 Early vs Late        Human        Human      36          35          35
#> 2 Early vs Late        Human           AI      36          36          36
#> 3 Early vs Late           AI        Human      36          36          36
#> 4 Early vs Late           AI           AI      36          28          27
#>   n_changed mean_difference mean_absolute_difference rms_difference
#> 1        35     0.001681478               0.01593235     0.02279098
#> 2        36    -0.001681478               0.01791131     0.02767092
#> 3        36    -0.006221056               0.02158038     0.02683857
#> 4        28     0.006221056               0.02124805     0.04281287
#>   max_absolute_difference
#> 1              0.06311526
#> 2              0.09221141
#> 3              0.05710535
#> 4              0.18124427
as.data.frame(comparison, what = "metrics")
#>       comparison             category               metric       value
#> 1  Early vs Late    Weight Deviations      Mean Abs. Diff.  0.01916802
#> 2  Early vs Late    Weight Deviations    Median Abs. Diff.  0.01073422
#> 3  Early vs Late    Weight Deviations            RMS Diff.  0.03097725
#> 4  Early vs Late    Weight Deviations       Max Abs. Diff.  0.18124427
#> 5  Early vs Late    Weight Deviations Rel. Mean Abs. Diff.  0.23001624
#> 6  Early vs Late    Weight Deviations             CV Ratio  1.09573174
#> 7  Early vs Late         Correlations              Pearson  0.95944448
#> 8  Early vs Late         Correlations             Spearman  0.93646692
#> 9  Early vs Late         Correlations              Kendall  0.79484435
#> 10 Early vs Late         Correlations             Distance  0.94348643
#> 11 Early vs Late      Dissimilarities            Euclidean  0.37172695
#> 12 Early vs Late      Dissimilarities            Manhattan  2.76019489
#> 13 Early vs Late      Dissimilarities             Canberra 26.56792950
#> 14 Early vs Late      Dissimilarities          Bray-Curtis  0.11500812
#> 15 Early vs Late      Dissimilarities            Frobenius  0.15175689
#> 16 Early vs Late         Similarities               Cosine  0.97441228
#> 17 Early vs Late         Similarities              Jaccard  0.79370891
#> 18 Early vs Late         Similarities                 Dice  0.88499188
#> 19 Early vs Late         Similarities              Overlap  0.88499188
#> 20 Early vs Late         Similarities                   RV  0.96728262
#> 21 Early vs Late Pattern Similarities       Rank Agreement  0.87878788
#> 22 Early vs Late Pattern Similarities       Sign Agreement  0.99305556
```
