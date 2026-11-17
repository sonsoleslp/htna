# Plot the comparisons of a grouped network

[`plot()`](https://rdrr.io/r/graphics/plot.default.html) draws the plots
of a descriptive network comparison for every cohort pair of a grouped
comparison from
[`compare_htna()`](https://sonsoles.me/htna/reference/compare_htna.md):
a scatter plot of the edge weights of the two networks, a heatmap of the
edge differences, a histogram of the differences, or the weight
distributions. It returns, invisibly, a named list with the plot of
every cohort pair.

## Usage

``` r
# S3 method for class 'htna_comparison_group'
plot(x, ...)
```

## Arguments

- x:

  An `htna_comparison_group` from
  [`compare_htna()`](https://sonsoles.me/htna/reference/compare_htna.md).

- ...:

  Plot settings: `type` (`"scatter"`, `"heatmap"`, `"diff_hist"`,
  `"weight_dist"` or `"all"`) and `combined`.

## Value

Invisibly, a named list with the plot of every cohort pair.

## Conditions

An error of class `htna_empty_input` when `x` is empty.

## See also

[`compare_htna()`](https://sonsoles.me/htna/reference/compare_htna.md),
[`plot_htna_diff()`](https://sonsoles.me/htna/reference/plot_htna_diff.md)
for the difference network.

## Examples

``` r
data(human_ai)
grp <- build_htna(human_ai, actor_type = "actor_type", group = "phase")
#> Metadata aggregated per session: ties resolved by first occurrence in 'cluster' (24 sessions), 'actor_type' (9 sessions)
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (18 sessions), 'actor_type' (15 sessions)
plot(compare_htna(grp), type = "scatter")
```
