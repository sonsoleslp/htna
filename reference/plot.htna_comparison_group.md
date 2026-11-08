# Plot All Comparisons in an HTNA Comparison Group

Applies Nestimate's `net_comparison` plot method to every cohort pair.

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

  Passed to the `net_comparison` plot method, including `type`
  (`"scatter"`, `"heatmap"`, `"diff_hist"`, `"weight_dist"` or `"all"`)
  and `combined`.

## Value

Invisibly, a named list containing the plot for every comparison.

## Examples

``` r
data(human_ai)
grouped <- build_htna(human_ai, actor_type = "actor_type", group = "phase")
#> Metadata aggregated per session: ties resolved by first occurrence in 'cluster' (24 sessions), 'actor_type' (9 sessions)
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (18 sessions), 'actor_type' (15 sessions)
plot(compare_htna(grouped))
```
