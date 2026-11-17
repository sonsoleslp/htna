# Describe the differences between heterogeneous transition networks

`compare_htna()` describes the differences between two heterogeneous
transition networks, or between every pair of cohorts of a grouped
network: overall, in how similar their weight matrices are, and in where
the differences lie in the actor structure, within an actor (for
example, how humans move between their own codes) or between actors (how
the human hands over to the AI and back). It reports correlation,
distance and similarity measures of the two weight matrices, the
network-level metrics of each network, the difference of every edge, and
a summary of the differences in each directed actor-pair block.

Networks that contain different subsets of the codes are aligned on the
union of their codes, in actor order, with the missing codes treated as
having no transitions; a code shared by both networks must belong to the
same actor in both. Differences are `x - y`. `compare_htna()` runs
Nestimate's `compare_model()` on the aligned networks and adds the
actor-pair summary. It returns an `htna_comparison` object (an
`htna_comparison_group` for a grouped network).

The comparison is descriptive and gives no test. Use
[`permutation_htna()`](https://sonsoles.me/htna/reference/permutation_htna.md)
for a permutation test of the differences and
[`bayes_compare_htna()`](https://sonsoles.me/htna/reference/bayes_compare_htna.md)
for credible intervals;
[`plot_htna_diff()`](https://sonsoles.me/htna/reference/plot_htna_diff.md)
draws the difference network. The tables of the result are returned by
[`summary()`](https://rdrr.io/r/base/summary.html) and
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) (see
[htna_comparison-methods](https://sonsoles.me/htna/reference/htna_comparison-methods.md)).

## Usage

``` r
compare_htna(x, y = NULL, ...)
```

## Arguments

- x:

  An htna network, or an `htna_group`.

- y:

  A second htna network; `NULL` (default) when `x` is an `htna_group`,
  in which case every pair of cohorts is compared.

- ...:

  Settings of the comparison: `scaling`, `measures` and `network`.

## Value

For two networks, an object of class
`c("htna_comparison", "net_comparison")`. For an `htna_group`, a named
list of class `c("htna_comparison_group", "list")` with one comparison
per cohort pair (for example `"Early vs Late"`).
[`summary()`](https://rdrr.io/r/base/summary.html) returns the
actor-pair table, one row per directed actor pair;
`as.data.frame(x, what = )` returns the actor-pair, edge, metric or
network table (see
[htna_comparison-methods](https://sonsoles.me/htna/reference/htna_comparison-methods.md)).

## Conditions

Errors of class `htna_bad_input` when `x` and `y` are not htna networks,
when `y` is given with an `htna_group`, or when the networks use
different actor types; `htna_empty_input` when a group has fewer than
two cohorts; `htna_actor_conflict` when a shared code belongs to
different actors.

## See also

[htna_comparison-methods](https://sonsoles.me/htna/reference/htna_comparison-methods.md),
[`permutation_htna()`](https://sonsoles.me/htna/reference/permutation_htna.md),
[`bayes_compare_htna()`](https://sonsoles.me/htna/reference/bayes_compare_htna.md),
[`plot_htna_diff()`](https://sonsoles.me/htna/reference/plot_htna_diff.md).

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
```
