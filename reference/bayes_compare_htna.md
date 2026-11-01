# Bayesian Comparison of Two Networks

htna-named wrapper for
[`Nestimate::bayes_compare()`](https://pak.dynasite.org/Nestimate/reference/bayes_compare.html).
The Bayesian complement of
[`permutation_htna()`](https://sonsoles.me/htna/reference/permutation_htna.md):
instead of a permutation null, it places a prior on the transition
structure and returns the posterior distribution of edge-weight
differences between two networks.

## Usage

``` r
bayes_compare_htna(x, y = NULL, ...)
```

## Arguments

- x, y:

  The two networks to compare, or a grouped network (`htna_group`) in
  `x` with `y = NULL`, in which case every cohort pair is compared. See
  [`Nestimate::bayes_compare()`](https://pak.dynasite.org/Nestimate/reference/bayes_compare.html).

- ...:

  Additional arguments passed to
  [`Nestimate::bayes_compare()`](https://pak.dynasite.org/Nestimate/reference/bayes_compare.html),
  such as `prior`, `draws`, `ci`, `mean_threshold`, `bound_threshold`,
  and `seed`. See that function for the full, current argument list.

## Value

For two networks, an object of class
`c("net_bayes", "netdifference", "net_permutation")`. For a grouped
network, a named list of class `c("net_bayes_group", "list")` with one
such object per cohort pair.
[`print()`](https://rdrr.io/r/base/print.html) and
[`summary()`](https://rdrr.io/r/base/summary.html) methods are supplied
by Nestimate; see
[`Nestimate::bayes_compare()`](https://pak.dynasite.org/Nestimate/reference/bayes_compare.html)
for the full slot list.

## Details

Works on htna networks: the actor partition (`$node_groups`,
`$actor_levels`, `htna` class) is preserved on `result$x` / `result$y`,
and the result carries class `net_permutation`, so
[`plot_htna_diff()`](https://sonsoles.me/htna/reference/plot_htna_diff.md)
can render it with htna's colour and layout conventions, exactly like a
[`permutation_htna()`](https://sonsoles.me/htna/reference/permutation_htna.md)
result.

Suffixed `_htna` to avoid clashing with the tna verb `compare()` and to
sit alongside
[`permutation_htna()`](https://sonsoles.me/htna/reference/permutation_htna.md)
in the htna API.

## See also

[`permutation_htna()`](https://sonsoles.me/htna/reference/permutation_htna.md)
for the permutation-test analogue,
[`plot_htna_diff()`](https://sonsoles.me/htna/reference/plot_htna_diff.md)
to plot the result.

## Examples

``` r
data(human_ai)
grp <- build_htna(human_ai, actor_type = "actor_type", group = "phase")
#> A network with one long sequence is not recommended and can't be validated using bootstrap and other confirmatory testings.
#> Metadata aggregated per session: ties resolved by first occurrence in 'cluster' (24 sessions), 'actor_type' (9 sessions)
#> A network with one long sequence is not recommended and can't be validated using bootstrap and other confirmatory testings.
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (18 sessions), 'actor_type' (15 sessions)
# Pass the grouped network as it is; all pairwise comparisons are returned.
bayes_compare_htna(grp, draws = 200, seed = 1)
#> Grouped Bayesian Dirichlet-Multinomial Comparison
#> Comparisons: Late vs Early 
#> Use summary() for combined edge-level results.
```
