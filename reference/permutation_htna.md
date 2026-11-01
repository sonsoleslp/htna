# Permutation Test for Network Differences

htna-named wrapper for
[`Nestimate::permutation()`](https://pak.dynasite.org/Nestimate/reference/permutation.html).
Tests whether observed edge-weight differences between two networks (or
all pairwise differences within a `netobject_group`) exceed what would
be expected under a null of identical generating processes.

## Usage

``` r
permutation_htna(x, y = NULL, ...)
```

## Arguments

- x, y:

  The two networks to compare, or a grouped network (`htna_group`, which
  is also a `netobject_group`) in `x` with `y = NULL` for all pairwise
  cohort comparisons. See
  [`Nestimate::permutation()`](https://pak.dynasite.org/Nestimate/reference/permutation.html).

- ...:

  Additional arguments passed to
  [`Nestimate::permutation()`](https://pak.dynasite.org/Nestimate/reference/permutation.html),
  such as `iter`, `alpha`, `paired`, `adjust`, `measures`, `nlambda`,
  `seed`, and `actor`. See that function for the full, current argument
  list.

## Value

An object of class `net_permutation` (single pair) or a named list of
class `c("net_permutation_group", "list")` with one `net_permutation`
per cohort pair. Both render with
[`plot_htna_diff()`](https://sonsoles.me/htna/reference/plot_htna_diff.md).
See
[`Nestimate::permutation()`](https://pak.dynasite.org/Nestimate/reference/permutation.html)
for the full slot list.

## Details

Works on htna networks: the actor partition (`$node_groups`,
`$actor_levels`, `htna` class) is preserved on `result$x` / `result$y`,
so
[`plot_htna_diff()`](https://sonsoles.me/htna/reference/plot_htna_diff.md)
can render the result with htna's colour and layout conventions.

Suffixed `_htna` to avoid clashing with
[`Nestimate::permutation()`](https://pak.dynasite.org/Nestimate/reference/permutation.html)
when both packages are loaded.

## See also

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
# Pass the grouped network as it is; every cohort pair is tested.
permutation_htna(grp, iter = 50, seed = 1)
#> Grouped Permutation Test
#> Groups: Late vs Early 
#> 
#> -- Late vs Early --
#> Permutation Test: Transition Network (relative probabilities) [directed]
#>   Iterations: 50  |  Alpha: 0.05
#>   Nodes: 12  |  Edges tested: 135  |  Significant: 28
#>   Global test (networks differ overall?): M = 2.760 (p = 0.0196)  |  S = 0.181 (p = 0.0196)
```
