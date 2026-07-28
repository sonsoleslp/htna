# Permutation Test for Network Differences

htna-named alias of
[`Nestimate::permutation()`](https://saqr.me/Nestimate/reference/permutation.html).
Tests whether observed edge-weight differences between two networks (or
all pairwise differences within a `netobject_group`) exceed what would
be expected under a null of identical generating processes.

## Usage

``` r
permutation_htna(x, y = NULL, ...)
```

## Arguments

- x, y:

  The two networks to compare, or a single `netobject_group` in `x`
  (with `y = NULL`) for all pairwise comparisons. See
  [`Nestimate::permutation()`](https://saqr.me/Nestimate/reference/permutation.html).

- ...:

  Additional arguments passed to
  [`Nestimate::permutation()`](https://saqr.me/Nestimate/reference/permutation.html),
  such as `iter`, `alpha`, `paired`, `adjust`, `measures`, `nlambda`,
  and `seed`. See that function for the full, current argument list.

## Value

An object of class `net_permutation` (single pair) or
`net_permutation_group` (multiple pairs). See
[`Nestimate::permutation()`](https://saqr.me/Nestimate/reference/permutation.html)
for the full slot list.

## Details

Works on htna networks: the actor partition (`$node_groups`,
`$actor_levels`, `htna` class) is preserved on `result$x` / `result$y`,
so
[`plot_htna_diff()`](https://sonsoles.me/htna/reference/plot_htna_diff.md)
can render the result with htna's colour and layout conventions.

Suffixed `_htna` to avoid clashing with
[`Nestimate::permutation()`](https://saqr.me/Nestimate/reference/permutation.html)
when both packages are loaded.

## See also

[`plot_htna_diff()`](https://sonsoles.me/htna/reference/plot_htna_diff.md)
to plot the result.

## Examples

``` r
# \donttest{
data(human_ai)
grp <- build_htna(human_ai, actor_type = "actor_type", group = "phase")
#> Warning: A network with one long sequence is not recommended and can't be validated using bootstrap and other confirmatory testings.
#> Metadata aggregated per session: ties resolved by first occurrence in 'cluster' (24 sessions), 'actor_type' (9 sessions)
#> Warning: A network with one long sequence is not recommended and can't be validated using bootstrap and other confirmatory testings.
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (18 sessions), 'actor_type' (15 sessions)
permutation_htna(grp$Early, grp$Late, iter = 50)
#> Permutation Test:Transition Network (relative probabilities) [directed]
#>   Iterations: 50  |  Alpha: 0.05
#>   Nodes: 12  |  Edges tested: 135  |  Significant: 24
# }
```
