# Compare Heterogeneous Transition Networks

Computes the descriptive network comparison implemented by
[`Nestimate::compare_model()`](https://saqr.me/Nestimate/reference/compare_model.html)
while retaining the HTNA actor partition. Comparing two individual
networks returns one `htna_comparison`. Passing an `htna_group` as `x`
with `y = NULL` compares every cohort pair and returns an
`htna_comparison_group`; callers do not need to select or subset
cohorts.

## Usage

``` r
compare_htna(x, y = NULL, ...)
```

## Arguments

- x:

  An `htna` network or an `htna_group`.

- y:

  A second `htna` network. Must be `NULL` when `x` is an `htna_group`,
  in which case all cohort pairs are compared.

- ...:

  Additional arguments passed to
  [`Nestimate::compare_model()`](https://saqr.me/Nestimate/reference/compare_model.html),
  including `scaling`, `measures`, and `network`.

## Value

For two networks, an object inheriting from `htna_comparison` and
`net_comparison`. In addition to the Nestimate comparison fields it
contains:

- `node_groups` and `actor_levels`: the aligned actor partition;

- `edge_metrics$source_actor` and `edge_metrics$target_actor`;

- `actor_pair_metrics`: difference summaries for every directed
  actor-type block;

- `models`: the two original, unmodified HTNA networks.

For an `htna_group`, a named `htna_comparison_group` containing every
pairwise comparison is returned.

## Details

Networks may contain different subsets of the declared nodes. Their
weight matrices are aligned to the union of nodes, in canonical actor
order, and missing nodes are represented by zero rows and columns.
Shared nodes must have the same actor assignment in both networks.

## See also

[`Nestimate::compare_model()`](https://saqr.me/Nestimate/reference/compare_model.html),
[`plot_htna_diff()`](https://sonsoles.me/htna/reference/plot_htna_diff.md)

## Examples

``` r
data(human_ai)
grouped <- build_htna(human_ai, actor_type = "actor_type", group = "phase")
#> Warning: A network with one long sequence is not recommended and can't be validated using bootstrap and other confirmatory testings.
#> Metadata aggregated per session: ties resolved by first occurrence in 'cluster' (24 sessions), 'actor_type' (9 sessions)
#> Warning: A network with one long sequence is not recommended and can't be validated using bootstrap and other confirmatory testings.
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (18 sessions), 'actor_type' (15 sessions)
comparisons <- compare_htna(grouped)
names(comparisons)
#> [1] "Late vs Early"
```
