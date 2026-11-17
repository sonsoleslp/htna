# Test differences between networks by permutation

`permutation_htna()` tests by permutation whether two heterogeneous
transition networks, for example of two groups of learners or two phases
of a study, differ more than chance would produce. The sessions behind
both networks are pooled, the group labels are shuffled many times, both
networks are re-estimated on every shuffle, and each observed difference
is compared with the shuffled ones. Each edge gets a p-value and an
effect size (the observed difference divided by the standard deviation
of the shuffled differences), and the networks as a whole are tested
with the sum and the maximum of the absolute differences. Because whole
sessions are shuffled, the nesting of transitions in sessions is
respected.

`permutation_htna()` runs Nestimate's `permutation()` with the actor
partition kept. It tests two htna networks estimated with the same
method on the same codes, or every pair of cohorts of a grouped network.
With many edges some fall below `alpha` by chance; set `adjust` (for
example `"BH"`) to correct for multiple testing. With `measures`, node
centralities are tested as well. It returns a `net_permutation` object,
or one per cohort pair for a grouped network.

A small p-value says that a difference is larger than the shuffles
produce, not how large or how probable it is, which
[`bayes_compare_htna()`](https://sonsoles.me/htna/reference/bayes_compare_htna.md)
estimates.
[`compare_htna()`](https://sonsoles.me/htna/reference/compare_htna.md)
gives descriptive differences per actor pair, and
[`plot_htna_diff()`](https://sonsoles.me/htna/reference/plot_htna_diff.md)
draws the result.

## Usage

``` r
permutation_htna(x, y = NULL, ...)
```

## Arguments

- x, y:

  Two htna networks, or an `htna_group` in `x` with `y = NULL`, in which
  case every pair of cohorts is tested.

- ...:

  Settings: `iter` (shuffles, default 1000), `alpha` (0.05), `paired`
  (`FALSE`), `adjust` (multiplicity correction, `"none"`), `measures`
  (centralities to test), `nlambda`, `seed` and `actor` (shuffle whole
  actors).

## Value

For two networks, an object of class `net_permutation`; for a grouped
network, a named list of class `c("net_permutation_group", "list")` with
one result per cohort pair (for example `"Early vs Late"`). Both have
[`print()`](https://rdrr.io/r/base/print.html) and
[`summary()`](https://rdrr.io/r/base/summary.html) methods;
[`summary()`](https://rdrr.io/r/base/summary.html) returns a data frame
with one row per edge (and pair) and columns `from`, `to`, `weight_x`,
`weight_y`, `diff`, `effect_size`, `p_value` and `sig`. Differences are
`x - y`.

## See also

[`bayes_compare_htna()`](https://sonsoles.me/htna/reference/bayes_compare_htna.md),
[`compare_htna()`](https://sonsoles.me/htna/reference/compare_htna.md),
[`plot_htna_diff()`](https://sonsoles.me/htna/reference/plot_htna_diff.md).

## Examples

``` r
data(human_ai)
grp <- build_htna(human_ai, actor_type = "actor_type", group = "phase")
#> Metadata aggregated per session: ties resolved by first occurrence in 'cluster' (24 sessions), 'actor_type' (9 sessions)
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (18 sessions), 'actor_type' (15 sessions)
perm <- permutation_htna(grp, iter = 50, seed = 1)
perm
#> Grouped Permutation Test
#> Groups: Early vs Late 
#> 
#> -- Early vs Late --
#> Permutation Test: Transition Network (relative probabilities) [directed]
#>   Iterations: 50  |  Alpha: 0.05
#>   Nodes: 12  |  Edges tested: 135  |  Significant: 25
#>   Global test (networks differ overall?): M = 2.760 (p = 0.0196)  |  S = 0.181 (p = 0.0196)
head(summary(perm))
#>           group from        to   weight_x   weight_y          diff effect_size
#> 1 Early vs Late  Ask       Ask 0.01827676 0.01819686  0.0000799055  0.01280436
#> 2 Early vs Late  Ask     Check 0.06875544 0.05789909  0.0108563494  1.12598665
#> 3 Early vs Late  Ask   Execute 0.03307224 0.01157982  0.0214924187  2.62829072
#> 4 Early vs Late  Ask Frustrate 0.09921671 0.13151365 -0.0322969375 -2.23575725
#> 5 Early vs Late  Ask   Inquire 0.05395997 0.07196030 -0.0180003326 -1.53373385
#> 6 Early vs Late  Ask      Plan 0.43342037 0.38544251  0.0479778511  1.59966614
#>      p_value   sig
#> 1 1.00000000 FALSE
#> 2 0.27450980 FALSE
#> 3 0.03921569  TRUE
#> 4 0.03921569  TRUE
#> 5 0.11764706 FALSE
#> 6 0.13725490 FALSE
```
