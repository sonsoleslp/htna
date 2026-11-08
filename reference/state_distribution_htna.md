# State Distribution Across Time

htna-named wrapper for
[`Nestimate::state_distribution()`](https://pak.dynasite.org/Nestimate/reference/state_distribution.html).
Returns the within-network distribution of states: how often each state
occurs and its share of the events in its group.

## Usage

``` r
state_distribution_htna(x, ...)
```

## Arguments

- x:

  An htna network (or other object accepted by
  [`Nestimate::state_distribution()`](https://pak.dynasite.org/Nestimate/reference/state_distribution.html));
  S3 dispatch on `x` is preserved, so `htna` objects use
  `state_distribution.htna`.

- ...:

  Additional arguments passed to
  [`Nestimate::state_distribution()`](https://pak.dynasite.org/Nestimate/reference/state_distribution.html).
  See that function for details.

## Value

A data frame with one row per `(group, state)` pair and columns `group`,
`state`, `count` and `proportion` (proportions sum to 1 within each
group). For an htna network `group` is the actor type; for a network
without an actor partition it is `"all"`. See
[`Nestimate::state_distribution()`](https://pak.dynasite.org/Nestimate/reference/state_distribution.html).

## Details

Designed with htna in mind: Nestimate ships an explicit
`state_distribution.htna` S3 method that uses the actor partition
carried by
[`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md)
networks, so each state is reported under its actor type. For an htna
network the result is the same table as
[`frequencies_htna()`](https://sonsoles.me/htna/reference/frequencies_htna.md).

Suffixed `_htna` to avoid clashing with
[`Nestimate::state_distribution()`](https://pak.dynasite.org/Nestimate/reference/state_distribution.html)
when both packages are loaded.

## See also

[`frequencies_htna()`](https://sonsoles.me/htna/reference/frequencies_htna.md),
[`state_frequencies_htna()`](https://sonsoles.me/htna/reference/state_frequencies_htna.md)
for the pooled (actor-blind) frequency table.

## Examples

``` r
data(human_ai)
net <- build_htna(human_ai, actor_type = "actor_type")
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions), 'actor_type' (24 sessions)
state_distribution_htna(net)
#>    group     state count proportion
#> 1     AI   Execute  3258 0.38100807
#> 2  Human   Request  3104 0.28751389
#> 3  Human   Specify  2920 0.27047054
#> 4     AI       Ask  2416 0.28254005
#> 5  Human Frustrate  1829 0.16941460
#> 6     AI      Plan  1620 0.18945153
#> 7  Human     Check  1298 0.12022971
#> 8  Human   Inquire   853 0.07901074
#> 9  Human    Refine   792 0.07336050
#> 10    AI    Report   705 0.08244650
#> 11    AI  Delegate   295 0.03449889
#> 12    AI    Repair   257 0.03005496
```
