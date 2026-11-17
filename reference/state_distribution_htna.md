# Tabulate the distribution of the codes within each actor

`state_distribution_htna()` returns the state distribution of a network:
how often each code occurs and its share of the events of its group. For
an htna network the group is the actor type, so the table shows how each
actor divides its activity over its own codes; for a network without an
actor partition all codes form one group, `"all"`.

`state_distribution_htna()` runs Nestimate's `state_distribution()` and
returns a data frame with one row per group and code. For an htna
network it is the same table as
[`frequencies_htna()`](https://sonsoles.me/htna/reference/frequencies_htna.md);
the function is provided under its general name for code written for
other network types.

## Usage

``` r
state_distribution_htna(x, ...)
```

## Arguments

- x:

  An htna network from
  [`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md), or
  another network.

- ...:

  Unused for htna networks.

## Value

A data frame with one row per group and code and columns `group`,
`state`, `count` and `proportion` (proportions sum to one within each
group).

## See also

[`frequencies_htna()`](https://sonsoles.me/htna/reference/frequencies_htna.md),
[`state_frequencies_htna()`](https://sonsoles.me/htna/reference/state_frequencies_htna.md)
for the pooled table.

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
