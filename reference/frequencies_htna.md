# Count the codes of each actor in a network

`frequencies_htna()` counts the codes of each actor in an htna network:
how often each code occurs and how each actor distributes its activity
over its own codes, such as which requests a human makes most often and
which actions the AI takes. These counts describe a heterogeneous
network before its transitions are read.

`frequencies_htna()` counts the occurrences of every code in the
sessions of an htna network and reports each code under its actor type,
with its share of that actor's events. The proportions therefore sum to
one within each actor, not over all events. It runs Nestimate's
`state_distribution()` on the network and returns a data frame with one
row per actor and code.

[`plot_frequencies_htna()`](https://sonsoles.me/htna/reference/plot_frequencies_htna.md)
draws the table.
[`state_frequencies_htna()`](https://sonsoles.me/htna/reference/state_frequencies_htna.md)
gives the pooled table without actors, with shares of all events;
[`state_distribution_htna()`](https://sonsoles.me/htna/reference/state_distribution_htna.md)
returns the same table as this function.

## Usage

``` r
frequencies_htna(x)
```

## Arguments

- x:

  An htna network from
  [`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md), or
  an `htna_group`, in which case the codes are counted in every cohort.

## Value

A data frame with one row per actor and code, sorted by decreasing
count, and columns `group` (the actor type), `state`, `count` and
`proportion` (the code's share of its actor's events). For a grouped
network, one row per cohort, actor and code, with columns `group` (the
cohort), `actor` (the actor type), `state`, `count` and `proportion`
(the share of that actor's events in the cohort).

## Conditions

An error of class `htna_bad_input` when `x` is neither an htna network
nor an `htna_group`.

## See also

[`plot_frequencies_htna()`](https://sonsoles.me/htna/reference/plot_frequencies_htna.md),
[`state_frequencies_htna()`](https://sonsoles.me/htna/reference/state_frequencies_htna.md),
[`mosaic_plot_htna()`](https://sonsoles.me/htna/reference/mosaic_plot_htna.md).

## Examples

``` r
data(human_ai)
net <- build_htna(human_ai, actor_type = "actor_type")
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions), 'actor_type' (24 sessions)
frequencies_htna(net)
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

# one table for all cohorts
grp <- build_htna(human_ai, actor_type = "actor_type", group = "phase")
#> Metadata aggregated per session: ties resolved by first occurrence in 'cluster' (24 sessions), 'actor_type' (9 sessions)
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (18 sessions), 'actor_type' (15 sessions)
frequencies_htna(grp)
#>    group actor     state count proportion
#> 1  Early    AI   Execute  1849 0.40861878
#> 2  Early Human   Request  1606 0.28449956
#> 3  Early Human   Specify  1548 0.27422498
#> 4  Early    AI       Ask  1191 0.26320442
#> 5  Early Human Frustrate   889 0.15748450
#> 6  Early    AI      Plan   883 0.19513812
#> 7  Early Human     Check   763 0.13516386
#> 8  Early Human    Refine   424 0.07511072
#> 9  Early Human   Inquire   415 0.07351639
#> 10 Early    AI    Report   289 0.06386740
#> 11 Early    AI  Delegate   168 0.03712707
#> 12 Early    AI    Repair   145 0.03204420
#> 13  Late Human   Request  1498 0.29081732
#> 14  Late    AI   Execute  1409 0.34997516
#> 15  Late Human   Specify  1372 0.26635605
#> 16  Late    AI       Ask  1225 0.30427223
#> 17  Late Human Frustrate   940 0.18248884
#> 18  Late    AI      Plan   737 0.18306011
#> 19  Late Human     Check   535 0.10386333
#> 20  Late Human   Inquire   438 0.08503203
#> 21  Late    AI    Report   416 0.10332837
#> 22  Late Human    Refine   368 0.07144244
#> 23  Late    AI  Delegate   127 0.03154496
#> 24  Late    AI    Repair   112 0.02781918
```
