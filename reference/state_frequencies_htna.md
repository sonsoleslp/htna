# Count the codes of a network over all actors

`state_frequencies_htna()` counts the codes of a network over all
actors: every code in the sessions of an htna network, or in raw
sequence data, in decreasing order of frequency. The pooled table shows
which codes dominate the interaction as a whole, whichever actor
produces them. Each code's proportion is its share of all events, so the
shares of the different actors can be compared directly.

`state_frequencies_htna()` runs Nestimate's `state_frequencies()` and
returns a data frame with one row per code.

The table has no actor column;
[`frequencies_htna()`](https://sonsoles.me/htna/reference/frequencies_htna.md)
reports each code under its actor, with shares within the actor.

## Usage

``` r
state_frequencies_htna(data, ...)
```

## Arguments

- data:

  An htna network from
  [`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md), or
  sequence data (a wide data frame or a list of character vectors).

- ...:

  Unused.

## Value

A data frame with one row per code, sorted by decreasing count, and
columns `state`, `count` and `proportion` (the share of all events,
rounded to four decimals).

## See also

[`frequencies_htna()`](https://sonsoles.me/htna/reference/frequencies_htna.md),
[`plot_frequencies_htna()`](https://sonsoles.me/htna/reference/plot_frequencies_htna.md).

## Examples

``` r
data(human_ai)
net <- build_htna(human_ai, actor_type = "actor_type")
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions), 'actor_type' (24 sessions)
state_frequencies_htna(net)
#>        state count proportion
#> 1    Execute  3258     0.1684
#> 2    Request  3104     0.1604
#> 3    Specify  2920     0.1509
#> 4        Ask  2416     0.1249
#> 5  Frustrate  1829     0.0945
#> 6       Plan  1620     0.0837
#> 7      Check  1298     0.0671
#> 8    Inquire   853     0.0441
#> 9     Refine   792     0.0409
#> 10    Report   705     0.0364
#> 11  Delegate   295     0.0152
#> 12    Repair   257     0.0133
```
