# Tidy State Frequency Table

htna-named wrapper for
[`Nestimate::state_frequencies()`](https://pak.dynasite.org/Nestimate/reference/state_frequencies.html).
Returns a tidy data frame with `state`, `count`, and `proportion`
columns summarising the within-network state vocabulary, pooled across
actors.

## Usage

``` r
state_frequencies_htna(data, ...)
```

## Arguments

- data:

  An htna network from
  [`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md)
  (its session sequences are counted), or raw sequence data. See
  [`Nestimate::state_frequencies()`](https://pak.dynasite.org/Nestimate/reference/state_frequencies.html).

- ...:

  Additional arguments passed to
  [`Nestimate::state_frequencies()`](https://pak.dynasite.org/Nestimate/reference/state_frequencies.html).
  See that function for details.

## Value

A data frame with one row per state, sorted by decreasing count, and
columns `state`, `count` and `proportion`. See
[`Nestimate::state_frequencies()`](https://pak.dynasite.org/Nestimate/reference/state_frequencies.html).

## Details

Companion to
[`plot_frequencies_htna()`](https://sonsoles.me/htna/reference/plot_frequencies_htna.md).
Unlike
[`frequencies_htna()`](https://sonsoles.me/htna/reference/frequencies_htna.md),
the table carries no actor column; use
[`frequencies_htna()`](https://sonsoles.me/htna/reference/frequencies_htna.md)
when each state should be reported under its actor type.

Suffixed `_htna` to avoid clashing with
[`Nestimate::state_frequencies()`](https://pak.dynasite.org/Nestimate/reference/state_frequencies.html)
when both packages are loaded.

## See also

[`plot_frequencies_htna()`](https://sonsoles.me/htna/reference/plot_frequencies_htna.md),
[`state_distribution_htna()`](https://sonsoles.me/htna/reference/state_distribution_htna.md).

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
