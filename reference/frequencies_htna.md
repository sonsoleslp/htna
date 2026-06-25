# Tidy Per-Actor Frequency Table for an htna Network

Returns the within-network state frequency table, partitioned by actor.
One row per `(actor, state)` pair, with `count` and `proportion`
columns. The data side of
[`plot_frequencies_htna()`](https://sonsoles.me/htna/reference/plot_frequencies_htna.md).

## Usage

``` r
frequencies_htna(x)
```

## Arguments

- x:

  An htna network from
  [`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md).

## Value

A data frame with columns `group` (actor), `state`, `count`,
`proportion`.

## Details

Internally a thin wrapper around the htna S3 method shipped by Nestimate
(`state_distribution.htna`); exposed under the `frequencies_htna()` name
as the canonical "give me the frequency table for this network" entry
point.

Suffixed `_htna` to avoid clashing with
[`Nestimate::frequencies()`](https://saqr.me/Nestimate/reference/frequencies.html)
(which takes raw long-format data and a column-name spec) when both
packages are loaded. If you have raw data rather than an htna network,
call
[`Nestimate::frequencies()`](https://saqr.me/Nestimate/reference/frequencies.html)
directly.

## See also

[`plot_frequencies_htna()`](https://sonsoles.me/htna/reference/plot_frequencies_htna.md)
for the rendered version,
[`state_distribution_htna()`](https://sonsoles.me/htna/reference/state_distribution_htna.md)
(same function under the upstream name),
[`mosaic_plot_htna()`](https://sonsoles.me/htna/reference/mosaic_plot_htna.md).

## Examples

``` r
# \donttest{
data(human_ai)
net <- build_htna(human_ai, actor_type = "actor_type")
#> Warning: A network with one long sequence is not recommended and can't be validated using bootstrap and other confirmatory testings.
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
# }
```
