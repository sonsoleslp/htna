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
data(human_long, ai_long, package = "Nestimate")
net <- build_htna(list(Human = human_long, AI = ai_long))
#> Warning: A network with one long sequence is not recommended and can't be validated using bootstrap and other confirmatory testings.
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions)
frequencies_htna(net)
#>    group       state count proportion
#> 1     AI     Execute  3258 0.38100807
#> 2  Human     Specify  2920 0.27047054
#> 3     AI Investigate  2317 0.27096246
#> 4  Human     Command  2052 0.19007040
#> 5     AI        Plan  1620 0.18945153
#> 6  Human     Request  1052 0.09744350
#> 7  Human   Frustrate   939 0.08697666
#> 8  Human   Interrupt   890 0.08243794
#> 9  Human     Inquire   853 0.07901074
#> 10 Human     Correct   798 0.07391627
#> 11 Human      Refine   792 0.07336050
#> 12    AI     Explain   524 0.06127938
#> 13 Human      Verify   500 0.04631345
#> 14    AI    Delegate   295 0.03449889
#> 15    AI      Repair   257 0.03005496
#> 16    AI      Report   181 0.02116711
#> 17    AI         Ask    99 0.01157759
```
