# Tidy State Frequency Table

htna-named alias of
[`Nestimate::state_frequencies()`](https://saqr.me/Nestimate/reference/state_frequencies.html).
Returns a tidy data frame with `state`, `count`, and `proportion`
columns summarising the within-network state vocabulary.

## Usage

``` r
state_frequencies_htna(data)
```

## Arguments

- data:

  A list of character vectors (trajectories) or a data.frame.

## Value

A data frame. See
[`Nestimate::state_frequencies()`](https://saqr.me/Nestimate/reference/state_frequencies.html)
for details.

## Details

Companion to
[`plot_frequencies_htna()`](https://sonsoles.me/htna/reference/plot_frequencies_htna.md)
(which is htna-aware via an explicit S3 method).
`state_frequencies_htna()` itself operates on the raw sequence data and
is the data side of the same family used by the htna tutorials.

Suffixed `_htna` to avoid clashing with
[`Nestimate::state_frequencies()`](https://saqr.me/Nestimate/reference/state_frequencies.html)
when both packages are loaded.

## See also

[`plot_frequencies_htna()`](https://sonsoles.me/htna/reference/plot_frequencies_htna.md),
[`state_distribution_htna()`](https://sonsoles.me/htna/reference/state_distribution_htna.md).

## Examples

``` r
data(human_long, ai_long, package = "Nestimate")
net <- build_htna(list(Human = human_long, AI = ai_long))
#> Warning: A network with one long sequence is not recommended and can't be validated using bootstrap and other confirmatory testings.
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions)
state_frequencies_htna(net$data)
#>          state count proportion
#> 1      Execute  3258     0.1684
#> 2      Specify  2920     0.1509
#> 3  Investigate  2317     0.1198
#> 4      Command  2052     0.1061
#> 5         Plan  1620     0.0837
#> 6      Request  1052     0.0544
#> 7    Frustrate   939     0.0485
#> 8    Interrupt   890     0.0460
#> 9      Inquire   853     0.0441
#> 10     Correct   798     0.0412
#> 11      Refine   792     0.0409
#> 12     Explain   524     0.0271
#> 13      Verify   500     0.0258
#> 14    Delegate   295     0.0152
#> 15      Repair   257     0.0133
#> 16      Report   181     0.0094
#> 17         Ask    99     0.0051
```
