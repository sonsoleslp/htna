# State Distribution Across Time

htna-named alias of
[`Nestimate::state_distribution()`](https://saqr.me/Nestimate/reference/state_distribution.html).
Returns the per-timestep distribution of states across sequences,
suitable for driving stacked-area or bar plots.

## Usage

``` r
state_distribution_htna(x, ...)
```

## Arguments

- x:

  A `netobject`, `netobject_group`, `mcml`, or `htna` object.

- ...:

  Currently unused.

## Value

A data frame with one row per (timestep, state). See
[`Nestimate::state_distribution()`](https://saqr.me/Nestimate/reference/state_distribution.html)
for full details.

## Details

Designed with htna in mind: Nestimate ships an explicit
`state_distribution.htna` S3 method that uses the actor partition
carried by
[`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md)
networks.

Suffixed `_htna` to avoid clashing with
[`Nestimate::state_distribution()`](https://saqr.me/Nestimate/reference/state_distribution.html)
when both packages are loaded.

## See also

[`state_frequencies_htna()`](https://sonsoles.me/htna/reference/state_frequencies_htna.md)
for the within-network summary.

## Examples

``` r
data(human_long, ai_long, package = "Nestimate")
net <- build_htna(list(Human = human_long, AI = ai_long))
#> Warning: A network with one long sequence is not recommended and can't be validated using bootstrap and other confirmatory testings.
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions)
state_distribution_htna(net)
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
