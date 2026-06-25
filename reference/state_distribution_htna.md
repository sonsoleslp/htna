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
# \donttest{
data(human_ai)
net <- build_htna(human_ai, actor_type = "actor_type")
#> Warning: A network with one long sequence is not recommended and can't be validated using bootstrap and other confirmatory testings.
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
# }
```
