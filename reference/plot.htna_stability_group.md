# Plot Method for htna Stability Groups

S3 method for plotting grouped htna stability results. Draws the
[`plot.htna_stability()`](https://sonsoles.me/htna/reference/plot.htna_stability.md)
chart once per cohort, titled with the cohort name. Requires the
'ggplot2' package (Suggests).

## Usage

``` r
# S3 method for class 'htna_stability_group'
plot(x, ...)
```

## Arguments

- x:

  An object of class `htna_stability_group` from
  [`centrality_stability_htna()`](https://sonsoles.me/htna/reference/centrality_stability_htna.md).

- ...:

  Passed to
  [`plot.htna_stability()`](https://sonsoles.me/htna/reference/plot.htna_stability.md)
  (currently unused there).

## Value

For a single cohort, that cohort's `ggplot` object. Otherwise a named
list of `ggplot` objects, one per cohort. Each cohort's chart is printed
as a side effect.

## Examples

``` r
data(human_ai)
grp <- build_htna(human_ai, actor_type = "actor_type", group = "phase")
#> A network with one long sequence is not recommended and can't be validated using bootstrap and other confirmatory testings.
#> Metadata aggregated per session: ties resolved by first occurrence in 'cluster' (24 sessions), 'actor_type' (9 sessions)
#> A network with one long sequence is not recommended and can't be validated using bootstrap and other confirmatory testings.
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (18 sessions), 'actor_type' (15 sessions)
cs  <- centrality_stability_htna(grp, iter = 20, seed = 1)
if (requireNamespace("ggplot2", quietly = TRUE)) {
  plot(cs)
}


#> $Late

#> 
#> $Early

#> 
```
