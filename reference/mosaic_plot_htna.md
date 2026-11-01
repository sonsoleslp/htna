# Chi-square Mosaic Plot of a Transition Network

htna-named wrapper for
[`Nestimate::mosaic_plot()`](https://pak.dynasite.org/Nestimate/reference/mosaic_plot.html).
Renders a chi-square mosaic where row x column area equals the joint
share of `(from, to)` transitions and fill encodes the standardized
residual (blue = over-represented, red = under-represented, white =
at-expected).

## Usage

``` r
mosaic_plot_htna(x, ...)
```

## Arguments

- x:

  An htna network built with `method = "frequency"` (or other object
  accepted by
  [`Nestimate::mosaic_plot()`](https://pak.dynasite.org/Nestimate/reference/mosaic_plot.html));
  S3 dispatch on `x` is preserved, so `htna` objects use
  `mosaic_plot.htna`.

- ...:

  Additional arguments passed to
  [`Nestimate::mosaic_plot()`](https://pak.dynasite.org/Nestimate/reference/mosaic_plot.html),
  such as `n_perm` and `seed`. See that function for details.

## Value

A ggplot or gtable, depending on input shape. See
[`Nestimate::mosaic_plot()`](https://pak.dynasite.org/Nestimate/reference/mosaic_plot.html)
for full details and the per-class S3 methods.

## Details

Designed with htna in mind: Nestimate ships an explicit
`mosaic_plot.htna` S3 method that recognises the actor partition on
`htna` networks. Pass an htna network from
[`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md)
directly. The underlying transition matrix must be integer-weighted
(build with `method = "frequency"`), since the chi-square test needs
counts.

Axis tick labels are coloured by actor group using the htna palette
([htna_palette](https://sonsoles.me/htna/reference/htna_palette.md)),
matching the colours used elsewhere in htna (e.g.
[`plot_htna()`](https://sonsoles.me/htna/reference/plot_htna.md)).

Suffixed `_htna` to avoid clashing with
[`Nestimate::mosaic_plot()`](https://pak.dynasite.org/Nestimate/reference/mosaic_plot.html)
when both packages are loaded.

## See also

[`plot_frequencies_htna()`](https://sonsoles.me/htna/reference/plot_frequencies_htna.md)
for the marginal-frequency companion view.

## Examples

``` r
data(human_ai)
net <- build_htna(human_ai, actor_type = "actor_type",
                  method = "frequency")
#> A network with one long sequence is not recommended and can't be validated using bootstrap and other confirmatory testings.
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions), 'actor_type' (24 sessions)
mosaic_plot_htna(net, n_perm = 50, seed = 1)
#> Warning: Vectorized input to `element_text()` is not officially supported.
#> ℹ Results may be unexpected or may change in future versions of ggplot2.
#> Warning: Vectorized input to `element_text()` is not officially supported.
#> ℹ Results may be unexpected or may change in future versions of ggplot2.
```
