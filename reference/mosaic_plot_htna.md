# Chi-square Mosaic Plot of a Transition Network

htna-named alias of
[`Nestimate::mosaic_plot()`](https://rdrr.io/pkg/Nestimate/man/mosaic_plot.html).
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

  One of the four data-bearing Nestimate classes: `netobject` (single
  mosaic of `$weights`), `netobject_group` (one panel per group), `mcml`
  (between-cluster mosaic by default; per-cluster panels with
  `level = "within"`), or `htna` (single mosaic of `$weights`; htna
  inherits netobject so the geometry matches). Also accepts a
  contingency `table` or plain numeric `matrix` for ad-hoc plotting.

- ...:

  Ignored.

## Value

A ggplot or gtable, depending on input shape. See
[`Nestimate::mosaic_plot()`](https://rdrr.io/pkg/Nestimate/man/mosaic_plot.html)
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
[`Nestimate::mosaic_plot()`](https://rdrr.io/pkg/Nestimate/man/mosaic_plot.html)
when both packages are loaded.

## See also

[`plot_frequencies_htna()`](https://sonsoles.me/htna/reference/plot_frequencies_htna.md)
for the marginal-frequency companion view.

## Examples

``` r
# \donttest{
data(human_ai)
net <- build_htna(human_ai, actor_type = "actor_type",
                  method = "frequency")
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions), 'actor_type' (24 sessions)
mosaic_plot_htna(net, n_perm = 50, seed = 1)
#> Warning: Vectorized input to `element_text()` is not officially supported.
#> ℹ Results may be unexpected or may change in future versions of ggplot2.
#> Warning: Vectorized input to `element_text()` is not officially supported.
#> ℹ Results may be unexpected or may change in future versions of ggplot2.

# }
```
