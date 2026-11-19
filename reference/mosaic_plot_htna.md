# Plot a chi-square mosaic of the transitions of an htna network

`mosaic_plot_htna()` plots a chi-square mosaic of the transitions of an
htna network. A mosaic plot of a transition table shows, for every pair
of codes, the share of all transitions that go from the first code to
the second (the area of the tile) and how far that count lies from what
independence of the source and target codes would predict (the colour of
the tile, from the standardized residual of a chi-square test).
Over-represented transitions are shaded blue and under-represented ones
red. In a heterogeneous network the plot shows which hand-overs between
actors are more or less frequent than the overall frequencies of the
codes imply.

`mosaic_plot_htna()` draws the mosaic of the transition counts of an
htna network with Nestimate's `mosaic_plot()`, with the axis labels
coloured by actor type. The chi-square test needs counts, so build the
network with `method = "frequency"`. With `n_perm`, p-values are
obtained by permutation (set `seed` for a reproducible result). It
returns a `ggplot` object.

For the marginal frequencies of the codes use
[`plot_frequencies_htna()`](https://sonsoles.me/htna/reference/plot_frequencies_htna.md).

## Usage

``` r
mosaic_plot_htna(x, ...)
```

## Arguments

- x:

  An htna network built with `method = "frequency"`.

- ...:

  Further arguments of
  [`Nestimate::mosaic_plot()`](https://pak.dynasite.org/Nestimate/reference/mosaic_plot.html),
  such as `n_perm` and `seed`. With Nestimate 1.0.13 or later,
  `nested = TRUE` draws residuals that allow for transitions nested in
  sessions, or in the actors named by `actor`; `nested_method` chooses
  the test and `report = TRUE` adds the overall test as the subtitle.

## Value

A `ggplot` object (or a `gtable` for some inputs), which is drawn when
printed.

## See also

[`plot_frequencies_htna()`](https://sonsoles.me/htna/reference/plot_frequencies_htna.md),
[`frequencies_htna()`](https://sonsoles.me/htna/reference/frequencies_htna.md),
[`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md)

## Examples

``` r
data(human_ai)
net <- build_htna(human_ai, actor_type = "actor_type",
                  method = "frequency")
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions), 'actor_type' (24 sessions)
mosaic_plot_htna(net, n_perm = 50, seed = 1)
```
