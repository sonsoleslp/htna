# Plot State Frequencies (htna-named)

Renders one of three views of an htna network's state frequencies: the
upstream treemap (default), a combined actor-coloured bar chart, or a
per-actor faceted bar chart.

## Usage

``` r
plot_frequencies_htna(x, view = c("treemap", "bars", "facet"), ...)
```

## Arguments

- x:

  An htna network from
  [`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md).

- view:

  One of `"treemap"` (default), `"bars"`, or `"facet"`.

  - `"treemap"` forwards to
    [`Nestimate::plot_state_frequencies()`](https://rdrr.io/pkg/Nestimate/man/plot_state_frequencies.html)
    and renders the chart automatically; returns the underlying
    `state_freq` object invisibly.

  - `"bars"` builds a combined bar chart with all states on one y-axis,
    sorted by count, fill coloured by actor (using `htna_palette` keyed
    off `x$actor_levels`). Returns the ggplot.

  - `"facet"` builds a per-actor faceted bar chart with
    `scales = "free_y"` so each panel only shows its own actor's states.
    Returns the ggplot.

- ...:

  Forwarded to
  [`Nestimate::plot_state_frequencies()`](https://rdrr.io/pkg/Nestimate/man/plot_state_frequencies.html)
  when `view = "treemap"`. Ignored for `"bars"` and `"facet"` — those
  return ggplot objects, so customisation works through standard ggplot
  composition (`+ theme(...)`, `+ labs(...)`, `+ scale_fill_*()`, etc.).

## Value

For `view = "treemap"`: the `state_freq` object invisibly. For `"bars"`
/ `"facet"`: a ggplot, returned visibly so the chart auto-prints and
standard `+` composition works.

## Details

Suffixed `_htna` to avoid clashing with
[`Nestimate::plot_state_frequencies()`](https://rdrr.io/pkg/Nestimate/man/plot_state_frequencies.html)
when both packages are loaded.

## See also

[`frequencies_htna()`](https://sonsoles.me/htna/reference/frequencies_htna.md)
for the underlying tidy table,
[`state_frequencies_htna()`](https://sonsoles.me/htna/reference/state_frequencies_htna.md),
[`state_distribution_htna()`](https://sonsoles.me/htna/reference/state_distribution_htna.md),
[`mosaic_plot_htna()`](https://sonsoles.me/htna/reference/mosaic_plot_htna.md).

## Examples

``` r
# \donttest{
data(human_long, ai_long, package = "Nestimate")
net <- build_htna(list(Human = human_long, AI = ai_long))
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions)
plot_frequencies_htna(net, view = "treemap")

if (requireNamespace("ggplot2", quietly = TRUE)) {
  plot_frequencies_htna(net, view = "bars")
}

# }
```
