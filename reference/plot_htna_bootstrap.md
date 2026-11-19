# Plot the bootstrap-assessed edges of an htna network

`plot_htna_bootstrap()` plots the bootstrap-assessed edges of an htna
network: it shows which transitions of a heterogeneous network are
stable enough to interpret. Edges that are significant across resamples
are drawn solid, the others dashed (or hidden), so the reader sees which
within-actor transitions and which hand-overs between actors the data
support. A bootstrap of a transition network resamples the sessions many
times and records how often each edge is present and how much its weight
varies.

`plot_htna_bootstrap()` draws the result of
[`bootstrap_htna()`](https://sonsoles.me/htna/reference/bootstrap_htna.md),
or the closed-form result of
[`certainty_htna()`](https://sonsoles.me/htna/reference/certainty_htna.md),
in the layout and actor styling of
[`plot_htna()`](https://sonsoles.me/htna/reference/plot_htna.md). With
`display = "significant"` only the significant edges are drawn;
`show_ci = TRUE` adds the confidence interval of each edge weight. A
grouped bootstrap is drawn as one plot per cohort, titled with its
cohort name. [`plot()`](https://rdrr.io/r/graphics/plot.default.html) on
a bootstrap result calls this function. It is called for the plot and
returns, invisibly, the value of the drawing call, or `boot` for a
grouped bootstrap.

The table behind the plot (one row per edge, with its weight, interval
and p-value) is returned by
[`summary()`](https://rdrr.io/r/base/summary.html) of the bootstrap
result.

## Usage

``` r
plot_htna_bootstrap(boot, group_colors = htna_palette, ...)

# S3 method for class 'htna_bootstrap'
plot(x, ...)
```

## Arguments

- boot, x:

  A result of
  [`bootstrap_htna()`](https://sonsoles.me/htna/reference/bootstrap_htna.md)
  (an `htna_bootstrap` or an `htna_bootstrap_group`) or of
  [`certainty_htna()`](https://sonsoles.me/htna/reference/certainty_htna.md).

- group_colors:

  Colours of the actor types, in actor order. Default
  [htna_palette](https://sonsoles.me/htna/reference/htna_palette.md).

- ...:

  Further drawing arguments: `display` (`"styled"` (default),
  `"significant"` or `"full"`), `show_ci` (default `FALSE`),
  `show_stars` (default `TRUE`), `title`, and the arguments of
  [`cograph::splot()`](https://sonsoles.me/cograph/reference/splot.html).

## Value

Called for the plot. Invisibly, the value of the drawing call for one
bootstrap, or `boot` for a grouped bootstrap.

## Conditions

`htna_empty_input` for a grouped bootstrap without cohorts;
`htna_bad_input` when `boot` is not a result of
[`bootstrap_htna()`](https://sonsoles.me/htna/reference/bootstrap_htna.md)
or
[`certainty_htna()`](https://sonsoles.me/htna/reference/certainty_htna.md)
on an htna network.

## See also

[`bootstrap_htna()`](https://sonsoles.me/htna/reference/bootstrap_htna.md),
[`plot_htna()`](https://sonsoles.me/htna/reference/plot_htna.md),
[`certainty_htna()`](https://sonsoles.me/htna/reference/certainty_htna.md)

## Examples

``` r
data(human_ai)
net  <- build_htna(human_ai, actor_type = "actor_type")
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions), 'actor_type' (24 sessions)
boot <- bootstrap_htna(net, iter = 50, seed = 1)
plot_htna_bootstrap(boot)

plot_htna_bootstrap(boot, display = "significant")

```
