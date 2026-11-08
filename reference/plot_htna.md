# Plot a Heterogeneous Transition Network

Wrapper around
[`cograph::plot_htna()`](https://sonsoles.me/cograph/reference/plot_htna.html)
with defaults suited for HTNA networks built by
[`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md). The
drawing itself is done by cograph; this function supplies the
actor-aware styling: one colour and shape per actor group, darkened
donut borders, and an actor legend. For the circular layout (and
`"auto"` with two or more groups) the group order is arranged so that
the first actor group appears on the left.

## Usage

``` r
plot_htna(
  x,
  layout = "circular",
  group_colors = htna_palette,
  group_shapes = htna_shape_palette,
  minimum = 0.05,
  ...
)

# S3 method for class 'htna'
plot(x, ...)

# S3 method for class 'htna_group'
plot(x, ...)
```

## Arguments

- x:

  A network produced by
  [`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md), or
  an `htna_group`.

- layout:

  Character. Layout algorithm. Default `"circular"`.

- group_colors:

  Character vector of colours, one per actor group. Defaults to the
  built-in
  [htna_palette](https://sonsoles.me/htna/reference/htna_palette.md).

- group_shapes:

  Character vector of node shapes, one per actor group. Defaults to the
  built-in `htna_shape_palette`.

- minimum:

  Numeric. Minimum absolute edge weight to display. Edges below this
  threshold are hidden. Default `0.05`.

- ...:

  Additional arguments passed to
  [`cograph::plot_htna()`](https://sonsoles.me/cograph/reference/plot_htna.html)
  (and from there to
  [`cograph::splot()`](https://sonsoles.me/cograph/reference/splot.html)),
  e.g. `title`.

## Value

Called for its side effect (a plot). For a single network, invisibly,
the value returned by
[`cograph::plot_htna()`](https://sonsoles.me/cograph/reference/plot_htna.html);
for an `htna_group`, `x` invisibly.

## Details

For an `htna_group` (or any plain list of htna networks) one plot is
drawn per cohort, titled with the cohort name. The function does not
manage the panel grid; wrap the call in `par(mfrow = ...)` for a panel.

## See also

[`cograph::plot_htna()`](https://sonsoles.me/cograph/reference/plot_htna.html),
[`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md)

## Examples

``` r
data(human_ai)
net <- build_htna(human_ai, actor_type = "actor_type")
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions), 'actor_type' (24 sessions)
plot_htna(net)

plot_htna(net, layout = "auto", minimum = 0.1)

plot(net)                     # the plot() method calls plot_htna()

```
