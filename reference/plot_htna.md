# Plot a heterogeneous transition network

`plot_htna()` plots a heterogeneous transition network: the codes of
every actor type as nodes and the transitions between them as directed
edges whose width and label give the transition weight. Each actor type
has its own colour and node shape and occupies its own arc of the
circular layout, so the transitions within an actor type run along its
arc and the hand-overs between actor types cross the center. The plot
shows which transitions dominate the interaction and how the actors pass
control to each other.

`plot_htna()` draws one network, or one plot per cohort for a grouped
network, each titled with its cohort name; arrange several plots with
`par(mfrow = ...)`. Edges with a weight below `minimum` are hidden. The
first actor type is placed on the left of the circular layout.
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) on an htna
network or `htna_group` calls `plot_htna()`. It is called for the plot
and returns, invisibly, the value of
[`cograph::plot_htna()`](https://sonsoles.me/cograph/reference/plot_htna.html)
for one network, or `x` for a grouped network.

A start and an end state (`build_htna(start = , end = )`) are not an arc
of the circular layout. The actor types keep the positions they have
without them, and the start state is placed at the top centre and the
end state at the bottom centre, in the colour and shape of the
`"Boundary"` type.
[`plot_htna_bootstrap()`](https://sonsoles.me/htna/reference/plot_htna_bootstrap.md)
and
[`plot_htna_diff()`](https://sonsoles.me/htna/reference/plot_htna_diff.md)
place them the same way.

To show the difference between two networks use
[`plot_htna_diff()`](https://sonsoles.me/htna/reference/plot_htna_diff.md);
to show only the edges that survive bootstrapping use
[`plot_htna_bootstrap()`](https://sonsoles.me/htna/reference/plot_htna_bootstrap.md).

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

  An htna network from
  [`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md), or
  an `htna_group`.

- layout:

  Node layout. Default `"circular"`; `"polygon"` places three or more
  actor types on the sides of a polygon. Start and end states are placed
  as anchors in the circular layout only; in another layout they are
  laid out as one more actor type.

- group_colors:

  Colours of the actor types, in actor order. Default
  [htna_palette](https://sonsoles.me/htna/reference/htna_palette.md).

- group_shapes:

  Node shapes of the actor types, in actor order. Default
  `c("circle", "square", "diamond", "triangle", "pentagon", "hexagon", "star", "cross")`.

- minimum:

  Smallest edge weight drawn. Default `0.05`.

- ...:

  Further drawing arguments of
  [`cograph::plot_htna()`](https://sonsoles.me/cograph/reference/plot_htna.html)
  and
  [`cograph::splot()`](https://sonsoles.me/cograph/reference/splot.html),
  such as `title`, `threshold` or `edge_labels`.

## Value

Called for the plot. Invisibly, the value of
[`cograph::plot_htna()`](https://sonsoles.me/cograph/reference/plot_htna.html)
for one network, or `x` for a grouped network.

## Conditions

`htna_empty_input` when a grouped network has no cohorts.

## See also

[`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md),
[`plot_htna_diff()`](https://sonsoles.me/htna/reference/plot_htna_diff.md),
[`plot_htna_bootstrap()`](https://sonsoles.me/htna/reference/plot_htna_bootstrap.md),
[htna_palette](https://sonsoles.me/htna/reference/htna_palette.md)

## Examples

``` r
data(human_ai)
net <- build_htna(human_ai, actor_type = "actor_type")
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions), 'actor_type' (24 sessions)
plot_htna(net)

plot_htna(net, minimum = 0.1)


grp <- build_htna(human_ai, actor_type = "actor_type", group = "phase")
#> Metadata aggregated per session: ties resolved by first occurrence in 'cluster' (24 sessions), 'actor_type' (9 sessions)
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (18 sessions), 'actor_type' (15 sessions)
plot(grp)


```
