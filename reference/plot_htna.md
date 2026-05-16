# Plot a Heterogeneous Transition Network

Wrapper around
[`cograph::plot_htna()`](https://sonsoles.me/cograph/reference/plot_htna.html)
with defaults suited for HTNA networks built by
[`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md). When
`layout = "circular"`, a custom layout is computed so that the first
actor group appears on the left.

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

  A network object produced by
  [`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md).

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
  [`cograph::plot_htna()`](https://sonsoles.me/cograph/reference/plot_htna.html).

## Value

Called for its side effect (a plot). Returns `x` invisibly.

## See also

[`cograph::plot_htna()`](https://sonsoles.me/cograph/reference/plot_htna.html),
[`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md)

## Examples

``` r
if (FALSE) { # \dontrun{
data(human_long, ai_long, package = "Nestimate")
net <- build_htna(list(Human = human_long, AI = ai_long))
plot_htna(net)
plot_htna(net, layout = "auto", threshold = 0.1)
} # }
```
