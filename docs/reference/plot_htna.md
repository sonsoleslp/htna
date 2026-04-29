# Plot a Heterogeneous Transition Network

htna's wrapper around
[`cograph::plot_htna()`](https://sonsoles.me/cograph/reference/plot_htna.html).
Reads the actor partition from the canonical `$node_groups` schema
written by
[`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md),
applies the htna visual defaults, and forwards everything to
[`cograph::plot_htna()`](https://sonsoles.me/cograph/reference/plot_htna.html)
with `node_list` passed explicitly (no reliance on cograph's column-name
auto-detection).

Wrapper around
[`cograph::plot_htna()`](https://sonsoles.me/cograph/reference/plot_htna.html)
with defaults suited for HTNA networks built by
[`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md). When
`layout = "circular"`, a custom layout is computed so that the first
actor group appears on the left.

## Usage

``` r
plot_htna(x, layout = "circular", group_colors = htna_palette, ...)

plot_htna(x, layout = "circular", group_colors = htna_palette, ...)
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

- ...:

  Additional arguments passed to
  [`cograph::plot_htna()`](https://sonsoles.me/cograph/reference/plot_htna.html).

## Value

The value returned by
[`cograph::plot_htna()`](https://sonsoles.me/cograph/reference/plot_htna.html)
(invisibly).

Called for its side effect (a plot). Returns `x` invisibly.

## Details

User arguments in `...` always win over the defaults.

## See also

[`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md),
[`plot_htna_bootstrap()`](https://sonsoles.me/htna/reference/plot_htna_bootstrap.md),
[`plot_htna_diff()`](https://sonsoles.me/htna/reference/plot_htna_diff.md).

[`cograph::plot_htna()`](https://sonsoles.me/cograph/reference/plot_htna.html),
[`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md)

## Examples

``` r
if (FALSE) { # \dontrun{
data(human_long, ai_long, package = "Nestimate")
net <- build_htna(list(Human = human_long, AI = ai_long))
plot_htna(net)
plot_htna(net, threshold = 0.1, legend = FALSE)
} # }

if (FALSE) { # \dontrun{
data(human_long, ai_long, package = "Nestimate")
net <- build_htna(list(Human = human_long, AI = ai_long))
plot_htna(net)
plot_htna(net, layout = "auto", threshold = 0.1)
} # }
```
