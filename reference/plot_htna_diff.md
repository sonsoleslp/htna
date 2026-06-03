# Plot the Difference Between Two htna Networks

Renders the elementwise edge-weight difference `x - y` as a
heterogeneous transition network. Accepts:

## Usage

``` r
plot_htna_diff(
  x,
  y = NULL,
  pos_color = "#009900",
  neg_color = "#C62828",
  group_colors = htna_palette,
  ...
)
```

## Arguments

- x:

  One of: an htna network, a `net_permutation` result, or a
  `net_permutation_group`.

- y:

  htna network (only used when `x` is also an htna network).

- pos_color:

  Edge colour for positive differences. Default `"#009900"`.

- neg_color:

  Edge colour for negative differences. Default `"#C62828"`.

- group_colors:

  Character vector of colours, one per actor group. Defaults to the
  built-in
  [htna_palette](https://sonsoles.me/htna/reference/htna_palette.md).

- ...:

  Forwarded to the underlying splot dispatcher
  ([`cograph::splot()`](https://sonsoles.me/cograph/reference/splot.html)
  or
  [`cograph::splot.net_permutation()`](https://sonsoles.me/cograph/reference/splot.html)).

## Value

The value of the underlying splot call (invisibly).

## Details

- Two htna networks (`x`, `y`): plots `x - y` via
  [`cograph::splot()`](https://sonsoles.me/cograph/reference/splot.html)
  with positive/negative edge coloring.

- A `net_permutation` result: routes through
  [`cograph::splot.net_permutation()`](https://sonsoles.me/cograph/reference/splot.html)
  so the significance overlay (significant pos/neg edges, dashed
  non-significant edges, stars) is drawn upstream.

- A `net_permutation_group`: iterates over all pairwise comparisons and
  plots each (no `par(mfrow=...)` management - user controls layout).

In all cases the layout, node colours and donut match
[`plot_htna()`](https://sonsoles.me/htna/reference/plot_htna.md).

## See also

[`plot_htna()`](https://sonsoles.me/htna/reference/plot_htna.md),
[`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md),
[`Nestimate::permutation()`](https://saqr.me/Nestimate/reference/permutation.html).

## Examples

``` r
if (FALSE) { # \dontrun{
data(human_long, ai_long, package = "Nestimate")
net1 <- build_htna(list(Human = human_long,        AI = ai_long))
net2 <- build_htna(list(Human = human_long[1:50,], AI = ai_long))
plot_htna_diff(net1, net2)

perm <- permutation(net1, net2, iter = 200)
plot_htna_diff(perm)
plot_htna_diff(perm, show_nonsig = TRUE)
} # }
```
