# Plot an htna Bootstrap Result

Renders the bootstrap result via
[`cograph::splot.net_bootstrap()`](https://sonsoles.me/cograph/reference/splot.html)
so the CI / significance / dashed-edge overlay is preserved, but
overrides the layout and node styling to match
[`plot_htna()`](https://sonsoles.me/htna/reference/plot_htna.md)
(multi-group circular layout, warm
[htna_palette](https://sonsoles.me/htna/reference/htna_palette.md),
darkened donut).

## Usage

``` r
plot_htna_bootstrap(boot, group_colors = htna_palette, ...)

# S3 method for class 'htna_bootstrap'
plot(x, ...)
```

## Arguments

- boot:

  An object from
  [`bootstrap_htna()`](https://sonsoles.me/htna/reference/bootstrap_htna.md)
  (or a `htna_bootstrap_group` list of them).

- group_colors:

  Character vector of colours, one per actor group. Defaults to the
  built-in
  [htna_palette](https://sonsoles.me/htna/reference/htna_palette.md).

- ...:

  Forwarded to
  [`cograph::splot.net_bootstrap()`](https://sonsoles.me/cograph/reference/splot.html)
  (e.g. `display`, `show_ci`, `show_stars`). User args win.

- x:

  Same as `boot`; used when calling via the
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) generic.

## Value

The value returned by
[`cograph::splot.net_bootstrap()`](https://sonsoles.me/cograph/reference/splot.html)
(invisibly), or the input group invisibly.

## Details

For an `htna_bootstrap_group` (i.e. the result of running
[`bootstrap_htna()`](https://sonsoles.me/htna/reference/bootstrap_htna.md)
on a `build_htna(..., group = ...)` output), each element is plotted
individually with its name as the title - this function does not manage
`par(mfrow=...)`.

## See also

[`bootstrap_htna()`](https://sonsoles.me/htna/reference/bootstrap_htna.md),
[`plot_htna()`](https://sonsoles.me/htna/reference/plot_htna.md),
[`cograph::splot.net_bootstrap()`](https://sonsoles.me/cograph/reference/splot.html).

## Examples

``` r
if (FALSE) { # \dontrun{
data(human_long, ai_long, package = "Nestimate")
net  <- build_htna(list(Human = human_long, AI = ai_long))
boot <- bootstrap_htna(net, iter = 200)
plot_htna_bootstrap(boot)
plot_htna_bootstrap(boot, display = "significant")
} # }
```
