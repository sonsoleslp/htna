# Plot an htna Bootstrap Result

Composes htna's visual defaults and the htna multi-group circular layout
with cograph's bootstrap renderer
([`cograph::splot.net_bootstrap()`](https://sonsoles.me/cograph/reference/splot.html)).
The CI / significance overlay is whatever cograph produces; htna only
owns the geometry and the visual identity (so the bootstrap plot looks
like the same network as
[`plot_htna()`](https://sonsoles.me/htna/reference/plot_htna.md)).

## Usage

``` r
plot_htna_bootstrap(boot, ...)
```

## Arguments

- boot:

  An object returned by
  [`bootstrap_htna()`](https://sonsoles.me/htna/reference/bootstrap_htna.md).

- ...:

  Forwarded to
  [`cograph::splot.net_bootstrap()`](https://sonsoles.me/cograph/reference/splot.html).
  User args win over the htna defaults.

## Value

The value returned by
[`cograph::splot.net_bootstrap()`](https://sonsoles.me/cograph/reference/splot.html)
(invisibly).

## Details

The layout is computed on the htna side and injected as a matrix
together with `rescale = FALSE` and `layout_scale = 1`. Those three
arguments together are the contract that lets the htna geometry survive
into cograph's renderer unchanged - drop any one and cograph
re-normalizes.

## See also

[`bootstrap_htna()`](https://sonsoles.me/htna/reference/bootstrap_htna.md),
[`plot_htna()`](https://sonsoles.me/htna/reference/plot_htna.md).

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
