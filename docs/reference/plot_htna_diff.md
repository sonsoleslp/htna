# Plot the Difference Between Two htna Networks

Renders the elementwise edge-weight difference `x - y` as a
heterogeneous transition network using
[`plot_htna()`](https://sonsoles.me/htna/reference/plot_htna.md). Both
networks must share the same actor partition (same group labels in
`$node_groups`).

## Usage

``` r
plot_htna_diff(x, y, ...)
```

## Arguments

- x, y:

  htna networks produced by
  [`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md)
  with matching actor partitions.

- ...:

  Forwarded to
  [`cograph::plot_htna()`](https://sonsoles.me/cograph/reference/plot_htna.html).
  User args win.

## Value

The value returned by
[`cograph::plot_htna()`](https://sonsoles.me/cograph/reference/plot_htna.html)
(invisibly).

## Details

This is the htna-side workaround for
[`cograph::plot_compare()`](https://sonsoles.me/cograph/reference/plot_compare.html),
which does not (yet) accept a `node_list` or pre-computed multi-group
layout. As a result, the signed (positive / negative) edge coloring that
`plot_compare` provides upstream is lost here - edges are colored as in
any other
[`plot_htna()`](https://sonsoles.me/htna/reference/plot_htna.md) call.
When cograph adds `node_list` (or a layout-matrix argument) to
`plot_compare`, swap this implementation for a real `plot_compare`
wrapper that preserves diff semantics.

## See also

[`plot_htna()`](https://sonsoles.me/htna/reference/plot_htna.md),
[`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md).

## Examples

``` r
if (FALSE) { # \dontrun{
data(human_long, ai_long, package = "Nestimate")
net1 <- build_htna(list(Human = human_long,        AI = ai_long))
net2 <- build_htna(list(Human = human_long[1:50,], AI = ai_long))
plot_htna_diff(net1, net2)
} # }
```
