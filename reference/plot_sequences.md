# Sequence plot generic

S3 generic dispatched on `x`. Calling `plot_sequences(net)` on an htna
network forwards to
[`sequence_plot_htna()`](https://sonsoles.me/htna/reference/sequence_plot_htna.md).

## Usage

``` r
plot_sequences(x, ...)
```

## Arguments

- x:

  An object to plot.

- ...:

  Forwarded to the method.

## Value

Method-defined.

## Examples

``` r
# \donttest{
data(human_long, ai_long, package = "Nestimate")
net <- build_htna(list(Human = human_long, AI = ai_long))
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions)
plot_sequences(net)

# }
```
