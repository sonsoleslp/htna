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
data(human_ai)
net <- build_htna(human_ai, actor_type = "actor_type")
#> Warning: A network with one long sequence is not recommended and can't be validated using bootstrap and other confirmatory testings.
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions), 'actor_type' (24 sessions)
plot_sequences(net)

# }
```
