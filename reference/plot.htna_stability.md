# Plot Method for htna Centrality Stability Objects

S3 method for plotting htna centrality stability results. Dispatches to
cograph's plotting method with htna-specific styling.

## Usage

``` r
# S3 method for class 'htna_stability'
plot(x, ...)
```

## Arguments

- x:

  An object of class `htna_stability` from
  [`centrality_stability_htna()`](https://sonsoles.me/htna/reference/centrality_stability_htna.md).

- ...:

  Additional arguments passed to the plotting method.

## Value

A ggplot object or plot output, depending on the underlying method.

## Examples

``` r
# \donttest{
data(human_ai)
net <- build_htna(human_ai, actor_type = "actor_type")
#> Warning: A network with one long sequence is not recommended and can't be validated using bootstrap and other confirmatory testings.
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions), 'actor_type' (24 sessions)
cs  <- centrality_stability_htna(net, iter = 20, seed = 1)
plot(cs)

# }
```
