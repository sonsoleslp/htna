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
