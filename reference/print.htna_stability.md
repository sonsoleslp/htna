# Print Method for htna Centrality Stability Objects

S3 method for printing htna centrality stability results.

## Usage

``` r
# S3 method for class 'htna_stability'
print(x, ...)
```

## Arguments

- x:

  An object of class `htna_stability` from
  [`centrality_stability_htna()`](https://sonsoles.me/htna/reference/centrality_stability_htna.md).

- ...:

  Additional arguments passed to the print method.

## Value

Invisibly returns the object, prints stability information.

## Examples

``` r
# \donttest{
data(human_ai)
net <- build_htna(human_ai, actor_type = "actor_type")
#> Warning: A network with one long sequence is not recommended and can't be validated using bootstrap and other confirmatory testings.
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions), 'actor_type' (24 sessions)
cs  <- centrality_stability_htna(net, iter = 20, seed = 1)
print(cs)
#> HTNA Centrality Stability Analysis
#> ===================================
#> Centrality Stability (20 iterations, threshold = 0.7)
#>   Drop proportions: 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9
#> 
#>   CS-coefficients:
#>     InStrength       0.90
#>     OutStrength      0.90
#>     Betweenness      0.90
# }
```
