# Print centrality stability coefficients

[`print()`](https://rdrr.io/r/base/print.html) prints the case-dropping
stability (CS) coefficient of each centrality measure of an
`htna_stability` result. The CS coefficient is the largest proportion of
sessions that can be dropped while the correlation with the full-network
centralities stays above the threshold with the required certainty. The
header gives the number of resamples, the threshold and the drop
proportions. The grouped method prints one block per cohort. It returns
`x` invisibly.

## Usage

``` r
# S3 method for class 'htna_stability'
print(x, ...)

# S3 method for class 'htna_stability_group'
print(x, ...)
```

## Arguments

- x:

  A result of
  [`centrality_stability_htna()`](https://sonsoles.me/htna/reference/centrality_stability_htna.md):
  an `htna_stability` or an `htna_stability_group`.

- ...:

  Unused.

## Value

Invisibly, `x`, unchanged.

## See also

[`as.data.frame.htna_stability()`](https://sonsoles.me/htna/reference/as.data.frame.htna_stability.md)
for the coefficients and the correlation table as data frames,
[`plot.htna_stability()`](https://sonsoles.me/htna/reference/plot.htna_stability.md).

## Examples

``` r
data(human_ai)
net <- build_htna(human_ai, actor_type = "actor_type")
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions), 'actor_type' (24 sessions)
cs  <- centrality_stability_htna(net, iter = 20, seed = 1)
print(cs)
#> HTNA centrality stability
#>   20 iterations, threshold = 0.70, drop proportions 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9
#> 
#>   CS-coefficients:
#>     InStrength       0.90
#>     OutStrength      0.90
#>     Betweenness      0.90
```
