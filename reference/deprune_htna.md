# Restore All Edges of a Pruned Transition Network

htna-named wrapper for
[`Nestimate::net_deprune()`](https://pak.dynasite.org/Nestimate/reference/net_deprune.html).
Reverses a
[`prune_htna()`](https://sonsoles.me/htna/reference/prune_htna.md) step,
restoring the original (unpruned) edge set.

## Usage

``` r
deprune_htna(x, ...)
```

## Arguments

- x:

  A pruned htna network (or other object accepted by
  [`Nestimate::net_deprune()`](https://pak.dynasite.org/Nestimate/reference/net_deprune.html));
  S3 dispatch on `x` is preserved.

- ...:

  Additional arguments passed to
  [`Nestimate::net_deprune()`](https://pak.dynasite.org/Nestimate/reference/net_deprune.html).
  See that function for details.

## Value

The depruned htna network. See
[`Nestimate::net_deprune()`](https://pak.dynasite.org/Nestimate/reference/net_deprune.html).

## Details

The actor partition (`$node_groups`, `$actor_levels`, `htna` class) is
preserved, so the restored network stays htna-aware.

Suffixed `_htna` to avoid clashing with the tna verb `deprune()` and to
sit alongside the rest of the htna API.

## See also

[`prune_htna()`](https://sonsoles.me/htna/reference/prune_htna.md),
[`reprune_htna()`](https://sonsoles.me/htna/reference/reprune_htna.md),
[`pruning_details_htna()`](https://sonsoles.me/htna/reference/pruning_details_htna.md).

## Examples

``` r
data(human_ai)
net <- build_htna(human_ai, actor_type = "actor_type")
#> A network with one long sequence is not recommended and can't be validated using bootstrap and other confirmatory testings.
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions), 'actor_type' (24 sessions)
pruned <- prune_htna(net, method = "threshold", threshold = 0.05)
deprune_htna(pruned)
#> Transition Network (relative probabilities) [directed]
#>   Weights: [0.002, 0.611]  |  mean: 0.089
#> 
#>   Weight matrix:
#>               Ask Check Delegate Execute Frustrate Inquire  Plan Refine Repair
#>   Ask       0.018 0.063    0.000   0.022     0.116   0.063 0.409  0.051  0.004
#>   Check     0.117 0.050    0.015   0.411     0.058   0.005 0.008  0.042  0.033
#>   Delegate  0.000 0.039    0.000   0.011     0.110   0.032 0.611  0.014  0.000
#>   Execute   0.061 0.087    0.000   0.074     0.143   0.088 0.107  0.069  0.002
#>   Frustrate 0.194 0.104    0.039   0.119     0.114   0.060 0.004  0.098  0.029
#>   Inquire   0.251 0.069    0.027   0.285     0.039   0.033 0.009  0.016  0.042
#>   Plan      0.000 0.146    0.000   0.015     0.215   0.083 0.003  0.086  0.005
#>   Refine    0.143 0.045    0.006   0.206     0.008   0.014 0.013  0.000  0.025
#>   Repair    0.241 0.012    0.028   0.391     0.043   0.047 0.016  0.024  0.004
#>   Report    0.102 0.114    0.000   0.009     0.124   0.102 0.050  0.058  0.039
#>   Request   0.148 0.055    0.018   0.292     0.016   0.010 0.008  0.009  0.007
#>   Specify   0.269 0.017    0.040   0.273     0.096   0.011 0.015  0.002  0.013
#>             Report Request Specify
#>   Ask        0.028   0.173   0.052
#>   Check      0.055   0.049   0.156
#>   Delegate   0.018   0.131   0.035
#>   Execute    0.010   0.293   0.065
#>   Frustrate  0.021   0.135   0.085
#>   Inquire    0.170   0.028   0.029
#>   Plan       0.026   0.325   0.095
#>   Refine     0.023   0.105   0.413
#>   Repair     0.103   0.071   0.020
#>   Report     0.077   0.257   0.066
#>   Request    0.034   0.067   0.336
#>   Specify    0.037   0.126   0.101 
#> 
#>   Initial probabilities:
#>   Specify       0.818  ████████████████████████████████████████
#>   Request       0.156  ████████
#>   Frustrate     0.023  █
#>   Refine        0.002  
#>   Ask           0.000  
#>   Check         0.000  
#>   Delegate      0.000  
#>   Execute       0.000  
#>   Inquire       0.000  
#>   Plan          0.000  
#>   Repair        0.000  
#>   Report        0.000  
```
