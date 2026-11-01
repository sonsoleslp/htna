# Prune Weak Edges from a Transition Network

htna-named wrapper for
[`Nestimate::net_prune()`](https://pak.dynasite.org/Nestimate/reference/net_prune.html).
Removes edges that fall below a weight/threshold rule, returning a
pruned network of the same shape.

## Usage

``` r
prune_htna(x, ...)
```

## Arguments

- x:

  An htna network from
  [`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md) (or
  other object accepted by
  [`Nestimate::net_prune()`](https://pak.dynasite.org/Nestimate/reference/net_prune.html));
  S3 dispatch on `x` is preserved.

- ...:

  Additional arguments passed to
  [`Nestimate::net_prune()`](https://pak.dynasite.org/Nestimate/reference/net_prune.html),
  such as `method`, `threshold`, `lowest`, `level`, and `boot`. See that
  function for the full, current argument list.

## Value

A pruned htna network: class
`c("htna", "netobject", "cograph_network")`, the same structure as
[`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md)
returns, with the removed edges set to zero in `weights` and the pruning
rule recorded for
[`deprune_htna()`](https://sonsoles.me/htna/reference/deprune_htna.md),
[`reprune_htna()`](https://sonsoles.me/htna/reference/reprune_htna.md)
and
[`pruning_details_htna()`](https://sonsoles.me/htna/reference/pruning_details_htna.md).
See
[`Nestimate::net_prune()`](https://pak.dynasite.org/Nestimate/reference/net_prune.html).

## Details

The actor partition (`$node_groups`, `$actor_levels`, `htna` class) is
preserved on the pruned network, so
[`plot_htna()`](https://sonsoles.me/htna/reference/plot_htna.md) and the
other htna verbs keep working on the result.

Suffixed `_htna` to avoid clashing with the tna verb `prune()` and to
sit alongside the rest of the htna API.

## See also

[`deprune_htna()`](https://sonsoles.me/htna/reference/deprune_htna.md),
[`reprune_htna()`](https://sonsoles.me/htna/reference/reprune_htna.md),
[`pruning_details_htna()`](https://sonsoles.me/htna/reference/pruning_details_htna.md),
[`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md).

## Examples

``` r
data(human_ai)
net <- build_htna(human_ai, actor_type = "actor_type")
#> A network with one long sequence is not recommended and can't be validated using bootstrap and other confirmatory testings.
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions), 'actor_type' (24 sessions)
prune_htna(net, method = "threshold", threshold = 0.05)
#> Transition Network (relative probabilities) [directed]
#>   Weights: [0.003, 0.611]  |  mean: 0.146
#> 
#>   Weight matrix:
#>               Ask Check Delegate Execute Frustrate Inquire  Plan Refine Repair
#>   Ask       0.018 0.063        0   0.000     0.116   0.063 0.409  0.051  0.000
#>   Check     0.117 0.050        0   0.411     0.058   0.000 0.000  0.000  0.000
#>   Delegate  0.000 0.000        0   0.000     0.110   0.000 0.611  0.000  0.000
#>   Execute   0.061 0.087        0   0.074     0.143   0.088 0.107  0.069  0.000
#>   Frustrate 0.194 0.104        0   0.119     0.114   0.060 0.000  0.098  0.000
#>   Inquire   0.251 0.069        0   0.285     0.000   0.033 0.000  0.000  0.000
#>   Plan      0.000 0.146        0   0.000     0.215   0.083 0.003  0.086  0.000
#>   Refine    0.143 0.000        0   0.206     0.000   0.000 0.000  0.000  0.000
#>   Repair    0.241 0.000        0   0.391     0.000   0.000 0.000  0.000  0.004
#>   Report    0.102 0.114        0   0.000     0.124   0.102 0.000  0.058  0.000
#>   Request   0.148 0.055        0   0.292     0.000   0.000 0.000  0.000  0.000
#>   Specify   0.269 0.000        0   0.273     0.096   0.000 0.000  0.000  0.000
#>             Report Request Specify
#>   Ask        0.000   0.173   0.052
#>   Check      0.055   0.000   0.156
#>   Delegate   0.000   0.131   0.000
#>   Execute    0.000   0.293   0.065
#>   Frustrate  0.000   0.135   0.085
#>   Inquire    0.170   0.000   0.000
#>   Plan       0.000   0.325   0.095
#>   Refine     0.000   0.105   0.413
#>   Repair     0.103   0.071   0.000
#>   Report     0.077   0.257   0.066
#>   Request    0.000   0.067   0.336
#>   Specify    0.000   0.126   0.101 
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
