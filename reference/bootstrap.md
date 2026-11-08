# Bootstrap generic

S3 generic dispatched on the class of `x`. Provided so `bootstrap(net)`
works directly on an htna network (see
[`bootstrap_htna()`](https://sonsoles.me/htna/reference/bootstrap_htna.md)).

## Usage

``` r
bootstrap(x, ...)
```

## Arguments

- x:

  An object to bootstrap.

- ...:

  Arguments forwarded to the method.

## Value

An object whose structure is method-defined.

## Examples

``` r
data(human_ai)
net <- build_htna(human_ai, actor_type = "actor_type")
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions), 'actor_type' (24 sessions)
bootstrap(net, iter = 50, seed = 1)
#>   Edge                   Mean     95% CI          p
#>   -----------------------------------------------
#>   Delegate → Plan      0.614  [0.563, 0.672]  *  
#>   Refine → Specify     0.414  [0.362, 0.462]  *  
#>   Ask → Plan           0.413  [0.384, 0.441]  *  
#>   Check → Execute      0.410  [0.385, 0.441]  *  
#>   Repair → Execute     0.391  [0.346, 0.447]  *  
#>   ... and 54 more significant edges
#> 
#> Bootstrap Network  [Transition Network (relative) | directed]
#>   Iterations : 50  |  Nodes : 12
#>   Edges      : 59 significant / 135 total
#>   CI         : 95%  |  Inference: stability  |  CR [0.75, 1.25]
```
