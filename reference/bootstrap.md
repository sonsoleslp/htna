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
# \donttest{
data(human_ai)
net <- build_htna(human_ai, actor_type = "actor_type")
#> Warning: A network with one long sequence is not recommended and can't be validated using bootstrap and other confirmatory testings.
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions), 'actor_type' (24 sessions)
bootstrap(net, iter = 50)
#>   Edge                   Mean     95% CI          p
#>   -----------------------------------------------
#>   Delegate → Plan      0.615  [0.573, 0.672]  *  
#>   Refine → Specify     0.415  [0.365, 0.462]  *  
#>   Ask → Plan           0.411  [0.383, 0.439]  *  
#>   Check → Execute      0.408  [0.387, 0.436]  *  
#>   Repair → Execute     0.388  [0.327, 0.463]  *  
#>   ... and 53 more significant edges
#> 
#> Bootstrap Network  [Transition Network (relative) | directed]
#>   Iterations : 50  |  Nodes : 12
#>   Edges      : 58 significant / 135 total
#>   CI         : 95%  |  Inference: stability  |  CR [0.75, 1.25]
# }
```
