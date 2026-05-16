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
data(human_long, ai_long, package = "Nestimate")
net <- build_htna(list(Human = human_long, AI = ai_long))
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions)
bootstrap(net, iter = 50)
#>   Edge                   Mean     95% CI          p
#>   -----------------------------------------------
#>   Verify → Execute     0.649  [0.604, 0.701]  *  
#>   Delegate → Plan      0.615  [0.556, 0.675]  *  
#>   Request → Specify    0.545  [0.516, 0.573]  *  
#>   Investigate → Plan   0.426  [0.401, 0.452]  *  
#>   Refine → Specify     0.412  [0.361, 0.463]  *  
#>   ... and 62 more significant edges
#> 
#> Bootstrap Network  [Transition Network (relative) | directed]
#>   Iterations : 50  |  Nodes : 17
#>   Edges      : 67 significant / 246 total
#>   CI         : 95%  |  Inference: stability  |  CR [0.75, 1.25]
# }
```
