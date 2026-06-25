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
#>   Delegate → Plan      0.615  [0.556, 0.675]  *  
#>   Refine → Specify     0.412  [0.361, 0.463]  *  
#>   Check → Execute      0.411  [0.385, 0.438]  *  
#>   Ask → Plan           0.409  [0.387, 0.435]  *  
#>   Repair → Execute     0.385  [0.298, 0.448]  *  
#>   ... and 54 more significant edges
#> 
#> Bootstrap Network  [Transition Network (relative) | directed]
#>   Iterations : 50  |  Nodes : 12
#>   Edges      : 59 significant / 135 total
#>   CI         : 95%  |  Inference: stability  |  CR [0.75, 1.25]
# }
```
