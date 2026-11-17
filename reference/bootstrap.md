# Bootstrap a network

`bootstrap()` bootstraps a fitted network: it is a generic for
resampling-based uncertainty of a fitted network. Its htna method runs
[`bootstrap_htna()`](https://sonsoles.me/htna/reference/bootstrap_htna.md):
the sessions are resampled, the network is re-estimated on each
resample, and the distribution of every edge weight is summarized. For
an htna network it returns an `htna_bootstrap`.

The generic has the same name as the `tna` generic; when both packages
are attached, the one attached last is found first.
[`bootstrap_htna()`](https://sonsoles.me/htna/reference/bootstrap_htna.md)
is unambiguous.

## Usage

``` r
bootstrap(x, ...)
```

## Arguments

- x:

  An object to bootstrap; for htna, a network from
  [`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md).

- ...:

  Passed to the method; see
  [`bootstrap_htna()`](https://sonsoles.me/htna/reference/bootstrap_htna.md).

## Value

The method's result; for an htna network an `htna_bootstrap` (see
[`bootstrap_htna()`](https://sonsoles.me/htna/reference/bootstrap_htna.md)).

## See also

[`bootstrap_htna()`](https://sonsoles.me/htna/reference/bootstrap_htna.md).

## Examples

``` r
data(human_ai)
net <- build_htna(human_ai, actor_type = "actor_type")
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions), 'actor_type' (24 sessions)
bootstrap(net, iter = 50, seed = 1)
#>   Edge                   Mean     95% CI          p
#>   -----------------------------------------------
#>   Delegate → Plan        0.614  [0.563, 0.672]  *  
#>   Refine → Specify       0.414  [0.362, 0.462]  *  
#>   Ask → Plan             0.413  [0.384, 0.441]  *  
#>   Check → Execute        0.410  [0.385, 0.441]  *  
#>   Repair → Execute       0.391  [0.346, 0.447]  *  
#>   ... and 54 more significant edges
#> 
#> Bootstrap Network  [Transition Network (relative) | directed]
#>   Iterations : 50  |  Nodes : 12
#>   Edges      : 59 significant / 135 total
#>   CI         : 95%  |  Inference: stability  |  CR [0.75, 1.25]
```
