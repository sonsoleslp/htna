# Tidy Report of a Network's Pruning

htna-named wrapper for
[`Nestimate::net_pruning_details()`](https://saqr.me/Nestimate/reference/net_pruning_details.html).
Returns a tidy data frame describing which edges were kept or removed by
a [`prune_htna()`](https://sonsoles.me/htna/reference/prune_htna.md)
step and why.

## Usage

``` r
pruning_details_htna(x, ...)
```

## Arguments

- x:

  A pruned htna network (or other object accepted by
  [`Nestimate::net_pruning_details()`](https://saqr.me/Nestimate/reference/net_pruning_details.html));
  S3 dispatch on `x` is preserved.

- ...:

  Additional arguments passed to
  [`Nestimate::net_pruning_details()`](https://saqr.me/Nestimate/reference/net_pruning_details.html).
  See that function for details.

## Value

A data frame, one row per edge. See
[`Nestimate::net_pruning_details()`](https://saqr.me/Nestimate/reference/net_pruning_details.html)
for details.

## Details

Suffixed `_htna` to avoid clashing with the tna verb `pruning_details()`
and to sit alongside the rest of the htna API.

## See also

[`prune_htna()`](https://sonsoles.me/htna/reference/prune_htna.md),
[`deprune_htna()`](https://sonsoles.me/htna/reference/deprune_htna.md),
[`reprune_htna()`](https://sonsoles.me/htna/reference/reprune_htna.md).

## Examples

``` r
# \donttest{
data(human_ai)
net <- build_htna(human_ai, actor_type = "actor_type")
#> Warning: A network with one long sequence is not recommended and can't be validated using bootstrap and other confirmatory testings.
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions), 'actor_type' (24 sessions)
pruned <- prune_htna(net, method = "threshold", threshold = 0.05)
pruning_details_htna(pruned)
#> Pruning details
#>   Method:  user-specified threshold (0.05)
#>   Removed: 62 edges
#>   Retained: 73 edges
#> 
#>       from        to      weight
#>   Delegate     Check 0.038869258
#>     Refine     Check 0.045454545
#>     Repair     Check 0.011857708
#>    Specify     Check 0.016809605
#>      Check  Delegate 0.015408320
#>  Frustrate  Delegate 0.039122703
#>    Inquire  Delegate 0.026995305
#>     Refine  Delegate 0.006313131
#>     Repair  Delegate 0.027667984
#>    Request  Delegate 0.018387097
#>    Specify  Delegate 0.040137221
#>        Ask   Execute 0.022052587
#>   Delegate   Execute 0.010600707
#>       Plan   Execute 0.015483871
#>     Report   Execute 0.008771930
#>    Inquire Frustrate 0.038732394
#>     Refine Frustrate 0.007575758
#>     Repair Frustrate 0.043478261
#>    Request Frustrate 0.015806452
#>      Check   Inquire 0.004622496
#>   Delegate   Inquire 0.031802120
#>     Refine   Inquire 0.013888889
#>     Repair   Inquire 0.047430830
#>    Request   Inquire 0.009677419
#>    Specify   Inquire 0.010634648
#>      Check      Plan 0.008474576
#>  Frustrate      Plan 0.003556609
#>    Inquire      Plan 0.009389671
#>     Refine      Plan 0.012626263
#>     Repair      Plan 0.015810277
#>     Report      Plan 0.049707602
#>    Request      Plan 0.007741935
#>    Specify      Plan 0.015094340
#>      Check    Refine 0.041602465
#>   Delegate    Refine 0.014134276
#>    Inquire    Refine 0.016431925
#>     Repair    Refine 0.023715415
#>    Request    Refine 0.009354839
#>    Specify    Refine 0.002401372
#>        Ask    Repair 0.003816794
#>      Check    Repair 0.033127889
#>    Execute    Repair 0.001907184
#>  Frustrate    Repair 0.029045643
#>    Inquire    Repair 0.042253521
#>       Plan    Repair 0.005161290
#>     Refine    Repair 0.025252525
#>     Report    Repair 0.039473684
#>    Request    Repair 0.006774194
#>    Specify    Repair 0.012692967
#>        Ask    Report 0.027989822
#>   Delegate    Report 0.017667845
#>    Execute    Report 0.009535919
#>  Frustrate    Report 0.021339656
#>       Plan    Report 0.026451613
#>     Refine    Report 0.022727273
#>    Request    Report 0.033870968
#>    Specify    Report 0.037392796
#>      Check   Request 0.049306626
#>    Inquire   Request 0.028169014
#>   Delegate   Specify 0.035335689
#>    Inquire   Specify 0.029342723
#>     Repair   Specify 0.019762846
# }
```
