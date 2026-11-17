# AI turns of the vibe-coding sessions

`ai_simplified` is the AI part of
[human_ai](https://sonsoles.me/htna/reference/human_ai.md): its AI rows,
as one data frame for one actor type. Together with
[human_simplified](https://sonsoles.me/htna/reference/human_simplified.md)
it is the input for the named-list form of
[`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md), in
which each actor type comes in its own data frame.

## Usage

``` r
ai_simplified
```

## Format

A data frame with 8,551 rows and 10 columns: the columns of
[human_ai](https://sonsoles.me/htna/reference/human_ai.md) without
`phase`; `actor_type` is always `"AI"`.

## Source

Derived from the `ai_long` data of the 'Nestimate' package (see
[human_ai](https://sonsoles.me/htna/reference/human_ai.md)).

## See also

[human_ai](https://sonsoles.me/htna/reference/human_ai.md),
[human_simplified](https://sonsoles.me/htna/reference/human_simplified.md),
[human_ai_codebook](https://sonsoles.me/htna/reference/human_ai_codebook.md)

## Examples

``` r
data(human_simplified, ai_simplified)
net <- build_htna(list(Human = human_simplified, AI = ai_simplified))
#> Registered S3 methods overwritten by 'Nestimate':
#>   method            from       
#>   plot.net_mlvar    idiographic
#>   print.net_mlvar   idiographic
#>   summary.net_mlvar idiographic
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions), 'actor_type' (24 sessions)
summary(net)
#> <htna network>
#>   Method:    relative
#>   Sessions:  429   (max 287 timesteps)
#>   Nodes:     12
#>   Edges:     135 / 144 (non-zero)
#> 
#> Actor types (2):
#>   Human (6 nodes):  Check, Frustrate, Inquire, Refine, Request, Specify
#>   AI    (6 nodes):  Ask, Delegate, Execute, Plan, Repair, Report
#> 
#> Edge counts by actor (rows = source, cols = target):
#>           Human    AI
#>   Human      35    36
#>   AI         36    28
#> 
#> Network metrics:
#>                       metric        value
#>                   Node Count  12.00000000
#>                   Edge Count 135.00000000
#>              Network Density   1.00000000
#>                Mean Distance   0.02743124
#>            Mean Out-Strength   1.00000000
#>              SD Out-Strength   0.00000000
#>             Mean In-Strength   1.00000000
#>               SD In-Strength   0.63085045
#>              Mean Out-Degree  11.25000000
#>                SD Out-Degree   0.96530730
#>  Centralization (Out-Degree)   0.05785124
#>   Centralization (In-Degree)   0.05785124
#>                  Reciprocity   0.96000000
```
