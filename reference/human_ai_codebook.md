# Code-to-actor codebook for the vibe-coding data

`human_ai_codebook` is the codebook of
[human_ai](https://sonsoles.me/htna/reference/human_ai.md): the actor
type of each of its twelve codes. A codebook of this form declares the
actor partition when the event data have no actor-type column: it is
passed to
[`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md) as
`node_groups`.

## Usage

``` r
human_ai_codebook
```

## Format

A data frame with 12 rows, one per code, and 2 columns:

- code:

  Simplified interaction code.

- actor_type:

  `"Human"` or `"AI"`.

## Source

Derived from
[human_simplified](https://sonsoles.me/htna/reference/human_simplified.md)
and
[ai_simplified](https://sonsoles.me/htna/reference/ai_simplified.md).

## See also

[human_ai](https://sonsoles.me/htna/reference/human_ai.md),
[`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md)

## Examples

``` r
data(human_ai, human_ai_codebook)
human_ai_codebook
#>         code actor_type
#> 1    Specify      Human
#> 2    Request      Human
#> 3  Frustrate      Human
#> 4      Check      Human
#> 5    Inquire      Human
#> 6     Refine      Human
#> 7   Delegate         AI
#> 8       Plan         AI
#> 9    Execute         AI
#> 10       Ask         AI
#> 11    Repair         AI
#> 12    Report         AI
net <- build_htna(human_ai, node_groups = human_ai_codebook)
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
