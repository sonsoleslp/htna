# Simplified AI Interaction Sequences (per-actor frame)

The AI-only slice of
[`human_ai`](https://sonsoles.me/htna/reference/human_ai.md), in a
long-format data frame ready to feed the named-list form of
[`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md).
Codes have already been collapsed into the simplified AI alphabet; see
[`human_ai`](https://sonsoles.me/htna/reference/human_ai.md) for the
remapping rules.

## Usage

``` r
ai_simplified
```

## Format

A data frame with 8551 rows and 10 columns. Schema matches
[`human_ai`](https://sonsoles.me/htna/reference/human_ai.md) minus the
`phase` column; every value in `actor_type` is `"AI"`.

## Source

Derived from
[`Nestimate::ai_long`](https://pak.dynasite.org/Nestimate/reference/long-data.html);
see `data-raw/human_ai.R` for the build script.

## See also

[`human_ai`](https://sonsoles.me/htna/reference/human_ai.md),
[`human_simplified`](https://sonsoles.me/htna/reference/human_simplified.md),
[`human_ai_codebook`](https://sonsoles.me/htna/reference/human_ai_codebook.md).

## Examples

``` r
data(human_simplified, ai_simplified)
net <- build_htna(list(Human = human_simplified, AI = ai_simplified))
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
