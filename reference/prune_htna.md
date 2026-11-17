# Remove weak edges from a heterogeneous transition network

`prune_htna()` removes weak edges from a heterogeneous transition
network so that the plot and the descriptive measures focus on the
transitions that carry the process. A relative transition network
estimated from many sessions is usually dense: almost every code is
followed by almost every other code at least once, and most of these
transitions are rare. Because the weights are kept in the network and
only set aside, pruning can be undone or re-applied.

`prune_htna()` sets to zero the edges that fail a rule: weights below a
fixed `threshold` (`method = "threshold"`, the default), the `lowest`
share of the weights (`method = "lowest"`), or edges that are not
supported by a bootstrap (`method = "bootstrap"`, with `boot` from
[`bootstrap_htna()`](https://sonsoles.me/htna/reference/bootstrap_htna.md)).
It runs Nestimate's `net_prune()` with the actor partition kept, so
[`plot_htna()`](https://sonsoles.me/htna/reference/plot_htna.md) and the
other functions work on the pruned network unchanged. It returns an htna
network with the removed edges set to zero and the rule recorded.

[`deprune_htna()`](https://sonsoles.me/htna/reference/deprune_htna.md)
restores the removed edges,
[`reprune_htna()`](https://sonsoles.me/htna/reference/reprune_htna.md)
removes them again, and
[`pruning_details_htna()`](https://sonsoles.me/htna/reference/pruning_details_htna.md)
lists them.

## Usage

``` r
prune_htna(x, ...)
```

## Arguments

- x:

  An htna network from
  [`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md).

- ...:

  Pruning rule: `method` (`"threshold"`, default, `"lowest"` or
  `"bootstrap"`), `threshold` (default 0.1), `lowest` (default 0.05),
  `level` (default 0.5) and `boot` (a bootstrap result for
  `method = "bootstrap"`).

## Value

An htna network of the same class and structure as
[`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md)
returns, with the removed edges set to zero in its weight matrix and the
rule recorded for
[`deprune_htna()`](https://sonsoles.me/htna/reference/deprune_htna.md),
[`reprune_htna()`](https://sonsoles.me/htna/reference/reprune_htna.md)
and
[`pruning_details_htna()`](https://sonsoles.me/htna/reference/pruning_details_htna.md).

## See also

[`deprune_htna()`](https://sonsoles.me/htna/reference/deprune_htna.md),
[`reprune_htna()`](https://sonsoles.me/htna/reference/reprune_htna.md),
[`pruning_details_htna()`](https://sonsoles.me/htna/reference/pruning_details_htna.md),
[`bootstrap_htna()`](https://sonsoles.me/htna/reference/bootstrap_htna.md).

## Examples

``` r
data(human_ai)
net <- build_htna(human_ai, actor_type = "actor_type")
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions), 'actor_type' (24 sessions)
pruned <- prune_htna(net, threshold = 0.05)
summary(pruned)
#> <htna network>
#>   Method:    relative
#>   Sessions:  429   (max 287 timesteps)
#>   Nodes:     12
#>   Edges:     73 / 144 (non-zero)
#> 
#> Actor types (2):
#>   Human (6 nodes):  Check, Frustrate, Inquire, Refine, Request, Specify
#>   AI    (6 nodes):  Ask, Delegate, Execute, Plan, Repair, Report
#> 
#> Edge counts by actor (rows = source, cols = target):
#>           Human    AI
#>   Human      19    14
#>   AI         27    13
#> 
#> Network metrics:
#>                       metric       value
#>                   Node Count 12.00000000
#>                   Edge Count 73.00000000
#>              Network Density  0.55303030
#>                Mean Distance  0.16539036
#>            Mean Out-Strength  0.88699823
#>              SD Out-Strength  0.05614162
#>             Mean In-Strength  0.88699823
#>               SD In-Strength  0.67704425
#>              Mean Out-Degree  6.08333333
#>                SD Out-Degree  1.88092498
#>  Centralization (Out-Degree)  0.27272727
#>   Centralization (In-Degree)  0.37190083
#>                  Reciprocity  0.53968254
```
