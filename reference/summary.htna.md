# Summarise a Heterogeneous Transition Network

Prints a per-actor summary of an htna network: which nodes belong to
which actor type and how the non-zero edges distribute across the actor
partition. The result is also returned invisibly as a list so callers
can inspect the structured summary programmatically.

## Usage

``` r
# S3 method for class 'htna'
summary(object, max_nodes = 12L, ...)

# S3 method for class 'htna_group'
summary(object, max_nodes = 12L, ...)
```

## Arguments

- object:

  An htna network from
  [`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md) or
  an `htna_group`.

- max_nodes:

  Integer. Maximum number of nodes to list per actor type before
  truncating with an ellipsis. Default `12`.

- ...:

  Forwarded for compatibility; currently unused.

## Value

Invisibly, a list with components:

- `actors` - data frame with one row per actor type (`actor`, `n_nodes`,
  `nodes`).

- `edges_by_actor` - integer matrix of non-zero edge counts, rows are
  source actor, columns are target actor.

- `network_metrics` - data frame (columns `metric`, `value`) of standard
  network-level descriptive metrics from the Nestimate engine (node/edge
  counts, density, distance, strength, degree centralization, and
  reciprocity).

- `n_nodes`, `n_edges`, `n_sessions`, `n_timesteps`, `method`.

## Details

For an `htna_group`, the summary is printed once per cohort and the
return value is a named list of the per-cohort lists described under
Value.

## Examples

``` r
data(human_ai)
net <- build_htna(human_ai, actor_type = "actor_type")
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
