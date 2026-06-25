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

- `n_nodes`, `n_edges`, `n_sessions`, `n_timesteps`, `method`.

## Examples

``` r
data(human_ai)
net <- build_htna(human_ai, actor_type = "actor_type")
#> Warning: A network with one long sequence is not recommended and can't be validated using bootstrap and other confirmatory testings.
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
```
