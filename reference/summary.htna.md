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
if (FALSE) { # \dontrun{
data(human_long, ai_long, package = "Nestimate")
net <- build_htna(list(Human = human_long, AI = ai_long))
summary(net)
} # }
```
