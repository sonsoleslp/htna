# Tabulate the edges and nodes of an htna network

[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) returns
the edges or the nodes of a heterogeneous transition network as a tidy
data frame in which every row carries the actor type of its codes. The
edge table lists every non-zero transition with its weight and the actor
types of its source and target, so the within-actor and between-actor
transitions (for example Human to AI) can be read, counted or filtered
directly. The node table lists every code with its actor type.

For a grouped network the tables of all cohorts are stacked with a
leading `group` column. Self-transitions (the diagonal of the weight
matrix) are kept. For an undirected network (`method = "co_occurrence"`)
each pair of codes is listed once.

## Usage

``` r
# S3 method for class 'htna'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  ...,
  what = c("edges", "nodes")
)

# S3 method for class 'htna_group'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  ...,
  what = c("edges", "nodes")
)
```

## Arguments

- x:

  An htna network from
  [`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md) or
  an `htna_group`.

- row.names, optional:

  Ignored; present for the generic.

- ...:

  Unused.

- what:

  `"edges"` (default) or `"nodes"`.

## Value

A data frame. For `what = "edges"`: one row per non-zero edge, with
columns `from`, `to`, `weight`, `source_actor` and `target_actor`
(factors in the network's actor order), sorted by source then target in
node order. For `what = "nodes"`: one row per node, with columns `node`
and `actor`. Grouped networks add a first column `group`.

## See also

[`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md),
[`summary.htna()`](https://sonsoles.me/htna/reference/summary.htna.md)
for counts per actor pair.

## Examples

``` r
data(human_ai)
net <- build_htna(human_ai, actor_type = "actor_type")
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions), 'actor_type' (24 sessions)
head(as.data.frame(net))
#>   from        to     weight source_actor target_actor
#> 1  Ask       Ask 0.01823579           AI           AI
#> 2  Ask     Check 0.06318914           AI        Human
#> 3  Ask   Execute 0.02205259           AI           AI
#> 4  Ask Frustrate 0.11577608           AI        Human
#> 5  Ask   Inquire 0.06318914           AI        Human
#> 6  Ask      Plan 0.40882103           AI           AI
as.data.frame(net, what = "nodes")
#>         node actor
#> 1        Ask    AI
#> 2      Check Human
#> 3   Delegate    AI
#> 4    Execute    AI
#> 5  Frustrate Human
#> 6    Inquire Human
#> 7       Plan    AI
#> 8     Refine Human
#> 9     Repair    AI
#> 10    Report    AI
#> 11   Request Human
#> 12   Specify Human
```
