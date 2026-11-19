# Summarize a heterogeneous transition network

[`summary()`](https://rdrr.io/r/base/summary.html) summarizes a
heterogeneous transition network by its actor partition;
[`print()`](https://rdrr.io/r/base/print.html) shows the summary and
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) returns
its tables. A heterogeneous transition network is described first by
which codes belong to which actor type, and how the transitions between
codes fall within and between the actors. The summary reports the
network's size (sessions, nodes and non-zero edges), the nodes of each
actor, the number of non-zero edges in every directed actor-pair block
(for example Human to AI), and the standard network-level descriptive
metrics (density, mean distance, strength, degree centralization and
reciprocity). In an undirected network (`method = "co_occurrence"`)
every edge is counted once, so the blocks are the unordered actor pairs.

[`summary()`](https://rdrr.io/r/base/summary.html) returns the summary
as a `summary.htna` object; printing it shows the report. For a grouped
network, the summary of every cohort is returned and printed in turn.
The tables of the summary are returned as data frames by
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html).

## Usage

``` r
# S3 method for class 'htna'
summary(object, max_nodes = 12L, ...)

# S3 method for class 'htna_group'
summary(object, max_nodes = 12L, ...)

# S3 method for class 'summary.htna'
print(x, max_nodes = attr(x, "max_nodes") %||% 12L, ...)

# S3 method for class 'summary.htna_group'
print(x, ...)

# S3 method for class 'summary.htna'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  ...,
  what = c("actors", "actor_pairs", "metrics")
)

# S3 method for class 'summary.htna_group'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  ...,
  what = c("actors", "actor_pairs", "metrics")
)
```

## Arguments

- object:

  An htna network from
  [`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md) or
  an `htna_group`.

- max_nodes:

  Maximum number of nodes listed per actor type when the summary is
  printed (default 12).

- ...:

  Unused.

- x:

  A `summary.htna` or `summary.htna_group` object.

- row.names, optional:

  Ignored; present for the generic.

- what:

  Table to return: `"actors"` (default; one row per actor type with its
  number of nodes and their names), `"actor_pairs"` (one row per
  directed actor pair with the number of non-zero edges; one row per
  unordered pair for an undirected network) or `"metrics"` (one row per
  network metric).

## Value

[`summary()`](https://rdrr.io/r/base/summary.html) returns an object of
class `summary.htna`: a list with `actors` (data frame: `actor`,
`n_nodes`, `nodes`), `edges_by_actor` (integer matrix of non-zero edge
counts, source actors in rows; symmetric for an undirected network),
`network_metrics` (data frame: `metric`, `value`), and the scalars
`n_nodes`, `n_edges`, `directed`, `n_sessions`, `n_timesteps` and
`method`. For an `htna_group` it returns a `summary.htna_group`, a named
list of `summary.htna` objects.
[`print()`](https://rdrr.io/r/base/print.html) returns its input
invisibly.
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) returns
the table chosen by `what`; for a grouped summary the cohorts are
stacked with a leading `group` column.

## See also

[`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md),
[`as.data.frame.htna()`](https://sonsoles.me/htna/reference/as.data.frame.htna.md)
for the edge and node tables of the network itself.

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
as.data.frame(summary(net), what = "actor_pairs")
#>   source_actor target_actor n_edges
#> 1        Human        Human      35
#> 2        Human           AI      36
#> 3           AI        Human      36
#> 4           AI           AI      28

grp <- build_htna(human_ai, actor_type = "actor_type", group = "phase")
#> Metadata aggregated per session: ties resolved by first occurrence in 'cluster' (24 sessions), 'actor_type' (9 sessions)
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (18 sessions), 'actor_type' (15 sessions)
as.data.frame(summary(grp), what = "metrics")
#>    group                      metric        value
#> 1  Early                  Node Count 1.200000e+01
#> 2  Early                  Edge Count 1.350000e+02
#> 3  Early             Network Density 1.000000e+00
#> 4  Early               Mean Distance 2.704448e-02
#> 5  Early           Mean Out-Strength 1.000000e+00
#> 6  Early             SD Out-Strength 0.000000e+00
#> 7  Early            Mean In-Strength 1.000000e+00
#> 8  Early              SD In-Strength 6.596948e-01
#> 9  Early             Mean Out-Degree 1.125000e+01
#> 10 Early               SD Out-Degree 9.653073e-01
#> 11 Early Centralization (Out-Degree) 5.785124e-02
#> 12 Early  Centralization (In-Degree) 5.785124e-02
#> 13 Early                 Reciprocity 9.600000e-01
#> 14  Late                  Node Count 1.200000e+01
#> 15  Late                  Edge Count 1.340000e+02
#> 16  Late             Network Density 1.000000e+00
#> 17  Late               Mean Distance 2.438201e-02
#> 18  Late           Mean Out-Strength 1.000000e+00
#> 19  Late             SD Out-Strength 3.347448e-17
#> 20  Late            Mean In-Strength 1.000000e+00
#> 21  Late              SD In-Strength 6.109845e-01
#> 22  Late             Mean Out-Degree 1.116667e+01
#> 23  Late               SD Out-Degree 9.374369e-01
#> 24  Late Centralization (Out-Degree) 5.785124e-02
#> 25  Late  Centralization (In-Degree) 5.785124e-02
#> 26  Late                 Reciprocity 9.600000e-01
```
