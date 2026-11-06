# Build a Heterogeneous Transition Network (HTNA)

Builds a transition network over a combined sequence of two or more
actor groups (e.g. Human and AI) and preserves the actor partition on
the result so downstream plotting and analysis can treat each actor's
codes as a distinct node group.

## Usage

``` r
build_htna(
  data,
  actor_type = NULL,
  actor = NULL,
  node_groups = NULL,
  action = "code",
  session = "session_id",
  order = "order_in_session",
  method = "relative",
  group = NULL,
  disambiguate = FALSE,
  source_data = NULL,
  ...
)
```

## Arguments

- data:

  Either:

  - A named list of long-format data frames, one per actor type (e.g.
    `list(Human = human_simplified, AI = ai_simplified)`). All frames
    must share the same column schema.

  - A single long-format data frame. In that case either `actor_type`
    (row-level actor-type IDs) or `node_groups` (node-level actor-type
    lookup) must be supplied.

  - A fitted clustering result from
    [`Nestimate::build_clusters()`](https://pak.dynasite.org/Nestimate/reference/build_clusters.html),
    [`Nestimate::cluster_mmm()`](https://pak.dynasite.org/Nestimate/reference/cluster_mmm.html)
    (a `net_mmm`), or
    [`tna::cluster_sequences()`](https://sonsoles.me/tna/reference/cluster_data.html),
    or an already-materialized `netobject_group`. One HTNA network is
    returned per sequence cluster. The clustering fit is not rerun;
    assignments, diagnostics, fitted MMM weights, and actor metadata are
    preserved.

  - A fitted
    [`Nestimate::build_mcml()`](https://pak.dynasite.org/Nestimate/reference/build_mcml.html)
    object. Its node clusters become the HTNA actor groups and one full
    node-level network is rebuilt from the original source, including
    transitions between clusters.

- actor_type:

  Character. Name of the column tagging each row's actor type / group
  (e.g. `"Human"` vs `"AI"`) when `data` is a single data frame. Ignored
  when `data` is a named list or when `node_groups` is supplied.

- actor:

  Character. Name of an optional column identifying the individual actor
  that performed each event (e.g. a learner / user id). Forwarded to
  [`build_network`](https://pak.dynasite.org/Nestimate/reference/build_network.html)
  with the same semantics; orthogonal to `actor_type`, which encodes the
  group/type partition over codes.

- node_groups:

  Node-to-actor-type lookup, in either of two forms:

  - A **named list** mapping actor-type labels to character vectors of
    code names, e.g.
    `list(Human = c("Specify", "Command"), AI = c("Plan", "Execute"))`.

  - A **2-column data frame** with one column named after `action` (the
    codes) and one other column tagging each code with its actor type,
    e.g.
    `data.frame(code = c("Specify", "Plan"), actor_type = c("Human", "AI"))`.
    The actor-type ordering follows the column's factor levels or, for
    character vectors, the order of first appearance.

  Use `node_groups` when `data` is a single long-format frame with no
  actor-type column and you want to declare the node-to-type partition
  directly. Each code in `data[[action]]` must appear in exactly one
  entry. For clustering inputs, the canonical two-column
  `data.frame(node, group)` stored on an existing HTNA model is also
  accepted. The argument may be omitted when Nestimate preserved the
  partition from an HTNA input. Mutually exclusive with `actor_type`.
  For an `mcml` input, this optionally overrides `$cluster_members`;
  otherwise the fitted membership is used.

- action:

  Character. Name of the action/code column. Default `"code"`.

- session:

  Character. Name of the session column. Default `"session_id"`.

- order:

  Character. Name of the within-session order column. Default
  `"order_in_session"`.

- method:

  Character. Transition method passed to
  [`build_network`](https://pak.dynasite.org/Nestimate/reference/build_network.html):
  `"relative"` (default), `"frequency"`, `"co_occurrence"`, or
  `"attention"`. Co-occurrence models are undirected and preserve the
  same actor partition as transition models.

- group:

  Character. Optional name of a column in `data` used to split sessions
  into cohorts (e.g. `"cluster"`). When supplied, one network is built
  per group level and the result is a named list of htna networks with
  class `c("htna_group", "netobject_group", "list")`. Pass that object
  as it is to the downstream verbs; they iterate over the cohorts.

- disambiguate:

  Logical. If `FALSE` (default), the function errors when a code label
  appears in more than one actor-type group. If `TRUE`, codes are
  prefixed with the actor-type label (`"Human:Ask"`, `"AI:Ask"`) so they
  become distinct nodes.

- source_data:

  Original node-level data for an `mcml` input. Usually unnecessary when
  [`Nestimate::build_mcml()`](https://pak.dynasite.org/Nestimate/reference/build_mcml.html)
  retained its sequence source. Required when the fitted object has no
  expandable source, such as an `mcml` built from a transition matrix or
  edge list. The fitted macro and within-cluster weights are
  deliberately not stitched together: rebuilding from this source is
  what preserves cross-cluster transitions.

- ...:

  Additional arguments forwarded to
  [`build_network`](https://pak.dynasite.org/Nestimate/reference/build_network.html).

## Value

An object of class `c("htna", "netobject", "cograph_network")`: a
Nestimate `netobject` with the actor partition stored in cograph's
canonical schema, so `cograph::plot_htna(net)` auto-detects the groups
with no further arguments:

- `$nodes$groups` - actor label per node (the column name
  [`cograph::plot_htna`](https://sonsoles.me/cograph/reference/plot_htna.html)
  auto-detects).

- `$node_groups` - data frame with columns `node` and `group` (canonical
  `cograph_network` schema, also readable by
  [`cograph::get_groups()`](https://sonsoles.me/cograph/reference/get_nodes.html),
  [`Nestimate::cluster_summary()`](https://pak.dynasite.org/Nestimate/reference/cluster_summary.html),
  and the `print` method for `cograph_network`).

- `$actor_levels` - declared actor ordering, also retained as metadata
  on `$node_groups` so the table can be passed back to `build_htna()`
  without losing that order.

All other slots are exactly as returned by
[`build_network`](https://pak.dynasite.org/Nestimate/reference/build_network.html).
A `group` argument or a clustering input returns an `htna_group` (a
named list with one `htna` per cohort or cluster); its children retain
the actor partition and its outer clustering assignments, posterior
probabilities, diagnostics, and other attributes are preserved. An
`mcml` input instead returns one `htna`, equivalent to
[`Nestimate::as_htna()`](https://pak.dynasite.org/Nestimate/reference/as_htna.html).

## See also

[`build_network`](https://pak.dynasite.org/Nestimate/reference/build_network.html),
[`build_tna`](https://pak.dynasite.org/Nestimate/reference/build_tna.html),
[`summary.htna`](https://sonsoles.me/htna/reference/summary.htna.md),
[`plot_htna`](https://sonsoles.me/htna/reference/plot_htna.md) (which
renders through
[`plot_htna`](https://sonsoles.me/cograph/reference/plot_htna.html))

## Examples

``` r
data(human_ai, human_simplified, ai_simplified, human_ai_codebook)

# Form 1: named list of per-actor frames
net <- build_htna(list(Human = human_simplified, AI = ai_simplified))
#> A network with one long sequence is not recommended and can't be validated using bootstrap and other confirmatory testings.
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

# Form 2: single combined frame with a row-level actor-type tag
net <- build_htna(human_ai, actor_type = "actor_type")
#> A network with one long sequence is not recommended and can't be validated using bootstrap and other confirmatory testings.
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions), 'actor_type' (24 sessions)

# Form 3: combined frame plus a code-to-actor codebook
net <- build_htna(human_ai, node_groups = human_ai_codebook)
#> A network with one long sequence is not recommended and can't be validated using bootstrap and other confirmatory testings.
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions), 'actor_type' (24 sessions)

# One network per cohort
grp <- build_htna(human_ai, actor_type = "actor_type", group = "phase")
#> A network with one long sequence is not recommended and can't be validated using bootstrap and other confirmatory testings.
#> Metadata aggregated per session: ties resolved by first occurrence in 'cluster' (24 sessions), 'actor_type' (9 sessions)
#> A network with one long sequence is not recommended and can't be validated using bootstrap and other confirmatory testings.
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (18 sessions), 'actor_type' (15 sessions)
summary(grp)
#> <htna_group: 2 cohorts (Late, Early)>
#> 
#> === Late ===
#> <htna network>
#>   Method:    relative
#>   Sessions:  215   (max 208 timesteps)
#>   Nodes:     12
#>   Edges:     134 / 144 (non-zero)
#> 
#> Actor types (2):
#>   Human (6 nodes):  Check, Frustrate, Inquire, Refine, Request, Specify
#>   AI    (6 nodes):  Ask, Delegate, Execute, Plan, Repair, Report
#> 
#> Edge counts by actor (rows = source, cols = target):
#>           Human    AI
#>   Human      35    36
#>   AI         36    27
#> 
#> Network metrics:
#>                       metric        value
#>                   Node Count 1.200000e+01
#>                   Edge Count 1.340000e+02
#>              Network Density 1.000000e+00
#>                Mean Distance 2.438201e-02
#>            Mean Out-Strength 1.000000e+00
#>              SD Out-Strength 3.347448e-17
#>             Mean In-Strength 1.000000e+00
#>               SD In-Strength 6.109845e-01
#>              Mean Out-Degree 1.116667e+01
#>                SD Out-Degree 9.374369e-01
#>  Centralization (Out-Degree) 5.785124e-02
#>   Centralization (In-Degree) 5.785124e-02
#>                  Reciprocity 9.600000e-01
#> 
#> === Early ===
#> <htna network>
#>   Method:    relative
#>   Sessions:  214   (max 287 timesteps)
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
#>                Mean Distance   0.02704448
#>            Mean Out-Strength   1.00000000
#>              SD Out-Strength   0.00000000
#>             Mean In-Strength   1.00000000
#>               SD In-Strength   0.65969479
#>              Mean Out-Degree  11.25000000
#>                SD Out-Degree   0.96530730
#>  Centralization (Out-Degree)   0.05785124
#>   Centralization (In-Degree)   0.05785124
#>                  Reciprocity   0.96000000
```
