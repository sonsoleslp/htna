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
  ...
)
```

## Arguments

- data:

  Either:

  - A named list of long-format data frames, one per actor type (e.g.
    `list(Human = human_long, AI = ai_long)`). All frames must share the
    same column schema.

  - A single long-format data frame. In that case either `actor_type`
    (row-level actor-type IDs) or `node_groups` (node-level actor-type
    lookup) must be supplied.

- actor_type:

  Character. Name of the column tagging each row's actor type / group
  (e.g. `"Human"` vs `"AI"`) when `data` is a single data frame. Ignored
  when `data` is a named list or when `node_groups` is supplied.

- actor:

  Character. Name of an optional column identifying the individual actor
  that performed each event (e.g. a learner / user id). Forwarded to
  [`build_network`](https://saqr.me/Nestimate/reference/build_network.html)
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
  entry. Mutually exclusive with `actor_type`.

- action:

  Character. Name of the action/code column. Default `"code"`.

- session:

  Character. Name of the session column. Default `"session_id"`.

- order:

  Character. Name of the within-session order column. Default
  `"order_in_session"`.

- method:

  Character. Transition method passed to
  [`build_network`](https://saqr.me/Nestimate/reference/build_network.html):
  `"relative"` (default), `"frequency"`, or `"attention"`.

- group:

  Character. Optional name of a column in `data` used to split sessions
  into cohorts (e.g. `"cluster"`). When supplied, one network is built
  per group level and the result is a named list of htna networks with
  class `c("htna_group", "netobject_group")`.

- disambiguate:

  Logical. If `FALSE` (default), the function errors when a code label
  appears in more than one actor-type group. If `TRUE`, codes are
  prefixed with the actor-type label (`"Human:Ask"`, `"AI:Ask"`) so they
  become distinct nodes.

- ...:

  Additional arguments forwarded to
  [`build_network`](https://saqr.me/Nestimate/reference/build_network.html).

## Value

A `netobject` with the actor partition stored in cograph's canonical
schema, so `cograph::plot_htna(net)` auto-detects the groups with no
further arguments:

- `$nodes$groups` - actor label per node (the column name
  [`cograph::plot_htna`](https://sonsoles.me/cograph/reference/plot_htna.html)
  auto-detects).

- `$node_groups` - data frame with columns `node` and `group` (canonical
  `cograph_network` schema, also readable by
  [`cograph::get_groups`](https://sonsoles.me/cograph/reference/get_groups.html),
  `cluster_summary`, and the `print` method for `cograph_network`).

All other slots are exactly as returned by
[`build_network`](https://saqr.me/Nestimate/reference/build_network.html).

## See also

[`build_network`](https://saqr.me/Nestimate/reference/build_network.html),
[`build_tna`](https://saqr.me/Nestimate/reference/build_tna.html),
[`plot_htna`](https://sonsoles.me/cograph/reference/plot_htna.html)

## Examples

``` r
if (FALSE) { # \dontrun{
data(human_long, ai_long, package = "Nestimate")
net <- build_htna(list(Human = human_long, AI = ai_long))
net$node_groups            # canonical (node, group) schema
cograph::plot_htna(net)    # auto-detects $nodes$groups, no other args

# Single combined frame with row-level actor-type tag and an
# individual-actor id forwarded to Nestimate.
df <- rbind(transform(human_long, actor_type = "Human"),
            transform(ai_long,    actor_type = "AI"))
net <- build_htna(df, actor_type = "actor_type", actor = "session_id")
} # }
```
