# Build a Heterogeneous Transition Network (HTNA)

Builds a transition network over a combined sequence of two or more
actor groups (e.g. Human and AI) and preserves the actor partition on
the result so downstream plotting and analysis can treat each actor's
codes as a distinct node group.

## Usage

``` r
build_htna(
  data,
  actor_col = NULL,
  action = "code",
  session = "session_id",
  order = "order_in_session",
  method = "relative",
  disambiguate = FALSE,
  ...
)
```

## Arguments

- data:

  Either:

  - A named list of long-format data frames, one per actor (e.g.
    `list(Human = human_long, AI = ai_long)`). All frames must share the
    same column schema.

  - A single long-format data frame with an actor column. In that case
    `actor_col` must be supplied.

- actor_col:

  Character. Name of the actor column when `data` is a single data
  frame. Ignored when `data` is a named list.

- action:

  Character. Name of the action/code column. Default `"code"`.

- session:

  Character. Name of the session column. Default `"session_id"`.

- order:

  Character. Name of the within-session order column. Default
  `"order_in_session"`.

- method:

  Character. Transition method passed to
  [`build_network`](https://rdrr.io/pkg/Nestimate/man/build_network.html):
  `"relative"` (default), `"frequency"`, or `"attention"`.

- disambiguate:

  Logical. If `FALSE` (default), the function errors when a code label
  appears in more than one actor group. If `TRUE`, codes are prefixed
  with the actor name (`"Human:Ask"`, `"AI:Ask"`) so they become
  distinct nodes.

- ...:

  Additional arguments forwarded to
  [`build_network`](https://rdrr.io/pkg/Nestimate/man/build_network.html).

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
[`build_network`](https://rdrr.io/pkg/Nestimate/man/build_network.html).

## See also

[`build_network`](https://rdrr.io/pkg/Nestimate/man/build_network.html),
[`build_tna`](https://rdrr.io/pkg/Nestimate/man/build_tna.html),
[`plot_htna`](https://sonsoles.me/cograph/reference/plot_htna.html)

## Examples

``` r
if (FALSE) { # \dontrun{
data(human_long, ai_long, package = "Nestimate")
net <- build_htna(list(Human = human_long, AI = ai_long))
net$node_groups            # canonical (node, group) schema
cograph::plot_htna(net)    # auto-detects $nodes$groups, no other args
} # }
```
