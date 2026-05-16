# Extract Path Patterns from a Heterogeneous Transition Network

Discovers recurring patterns in the actor-typed sequences that produced
a heterogeneous transition network. Operates at two levels:

## Usage

``` r
extract_meta_paths(
  x,
  level = c("state", "type"),
  length = 2:4,
  schema = NULL,
  type = c("contiguous", "gapped"),
  gap = 1L,
  min_count = 1L,
  min_support = 0,
  min_lift = 0,
  start = NULL,
  end = NULL,
  contain = NULL
)
```

## Arguments

- x:

  \[`htna_network`\]  
  A network built with
  [`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md).
  Must have `$nodes$groups`, `$node_groups`, and `$data` populated.

- level:

  \[`character(1)`: `"state"`\]  
  `"state"` returns concrete state-level patterns with a `meta_schema`
  rollup column. `"type"` returns the type-level meta-path summary.

- length:

  \[[`integer()`](https://rdrr.io/r/base/integer.html): `2:4`\]  
  Pattern lengths to enumerate. Ignored when `schema` is supplied.

- schema:

  \[`character(1)`\]  
  Optional. A path template written as elements separated by `"->"`.
  Each element can be a type name, a concrete state, or `"*"`. See
  Details.

- type:

  \[`character(1)`: `"contiguous"`\]  
  `"contiguous"` (default) considers consecutive positions; `"gapped"`
  considers positions spaced by `gap + 1` apart.

- gap:

  \[[`integer()`](https://rdrr.io/r/base/integer.html): `1L`\]  
  Gap size(s) used when `type = "gapped"`. Multiple values produce one
  row per (length, gap) combination.

- min_count:

  \[`integer(1)`: `1L`\]  
  Minimum total occurrences to retain.

- min_support:

  \[`numeric(1)`: `0`\]  
  Minimum proportion of sequences containing the pattern at least once.

- min_lift:

  \[`numeric(1)`: `0`\]  
  Minimum lift (observed / expected under marginal independence). `0`
  disables the filter.

- start, end, contain:

  \[[`character()`](https://rdrr.io/r/base/character.html)\]  
  Filter rows whose first / last element is in `start` / `end`, or whose
  element sequence contains all of `contain`. Compared against the
  alphabet of the chosen level (types or concrete states).

## Value

An object of class `c("htna_meta_paths", "htna_paths", "data.frame")`.
At `level = "state"` the columns are `schema`, `meta_schema`, `length`,
`gap`, `count`, `n_sequences`, `support`, `frequency`, `lift`. At
`level = "type"` the `meta_schema` column is omitted (the `schema`
column already holds the type pattern). Attributes: `n_sequences`,
`alphabet`, `level`, and (when supplied) `schema`.

## Details

- `level = "state"` (default) - enumerate concrete state-level patterns
  (e.g. `Command->Execute->Command`) and annotate each row with the
  type-level template it instantiates (e.g. `Human->AI->Human`).

- `level = "type"` - enumerate type-level meta-paths only (one row per
  actor-type pattern, summed over its concrete instances).

At either level, a `schema` can filter the search. Schema parts can be
type names (expand to every concrete code in that group), concrete
states, or `"*"` (any element). Parts can be mixed freely - e.g.
`"Human->Ask->Human"` means "any Human code, then `Ask`, then any Human
code". At `level = "type"` concrete codes resolve to their type, so the
same schema becomes `Human->AI->Human`.

Operates on the actor partition stored on the network by
[`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md).

## See also

[`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md).

## Examples

``` r
if (FALSE) { # \dontrun{
data(human_long, ai_long, package = "Nestimate")
net <- build_htna(list(Human = human_long, AI = ai_long))

# Concrete state-level patterns of length 2..4 (default)
extract_meta_paths(net)

# Type-level meta-path summary
extract_meta_paths(net, level = "type")

# Concrete instances of a type-level template
extract_meta_paths(net, schema = "Human->AI->Human")

# Mix types, concrete codes, and wildcards
extract_meta_paths(net, schema = "Human->Ask->Human")
extract_meta_paths(net, schema = "Human->*->Human")

# Gapped patterns; lift threshold
extract_meta_paths(net, length = 3, type = "gapped", gap = 1:2)
extract_meta_paths(net, length = 3, min_lift = 1.2)
} # }
```
