# Extract Meta-Paths from a Heterogeneous Transition Network

Discovers type-level patterns (meta-paths) over the actor-typed
sequences that produced a heterogeneous transition network. A meta-path
is a fixed sequence of node types - for example `Human->AI->Human` -
that abstracts away from concrete states and asks how often a *typed*
template appears in the data. Two regimes are supported: enumerate every
meta-path of given lengths (default), or search for one specific schema
(with optional wildcards).

## Usage

``` r
extract_meta_paths(
  x,
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
  [`build_htna()`](https://mohsaqr.github.io/htna/reference/build_htna.md).
  Must have `$nodes$groups`, `$node_groups`, and `$data` populated.

- length:

  \[[`integer()`](https://rdrr.io/r/base/integer.html): `2:4`\]  
  Meta-path lengths to enumerate. Ignored when `schema` is supplied.

- schema:

  \[`character(1)`\]  
  Optional. A meta-path template to search for, written as actor names
  separated by `"->"` (e.g. `"Human->AI->Human"`). Wildcards are
  supported: `"Human->*->Human"` matches every length-3 meta-path that
  starts and ends with Human, with any type between.

- type:

  \[`character(1)`: `"contiguous"`\]  
  `"contiguous"` (default) considers consecutive positions; `"gapped"`
  considers positions spaced by `gap + 1` apart.

- gap:

  \[[`integer()`](https://rdrr.io/r/base/integer.html): `1L`\]  
  Gap size(s) used when `type = "gapped"`. Multiple values produce one
  row per (length, gap) combination, distinguished by the `gap` column.

- min_count:

  \[`integer(1)`: `1L`\]  
  Minimum total occurrences to retain.

- min_support:

  \[`numeric(1)`: `0`\]  
  Minimum proportion of sequences containing the meta-path at least
  once.

- min_lift:

  \[`numeric(1)`: `0`\]  
  Minimum lift (observed / expected under marginal independence). `0`
  disables the filter.

- start:

  \[[`character()`](https://rdrr.io/r/base/character.html)\]  
  Keep only meta-paths whose first type is in this set.

- end:

  \[[`character()`](https://rdrr.io/r/base/character.html)\]  
  Keep only meta-paths whose last type is in this set.

- contain:

  \[[`character()`](https://rdrr.io/r/base/character.html)\]  
  Keep only meta-paths whose type sequence contains all elements of this
  set (in any order).

## Value

An object of class `c("htna_meta_paths", "htna_paths", "data.frame")`
with columns: `schema`, `length`, `gap`, `count`, `n_sequences`,
`support`, `frequency`, `lift`. Attributes: `n_sequences`, `alphabet`,
`level = "meta"`.

## Details

Operates on the actor partition stored on the network by
[`build_htna()`](https://mohsaqr.github.io/htna/reference/build_htna.md).

## See also

[`build_htna()`](https://mohsaqr.github.io/htna/reference/build_htna.md).

## Examples

``` r
if (FALSE) { # \dontrun{
data(human_long, ai_long, package = "Nestimate")
net <- build_htna(list(Human = human_long, AI = ai_long))

# All meta-paths of length 2..4
extract_meta_paths(net)

# Wildcard family search: any length-3 path that returns to Human
extract_meta_paths(net, schema = "Human->*->Human")

# Gapped meta-paths
extract_meta_paths(net, length = 3, type = "gapped", gap = 1:2)

# Lift threshold (over-represented relative to type marginals)
extract_meta_paths(net, length = 3, min_lift = 1.2)
} # }
```
