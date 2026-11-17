# Count recurring paths and meta-paths in the sessions

`extract_meta_paths()` counts the recurring paths and meta-paths in the
sessions of an htna network. In a heterogeneous network every code
belongs to an actor, so each run of codes in a session has two readings:
the concrete path (for example `Request->Plan->Execute`) and its
meta-path, the sequence of actor types it passes through
(`Human->AI->AI`). Meta-paths describe the turn-taking between the
actors, such as how often the human intervenes between two AI actions,
while the concrete paths say what is done in each turn. Counting both
shows which interaction patterns recur and whether they occur more often
than the frequencies of their codes would predict.

`extract_meta_paths()` counts every path of the requested lengths in the
sessions of an htna network. With `level = "state"` (default) it returns
concrete paths, each tagged with its meta-path; with `level = "type"` it
returns the meta-paths, summed over their concrete instances. A `schema`
restricts the search to one template whose elements can be actor types
(any code of that actor), concrete codes, or `"*"` (any element), freely
mixed: `"Human->Ask->*"` is any human code, then the AI code `Ask`, then
anything. With `type = "gapped"`, the elements of a path are `gap + 1`
positions apart instead of adjacent.

It returns a data frame with one row per path and gap. For each path the
table gives its count, the number and share (`support`) of sessions that
contain it, its share among the paths of the same length and gap
(`frequency`), and its `lift`: the count divided by the count expected
if the elements occurred independently with their overall frequencies.

To compare paths between groups of sessions use
[`sequence_compare_htna()`](https://sonsoles.me/htna/reference/sequence_compare_htna.md);
for unordered co-occurrence use
[`association_rules_htna()`](https://sonsoles.me/htna/reference/association_rules_htna.md).

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

  An htna network from
  [`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md).

- level:

  `"state"` (default) for concrete paths with their meta-path, or
  `"type"` for meta-paths only.

- length:

  Path lengths to count (default `2:4`); ignored when `schema` is given.

- schema:

  A template of elements separated by `"->"`; each element is an actor
  type, a code or `"*"`. `NULL` (default) counts all paths.

- type:

  `"contiguous"` (default) for adjacent elements, or `"gapped"` for
  elements `gap + 1` positions apart.

- gap:

  Gap or gaps for `type = "gapped"` (default 1); several values give one
  set of rows per length and gap.

- min_count:

  Minimum number of occurrences (default 1).

- min_support:

  Minimum share of sessions containing the path (default 0).

- min_lift:

  Minimum lift (default 0, no filter).

- start, end, contain:

  Keep only paths whose first element is in `start`, whose last element
  is in `end`, or that contain every element of `contain` (default
  `NULL`); elements are codes or, with `level = "type"`, actor types.

## Value

A data frame of class `c("htna_meta_paths", "htna_paths", "data.frame")`
with one row per path and gap, sorted by length, gap and decreasing
count, and columns `schema` (the path), `meta_schema` (its meta-path;
only with `level = "state"`), `length`, `gap`, `count`, `n_sequences`,
`support`, `frequency` and `lift`. The number of sessions and the level
are kept as attributes and shown by
[`print()`](https://rdrr.io/r/base/print.html).

## Conditions

Errors of class `htna_bad_input` for an unknown element in `schema` or a
zero gap with `type = "gapped"`, and `htna_unassigned_node` when `x` has
no actor partition.

## See also

[`print.htna_paths()`](https://sonsoles.me/htna/reference/print.htna_paths.md),
[`sequence_compare_htna()`](https://sonsoles.me/htna/reference/sequence_compare_htna.md),
[`association_rules_htna()`](https://sonsoles.me/htna/reference/association_rules_htna.md).

## Examples

``` r
data(human_ai)
net <- build_htna(human_ai, actor_type = "actor_type")
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions), 'actor_type' (24 sessions)
extract_meta_paths(net, level = "type", length = 3)
#> Meta-paths (type-level) over 429 sequences
#> Rows: 8 | Lengths: 3 | Gaps: 0
#>               schema length gap count n_seq support frequency lift
#>     Human->AI->Human      3   0  3593   402   0.937     0.194 1.41
#>     Human->Human->AI      3   0  3172   422   0.984     0.172 1.25
#>     AI->Human->Human      3   0  2828   403   0.939     0.153 1.11
#>        AI->Human->AI      3   0  2744   383   0.893     0.148 1.36
#>        Human->AI->AI      3   0  2189   403   0.939     0.118 1.09
#>        AI->AI->Human      3   0  2100   397   0.925     0.114 1.04
#>  Human->Human->Human      3   0  1472   396   0.923     0.080 0.46
#>           AI->AI->AI      3   0   392   165   0.385     0.021 0.25
extract_meta_paths(net, schema = "Human->AI->Human")
#> State-level instances of schema 'Human->AI->Human' over 429 sequences
#> Rows: 163 | Lengths: 3 | Gaps: 0
#>                       schema      meta_schema length gap count n_seq support
#>    Request->Execute->Request Human->AI->Human      3   0   401   194   0.452
#>    Specify->Execute->Request Human->AI->Human      3   0   176   107   0.249
#>        Request->Ask->Request Human->AI->Human      3   0   130    91   0.212
#>      Check->Execute->Request Human->AI->Human      3   0   123    96   0.224
#>        Specify->Ask->Request Human->AI->Human      3   0   120    84   0.196
#>  Specify->Execute->Frustrate Human->AI->Human      3   0   114    88   0.205
#>  Request->Execute->Frustrate Human->AI->Human      3   0   106    88   0.205
#>    Request->Execute->Inquire Human->AI->Human      3   0    97    76   0.177
#>      Specify->Ask->Frustrate Human->AI->Human      3   0    86    70   0.163
#>    Inquire->Execute->Request Human->AI->Human      3   0    73    52   0.121
#>  frequency lift
#>      0.112 5.00
#>      0.049 2.33
#>      0.036 2.19
#>      0.034 3.67
#>      0.033 2.15
#>      0.032 2.57
#>      0.030 2.24
#>      0.027 4.40
#>      0.024 2.61
#>      0.020 3.31
#> ... (153 more)
extract_meta_paths(net, schema = "Human->Ask->*", min_lift = 1)
#> State-level instances of schema 'Human->Ask->*' over 429 sequences
#> Rows: 34 | Lengths: 3 | Gaps: 0
#>                   schema      meta_schema length gap count n_seq support
#>       Specify->Ask->Plan    Human->AI->AI      3   0   340   197   0.459
#>     Frustrate->Ask->Plan    Human->AI->AI      3   0   214   187   0.436
#>       Request->Ask->Plan    Human->AI->AI      3   0   139   106   0.247
#>    Request->Ask->Request Human->AI->Human      3   0   130    91   0.212
#>    Specify->Ask->Request Human->AI->Human      3   0   120    84   0.196
#>  Specify->Ask->Frustrate Human->AI->Human      3   0    86    70   0.163
#>       Inquire->Ask->Plan    Human->AI->AI      3   0    53    45   0.105
#>        Refine->Ask->Plan    Human->AI->AI      3   0    52    46   0.107
#>  Request->Ask->Frustrate Human->AI->Human      3   0    50    44   0.103
#>     Specify->Ask->Refine Human->AI->Human      3   0    47    34   0.079
#>  frequency  lift
#>      0.170 11.65
#>      0.107 11.71
#>      0.070  4.48
#>      0.065  2.19
#>      0.060  2.15
#>      0.043  2.61
#>      0.027  6.22
#>      0.026  6.57
#>      0.025  1.43
#>      0.024  3.29
#> ... (24 more)
extract_meta_paths(net, length = 3, type = "gapped", gap = 1:2)
#> Patterns (state-level) over 429 sequences
#> Rows: 2895 | Lengths: 3 | Gaps: 1, 2
#>                       schema         meta_schema length gap count n_seq support
#>    Execute->Execute->Execute          AI->AI->AI      3   1   271   117   0.273
#>     Request->Frustrate->Plan    Human->Human->AI      3   1   226   217   0.506
#>    Request->Request->Request Human->Human->Human      3   1   216   113   0.263
#>  Specify->Request->Frustrate Human->Human->Human      3   1   147   142   0.331
#>    Execute->Execute->Specify       AI->AI->Human      3   1   140    94   0.219
#>       Specify->Plan->Specify    Human->AI->Human      3   1   135   101   0.235
#>        Specify->Specify->Ask    Human->Human->AI      3   1   122   109   0.254
#>        Specify->Ask->Request    Human->AI->Human      3   1   118   106   0.247
#>    Execute->Specify->Request    AI->Human->Human      3   1   118    86   0.200
#>        Request->Specify->Ask    Human->Human->AI      3   1   113   107   0.249
#>  frequency  lift
#>      0.015  3.22
#>      0.013 10.09
#>      0.012  2.97
#>      0.008  3.64
#>      0.008  1.85
#>      0.008  4.01
#>      0.007  2.43
#>      0.007  2.21
#>      0.007  1.64
#>      0.006  2.12
#> ... (2885 more)
```
