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
# \donttest{
data(human_ai)
net <- build_htna(human_ai, actor_type = "actor_type")
#> Warning: A network with one long sequence is not recommended and can't be validated using bootstrap and other confirmatory testings.
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions), 'actor_type' (24 sessions)

# Concrete state-level patterns of length 2..4 (default)
extract_meta_paths(net)
#> Patterns (state-level) over 429 sequences
#> Rows: 5445 | Lengths: 2, 3, 4 | Gaps: 0
#>              schema  meta_schema length gap count n_seq support frequency lift
#>    Request->Specify Human->Human      2   0  1042   402   0.937     0.055 2.27
#>           Ask->Plan       AI->AI      2   0   964   346   0.807     0.051 4.87
#>    Execute->Request    AI->Human      2   0   921   269   0.627     0.049 1.80
#>    Request->Execute    Human->AI      2   0   904   299   0.697     0.048 1.77
#>    Specify->Execute    Human->AI      2   0   796   261   0.608     0.042 1.66
#>        Specify->Ask    Human->AI      2   0   784   292   0.681     0.041 2.20
#>      Check->Execute    Human->AI      2   0   534   243   0.566     0.028 2.50
#>       Plan->Request    AI->Human      2   0   503   268   0.625     0.027 1.98
#>        Request->Ask    Human->AI      2   0   459   222   0.517     0.024 1.21
#>  Execute->Frustrate    AI->Human      2   0   451   230   0.536     0.024 1.50
#> ... (5435 more)

# Type-level meta-path summary
extract_meta_paths(net, level = "type")
#> Meta-paths (type-level) over 429 sequences
#> Rows: 28 | Lengths: 2, 3, 4 | Gaps: 0
#>            schema length gap count n_seq support frequency lift
#>         Human->AI      2   0  5970   428   0.998     0.316 1.28
#>         AI->Human      2   0  5693   424   0.988     0.301 1.22
#>      Human->Human      2   0  4674   422   0.984     0.247 0.79
#>            AI->AI      2   0  2581   403   0.939     0.136 0.70
#>  Human->AI->Human      3   0  3593   402   0.937     0.194 1.41
#>  Human->Human->AI      3   0  3172   422   0.984     0.172 1.25
#>  AI->Human->Human      3   0  2828   403   0.939     0.153 1.11
#>     AI->Human->AI      3   0  2744   383   0.893     0.148 1.36
#>     Human->AI->AI      3   0  2189   403   0.939     0.118 1.09
#>     AI->AI->Human      3   0  2100   397   0.925     0.114 1.04
#> ... (18 more)

# Concrete instances of a type-level template
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

# Mix types, concrete codes, and wildcards
extract_meta_paths(net, schema = "Human->Ask->Human")
#> State-level instances of schema 'Human->Ask->Human' over 429 sequences
#> Rows: 36 | Lengths: 3 | Gaps: 0
#>                   schema      meta_schema length gap count n_seq support
#>    Request->Ask->Request Human->AI->Human      3   0   130    91   0.212
#>    Specify->Ask->Request Human->AI->Human      3   0   120    84   0.196
#>  Specify->Ask->Frustrate Human->AI->Human      3   0    86    70   0.163
#>  Request->Ask->Frustrate Human->AI->Human      3   0    50    44   0.103
#>    Specify->Ask->Specify Human->AI->Human      3   0    49    41   0.096
#>     Specify->Ask->Refine Human->AI->Human      3   0    47    34   0.079
#>    Specify->Ask->Inquire Human->AI->Human      3   0    41    34   0.079
#>    Inquire->Ask->Request Human->AI->Human      3   0    41    37   0.086
#>      Specify->Ask->Check Human->AI->Human      3   0    40    34   0.079
#>      Request->Ask->Check Human->AI->Human      3   0    37    35   0.082
#>  frequency lift
#>      0.126 2.19
#>      0.117 2.15
#>      0.084 2.61
#>      0.049 1.43
#>      0.048 0.93
#>      0.046 3.29
#>      0.040 2.67
#>      0.040 2.51
#>      0.039 1.71
#>      0.036 1.49
#> ... (26 more)
extract_meta_paths(net, schema = "Human->*->Human")
#> State-level instances of schema 'Human->*->Human' over 429 sequences
#> Rows: 285 | Lengths: 3 | Gaps: 0
#>                       schema         meta_schema length gap count n_seq support
#>    Request->Execute->Request    Human->AI->Human      3   0   401   194   0.452
#>  Request->Specify->Frustrate Human->Human->Human      3   0   248   241   0.562
#>    Specify->Request->Specify Human->Human->Human      3   0   198   192   0.448
#>    Specify->Execute->Request    Human->AI->Human      3   0   176   107   0.249
#>    Request->Request->Specify Human->Human->Human      3   0   169   168   0.392
#>    Specify->Request->Request Human->Human->Human      3   0   163   163   0.380
#>        Request->Ask->Request    Human->AI->Human      3   0   130    91   0.212
#>      Check->Execute->Request    Human->AI->Human      3   0   123    96   0.224
#>        Specify->Ask->Request    Human->AI->Human      3   0   120    84   0.196
#>  Specify->Execute->Frustrate    Human->AI->Human      3   0   114    88   0.205
#>  frequency lift
#>      0.079 5.00
#>      0.049 5.86
#>      0.039 2.93
#>      0.035 2.33
#>      0.033 2.35
#>      0.032 2.27
#>      0.026 2.19
#>      0.024 3.67
#>      0.024 2.15
#>      0.023 2.57
#> ... (275 more)

# Gapped patterns; lift threshold
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
extract_meta_paths(net, length = 3, min_lift = 1.2)
#> Patterns (state-level) over 429 sequences
#> Rows: 445 | Lengths: 3 | Gaps: 0
#>                       schema         meta_schema length gap count n_seq support
#>    Request->Execute->Request    Human->AI->Human      3   0   401   194   0.452
#>    Execute->Request->Execute       AI->Human->AI      3   0   391   180   0.420
#>        Request->Specify->Ask    Human->Human->AI      3   0   344   202   0.471
#>           Specify->Ask->Plan       Human->AI->AI      3   0   340   197   0.459
#>           Ask->Plan->Request       AI->AI->Human      3   0   303   197   0.459
#>    Request->Specify->Execute    Human->Human->AI      3   0   281   152   0.354
#>    Execute->Request->Specify    AI->Human->Human      3   0   249   142   0.331
#>  Request->Specify->Frustrate Human->Human->Human      3   0   248   241   0.562
#>         Frustrate->Ask->Plan       Human->AI->AI      3   0   214   187   0.436
#>    Specify->Request->Specify Human->Human->Human      3   0   198   192   0.448
#>  frequency  lift
#>      0.022  5.00
#>      0.021  4.65
#>      0.019  6.15
#>      0.018 11.65
#>      0.016  9.77
#>      0.015  3.73
#>      0.013  3.30
#>      0.013  5.86
#>      0.012 11.71
#>      0.011  2.93
#> ... (435 more)
# }
```
