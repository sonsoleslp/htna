# Subsequence Pattern Comparison Across Groups

Extends
[`Nestimate::sequence_compare()`](https://pak.dynasite.org/Nestimate/reference/sequence_compare.html)
with a `level` argument that lets the comparison run on type-level
(meta-path) sequences as well as state-level sequences.

## Usage

``` r
sequence_compare_htna(x, group = NULL, level = c("state", "type"), ...)
```

## Arguments

- x:

  A grouped htna network, a single htna network (paired with `group`),
  or wide-format sequence data. See
  [`Nestimate::sequence_compare()`](https://pak.dynasite.org/Nestimate/reference/sequence_compare.html).

- group:

  Grouping specification for the cohorts to compare, when not already
  carried by a grouped htna network. See
  [`Nestimate::sequence_compare()`](https://pak.dynasite.org/Nestimate/reference/sequence_compare.html).

- level:

  Character. `"state"` (default) compares concrete state-level k-grams;
  `"type"` first replaces each state with its actor type so the
  comparison runs on meta-paths (e.g. `Human->AI->Human`). Only
  meaningful when `x` is an htna network with an actor partition.

- ...:

  Additional arguments passed to
  [`Nestimate::sequence_compare()`](https://pak.dynasite.org/Nestimate/reference/sequence_compare.html),
  such as `sub`, `min_freq`, `test`, `iter`, and `adjust`. See that
  function for the full, current argument list.

## Value

An object of class `net_sequence_comparison` with components `patterns`,
`groups`, `n_patterns` and `params`.
[`summary()`](https://rdrr.io/r/base/summary.html) returns the tidy
pattern table: one row per k-gram, with columns `pattern`, `length`,
per-cohort frequencies (`freq_<cohort>`), proportions (`prop_<cohort>`)
and standardised residuals (`resid_<cohort>`), `effect_size` and, when a
test is run, `p_value`. [`print()`](https://rdrr.io/r/base/print.html)
and [`plot()`](https://rdrr.io/r/graphics/plot.default.html)
(`style = "pyramid"` or `"heatmap"`) methods are supplied by Nestimate;
see
[`Nestimate::sequence_compare()`](https://pak.dynasite.org/Nestimate/reference/sequence_compare.html).

## Details

Extracts all k-gram patterns (subsequences of length `k`) from the
sequences in each cohort, computes standardised residuals against the
independence model, and optionally runs a permutation or chi-square test
for differences in pattern rates between cohorts.

Operates on grouped htna networks (built via
`build_htna(..., group = ...)`), single htna networks paired with a
`group` argument, or wide-format sequence data with an explicit `group`
argument. The actor partition itself is not consumed when
`level = "state"`: sequence comparison is between cohorts of sessions,
not between actors. When `level = "type"`, concrete codes are folded
into their actor type before pattern enumeration, so the comparison runs
on meta-paths.

## See also

[`permutation_htna()`](https://sonsoles.me/htna/reference/permutation_htna.md)
for whole-network differences,
[`mosaic_plot_htna()`](https://sonsoles.me/htna/reference/mosaic_plot_htna.md)
for single-step transition residuals,
[`extract_meta_paths()`](https://sonsoles.me/htna/reference/extract_meta_paths.md)
for descriptive meta-path enumeration.

## Examples

``` r
data(human_ai)
grp <- build_htna(human_ai, actor_type = "actor_type", group = "phase")
#> A network with one long sequence is not recommended and can't be validated using bootstrap and other confirmatory testings.
#> Metadata aggregated per session: ties resolved by first occurrence in 'cluster' (24 sessions), 'actor_type' (9 sessions)
#> A network with one long sequence is not recommended and can't be validated using bootstrap and other confirmatory testings.
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (18 sessions), 'actor_type' (15 sessions)

# State-level comparison (default); few permutations to keep it fast
sequence_compare_htna(grp, iter = 50)
#> Sequence Comparison  [699 patterns | 2 groups: Early, Late]
#>   Lengths: 3, 4, 5  |  min_freq: 5  |  permutation: 50 iter (fdr)
#> 
#>                                 pattern length freq_Early freq_Late
#>          Specify->Request->Specify->Ask      4         10        58
#>             Frustrate->Ask->Plan->Check      4         39         7
#>    Specify->Request->Specify->Ask->Plan      5          8        35
#>    Specify->Frustrate->Ask->Plan->Check      5         37         6
#>               Specify->Request->Specify      3         82       116
#>  Request->Specify->Frustrate->Ask->Plan      5        105        51
#>                    Ask->Plan->Frustrate      3         74       112
#>             Request->Specify->Frustrate      3        160        88
#>                   Request->Specify->Ask      3        147       197
#>                   Execute->Ask->Request      3          6        22
#>    prop_Early    prop_Late resid_Early resid_Late effect_size   p_value
#>  0.0010494281 0.0067963440   -6.296503   6.296503    8.420388 0.3114973
#>  0.0040927694 0.0008202484    4.356696  -4.356696    6.541789 0.3114973
#>  0.0008586455 0.0042062252   -4.500415   4.500415    6.420094 0.3114973
#>  0.0039712354 0.0007210672    4.369457  -4.369457    6.374350 0.3114973
#>  0.0084171628 0.0132601738   -3.194473   3.194473    5.130398 0.3114973
#>  0.0112697220 0.0061290710    3.640078  -3.640078    5.102890 0.3114973
#>  0.0075959762 0.0128029264   -3.542427   3.542427    5.033177 0.3114973
#>  0.0164237323 0.0100594422    3.756080  -3.756080    4.866214 0.3114973
#>  0.0150893040 0.0225194330   -3.733134   3.733134    4.556679 0.3114973
#>  0.0006158900 0.0025148605   -3.315483   3.315483    4.490829 0.3114973
#>   ... and 689 more patterns

# Meta-path comparison
sequence_compare_htna(grp, level = "type", iter = 50)
#> Sequence Comparison  [55 patterns | 2 groups: Early, Late]
#>   Lengths: 3, 4, 5  |  min_freq: 5  |  permutation: 50 iter (fdr)
#> 
#>                         pattern length freq_Early freq_Late  prop_Early
#>  Human->Human->Human->AI->Human      5        112       196 0.012021037
#>  AI->Human->Human->Human->Human      5         41        71 0.004400558
#>            Human->AI->AI->Human      4        976       796 0.102424179
#>            Human->Human->AI->AI      4        846       646 0.088781614
#>                   Human->AI->AI      3       1203       986 0.123485937
#>                Human->AI->Human      3       1813      1780 0.186101417
#>     Human->AI->Human->AI->Human      5        647       695 0.069442954
#>           AI->Human->AI->AI->AI      5         44        63 0.004722550
#>     Human->Human->AI->AI->Human      5        676       525 0.072555544
#>        AI->AI->Human->AI->Human      5        363       279 0.038961039
#>    prop_Late resid_Early resid_Late effect_size   p_value
#>  0.023554861   -5.837799   5.837799    7.627748 0.1540616
#>  0.008532628   -3.448798   3.448798    3.804741 0.1540616
#>  0.093273963    2.064052  -2.064052    3.517733 0.1540616
#>  0.075697211    3.189261  -3.189261    3.499731 0.1540616
#>  0.112711477    2.264188  -2.264188    3.290279 0.1540616
#>  0.203475080   -2.980988   2.980988    3.252581 0.1540616
#>  0.083523615   -3.520925   3.520925    2.749593 0.1540616
#>  0.007571205   -2.432177   2.432177    2.809446 0.1659125
#>  0.063093378    2.490338  -2.490338    2.556875 0.1659125
#>  0.033529624    1.922749  -1.922749    2.477371 0.1659125
#>   ... and 45 more patterns
```
