# Subsequence Pattern Comparison Across Groups

Extends
[`Nestimate::sequence_compare()`](https://saqr.me/Nestimate/reference/sequence_compare.html)
with a `level` argument that lets the comparison run on type-level
(meta-path) sequences as well as state-level sequences.

## Usage

``` r
sequence_compare_htna(
  x,
  group = NULL,
  level = c("state", "type"),
  sub = 3:5,
  min_freq = 5L,
  test = c("permutation", "chisq", "none"),
  iter = 1000L,
  adjust = "fdr"
)
```

## Arguments

- x:

  A `netobject_group` (from grouped `build_network`), a `netobject`
  (requires `group`), or a wide-format `data.frame` (requires `group`).

- group:

  Character or vector. Column name or vector of group labels. Not needed
  for `netobject_group`.

- level:

  Character. `"state"` (default) compares concrete state-level k-grams;
  `"type"` first replaces each state with its actor type so the
  comparison runs on meta-paths (e.g. `Human->AI->Human`). Only
  meaningful when `x` is an htna network with an actor partition.

- sub:

  Integer vector. Pattern lengths to analyze. Default: `3:5`.

- min_freq:

  Integer. Minimum frequency in each group for a pattern to be included.
  Default: 5.

- test:

  Character. Inference method: one of `"permutation"` (default),
  `"chisq"`, or `"none"`. See Details.

- iter:

  Integer. Permutation iterations. Only used when
  `test = "permutation"`. Default: 1000.

- adjust:

  Character. P-value correction method (see
  [`p.adjust`](https://rdrr.io/r/stats/p.adjust.html)). Default:
  `"fdr"`.

## Value

An object of class `net_sequence_compare`. See
[`Nestimate::sequence_compare()`](https://saqr.me/Nestimate/reference/sequence_compare.html)
for full details and the corresponding
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) method.

## Details

Extracts all k-gram patterns (subsequences of length `k`) from the
sequences in each cohort, computes standardised residuals against the
independence model, and optionally runs a permutation or chi-square test
for differences in pattern rates between cohorts.

Operates on grouped htna networks (built via
`build_htna(..., group = ...)`), single htna networks paired with a
`group` argument, or wide-format sequence data with an explicit `group`
argument. The actor partition itself is not consumed when
`level = "state"` — sequence comparison is between cohorts of sessions,
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
# \donttest{
data(human_long, ai_long, package = "Nestimate")
net <- build_htna(list(Human = human_long, AI = ai_long))
#> Warning: A network with one long sequence is not recommended and can't be validated using bootstrap and other confirmatory testings.
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions)
n   <- nrow(net$data)
grp <- rep(c("early", "late"), length.out = n)

# State-level comparison (default)
sequence_compare_htna(net, group = grp, iter = 50)
#> Sequence Comparison  [647 patterns | 2 groups: early, late]
#>   Lengths: 3, 4, 5  |  min_freq: 5  |  permutation: 50 iter (fdr)
#> 
#>                                    pattern length freq_early freq_late
#>                Specify->Execute->Interrupt      3         16        40
#>     Command->Specify->Investigate->Command      4         17         6
#>     Specify->Investigate->Specify->Specify      4         17         5
#>         Specify->Execute->Specify->Specify      4          7        17
#>         Command->Execute->Inquire->Execute      4          7        19
#>  Plan->Specify->Specify->Investigate->Plan      5         20         8
#>      Execute->Refine->Specify->Investigate      4         23        10
#>                Frustrate->Specify->Execute      3          9        20
#>              Frustrate->Execute->Frustrate      3         17         6
#>                  Execute->Inquire->Execute      3         38        66
#>    prop_early    prop_late resid_early resid_late effect_size p_value
#>  0.0016576875 0.0045259108   -3.545384   3.545384    5.078925       1
#>  0.0018014199 0.0006955715    2.081757  -2.081757    2.932710       1
#>  0.0018014199 0.0005796429    2.351619  -2.351619    2.645629       1
#>  0.0007417612 0.0019707860   -2.264986   2.264986    2.274416       1
#>  0.0007417612 0.0022026432   -2.586801   2.586801    2.266562       1
#>  0.0021687270 0.0009505703    2.029712  -2.029712    2.262807       1
#>  0.0024372152 0.0011592859    2.008948  -2.008948    2.130374       1
#>  0.0009324492 0.0022629554   -2.283728   2.283728    2.029105       1
#>  0.0017612930 0.0006788866    2.085847  -2.085847    2.008580       1
#>  0.0039370079 0.0074677529   -3.206714   3.206714    2.003553       1
#>   ... and 637 more patterns

# Meta-path comparison
sequence_compare_htna(net, group = grp, level = "type", iter = 50)
#> Sequence Comparison  [55 patterns | 2 groups: early, late]
#>   Lengths: 3, 4, 5  |  min_freq: 5  |  permutation: 50 iter (fdr)
#> 
#>                      pattern length freq_early freq_late prop_early  prop_late
#>     Human->AI->Human->AI->AI      5        203       187 0.02201258 0.02221958
#>     Human->Human->AI->AI->AI      5        121       108 0.01312080 0.01283270
#>          Human->Human->Human      3        765       707 0.07925818 0.07999547
#>  Human->Human->AI->Human->AI      5        348       320 0.03773585 0.03802281
#>     AI->Human->Human->AI->AI      5        489       447 0.05302537 0.05311312
#>      AI->Human->Human->Human      4        280       252 0.02967045 0.02921400
#>            AI->AI->Human->AI      4        496       457 0.05255908 0.05297936
#>      Human->Human->Human->AI      4        454       422 0.04810851 0.04892186
#>  AI->Human->AI->Human->Human      5        438       402 0.04749512 0.04776616
#>   Human->Human->Human->Human      4        308       278 0.03263749 0.03222815
#>  resid_early  resid_late effect_size   p_value
#>  -0.09338247  0.09338247   -1.433885 0.9766926
#>   0.16882097 -0.16882097   -1.342532 0.9766926
#>  -0.18500516  0.18500516   -1.245541 0.9766926
#>  -0.09972162  0.09972162   -1.218174 0.9766926
#>  -0.02596475  0.02596475   -1.209401 0.9766926
#>   0.18123540 -0.18123540   -1.140339 0.9766926
#>  -0.12620976  0.12620976   -1.094861 0.9766926
#>  -0.25418197  0.25418197   -1.085603 0.9766926
#>  -0.08442168  0.08442168   -1.061727 0.9766926
#>   0.15510366 -0.15510366   -1.053529 0.9766926
#>   ... and 45 more patterns
# }
```
