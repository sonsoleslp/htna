# Compare short sequence patterns between groups of sessions

`sequence_compare_htna()` compares short sequence patterns between the
cohorts of a grouped htna network, or between the groups of a single
network given by `group`. It counts every k-gram (a run of k consecutive
codes) in each group and finds the patterns that are more or less
frequent in a group than expected if pattern and group were independent.
Each pattern gets a standardized residual per group (Haberman, 1973)
and, optionally, a permutation test (the session is the unit) or a
chi-square test of whether its frequency differs between the groups,
with p-values adjusted across all patterns. Two groups of sessions can
differ not only in single transitions but in longer runs of codes, such
as a human request followed by an AI plan and an AI execution.

With `level = "type"` every code is first replaced by its actor type, so
the patterns are meta-paths such as `Human->AI->Human` and the
comparison shows how the turn-taking between actors differs between the
groups. `sequence_compare_htna()` runs Nestimate's `sequence_compare()`
on the sequences and returns a `net_sequence_comparison` object.

For single transitions use
[`permutation_htna()`](https://sonsoles.me/htna/reference/permutation_htna.md)
or
[`mosaic_plot_htna()`](https://sonsoles.me/htna/reference/mosaic_plot_htna.md);
for descriptive counts of patterns in one network use
[`extract_meta_paths()`](https://sonsoles.me/htna/reference/extract_meta_paths.md).

## Usage

``` r
sequence_compare_htna(x, group = NULL, level = c("state", "type"), ...)
```

## Arguments

- x:

  A grouped htna network (an `htna_group`), or an htna network together
  with `group`.

- group:

  The grouping of the sessions when `x` is a single network: the name of
  a session-level column of the data the network was built from (for
  example `"phase"`), or a vector with one label per session (default
  `NULL`).

- level:

  `"state"` (default) compares patterns of codes; `"type"` compares
  patterns of actor types.

- ...:

  Settings: `sub` (pattern lengths, default `3:5`), `min_freq` (minimum
  count in every group, 5), `test` (`"permutation"`, `"chisq"` or
  `"none"`), `iter` (permutations, 1000), `adjust` (multiplicity
  correction, `"fdr"`) and `actor` (reassign whole actors in the
  permutation test; needs Nestimate 1.0.13 or later).

## Value

An object of class `net_sequence_comparison` with
[`print()`](https://rdrr.io/r/base/print.html),
[`summary()`](https://rdrr.io/r/base/summary.html) and
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) methods.
[`summary()`](https://rdrr.io/r/base/summary.html) returns a data frame
with one row per pattern and columns `pattern`, `length`, one
`freq_<group>`, `prop_<group>` and `resid_<group>` column per group,
`effect_size` and, when a test is run, `p_value`.

## Conditions

An error of class `htna_bad_input` when `level = "type"` and `x` has no
actor partition; `htna_missing_column` when `group` names a column the
network does not hold.

## References

Haberman, S. J. (1973). The analysis of residuals in cross-classified
tables. *Biometrics*, 29(1), 205-220.
[doi:10.2307/2529686](https://doi.org/10.2307/2529686)

## See also

[`extract_meta_paths()`](https://sonsoles.me/htna/reference/extract_meta_paths.md),
[`permutation_htna()`](https://sonsoles.me/htna/reference/permutation_htna.md),
[`mosaic_plot_htna()`](https://sonsoles.me/htna/reference/mosaic_plot_htna.md).

## Examples

``` r
data(human_ai)
grp <- build_htna(human_ai, actor_type = "actor_type", group = "phase")
#> Metadata aggregated per session: ties resolved by first occurrence in 'cluster' (24 sessions), 'actor_type' (9 sessions)
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (18 sessions), 'actor_type' (15 sessions)
# iter = 50 keeps the example fast; use the default 1000 in practice.
sequence_compare_htna(grp, iter = 50)
#> Sequence Comparison  [699 patterns | 2 groups: Early, Late]
#>   Lengths: 3, 4, 5  |  min_freq: 5  |  permutation: 50 iter (fdr)
#> 
#>                               pattern length freq_Early freq_Late   prop_Early
#>        Specify->Request->Specify->Ask      4         10        58 0.0010494281
#>  Specify->Request->Specify->Ask->Plan      5          8        35 0.0008586455
#>  Specify->Frustrate->Ask->Plan->Check      5         37         6 0.0039712354
#>           Frustrate->Ask->Plan->Check      4         39         7 0.0040927694
#>           Request->Specify->Frustrate      3        160        88 0.0164237323
#>             Specify->Request->Specify      3         82       116 0.0084171628
#>                  Plan->Frustrate->Ask      3          5        23 0.0005132416
#>        Specify->Ask->Request->Execute      4         11        27 0.0011543709
#>         Specify->Frustrate->Ask->Plan      4        106        51 0.0111239375
#>                   Ask->Frustrate->Ask      3         11        30 0.0011291316
#>     prop_Late resid_Early resid_Late effect_size   p_value
#>  0.0067963440   -6.296503   6.296503   10.605886 0.3045752
#>  0.0042062252   -4.500415   4.500415    7.809943 0.3045752
#>  0.0007210672    4.369457  -4.369457    7.700874 0.3045752
#>  0.0008202484    4.356696  -4.356696    7.135977 0.3045752
#>  0.0100594422    3.756080  -3.756080    5.395069 0.3045752
#>  0.0132601738   -3.194473   3.194473    5.112336 0.3045752
#>  0.0026291724   -3.694281   3.694281    4.738801 0.3045752
#>  0.0031638153   -2.942670   2.942670    4.723561 0.3045752
#>  0.0059760956    3.721097  -3.721097    4.581468 0.3045752
#>  0.0034293553   -3.320004   3.320004    4.486509 0.3045752
#>   ... and 689 more patterns
sequence_compare_htna(grp, level = "type", iter = 50)
#> Sequence Comparison  [55 patterns | 2 groups: Early, Late]
#>   Lengths: 3, 4, 5  |  min_freq: 5  |  permutation: 50 iter (fdr)
#> 
#>                         pattern length freq_Early freq_Late  prop_Early
#>  Human->Human->Human->AI->Human      5        112       196 0.012021037
#>            Human->Human->AI->AI      4        846       646 0.088781614
#>  AI->Human->Human->Human->Human      5         41        71 0.004400558
#>     Human->Human->AI->AI->Human      5        676       525 0.072555544
#>                   Human->AI->AI      3       1203       986 0.123485937
#>     Human->AI->Human->AI->Human      5        647       695 0.069442954
#>                Human->AI->Human      3       1813      1780 0.186101417
#>     Human->Human->Human->AI->AI      5        315       228 0.033809166
#>           AI->Human->AI->AI->AI      5         44        63 0.004722550
#>            Human->AI->AI->Human      4        976       796 0.102424179
#>    prop_Late resid_Early resid_Late effect_size   p_value
#>  0.023554861   -5.837799   5.837799    6.613494 0.1540616
#>  0.075697211    3.189261  -3.189261    3.715947 0.1540616
#>  0.008532628   -3.448798   3.448798    3.550704 0.1540616
#>  0.063093378    2.490338  -2.490338    3.198563 0.1540616
#>  0.112711477    2.264188  -2.264188    3.079960 0.1540616
#>  0.083523615   -3.520925   3.520925    2.537843 0.1540616
#>  0.203475080   -2.980988   2.980988    2.259193 0.1540616
#>  0.027400553    2.459688  -2.459688    2.504977 0.1960784
#>  0.007571205   -2.432177   2.432177    2.379338 0.1960784
#>  0.093273963    2.064052  -2.064052    2.319371 0.1960784
#>   ... and 45 more patterns
```
