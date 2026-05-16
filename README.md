
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `htna`: Heterogeneous Transition Network Analysis <img src="man/figures/logo.png" align="right" height="139"/>

<!-- badges: start -->

[![R-CMD-check](https://github.com/sonsoleslp/htna/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sonsoleslp/htna/actions/workflows/R-CMD-check.yaml)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

`htna` is an R package for Heterogeneous Transition Network Analysis
(HTNA) that models processes or interactions between a mix two or more
actor groups (e.g.  Human and AI) as a single network. HTNA builds on
the traditions of Transition Network Analysis (TNA) and Co-occurrence
Network Analysis (CNA) and maintains the rigor of either method.

The package provides a focused API on top of the
[Nestimate](https://CRAN.R-project.org/package=Nestimate) estimation
engine and the [cograph](https://CRAN.R-project.org/package=cograph)
rendering engine: build a network over the combined sequence while
preserving the actor partition, so downstream bootstrap, permutation,
reliability, centrality, and plotting functions treat each actor’s codes
as a distinct node group.

## Installation

You can install the development version of `htna` from
[GitHub](https://github.com/sonsoleslp/htna):

``` r
# install.packages("devtools")
devtools::install_github("sonsoleslp/htna")
```

## Example

``` r
library("htna")
```

Load the example data shipped with `Nestimate`:

``` r
data(human_long, ai_long, package = "Nestimate")
```

### Build a heterogeneous transition network

`build_htna()` takes a named list of long-format data frames, one per
actor group, combines the sequences, estimates transition probabilities,
and stores the actor partition on the resulting network:

``` r
net <- build_htna(list(Human = human_long, AI = ai_long))
#> Warning: S3 methods 'print.wtna_perm_mixed', 'summary.wtna_perm_mixed' were
#> declared in NAMESPACE but not found
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions)
```

### Plot the network

`plot_htna()` auto-detects the actor groups and renders them with
distinct colours:

``` r
plot_htna(net, threshold = 0.05, layout = "circular")
#> Registered S3 methods overwritten by 'cograph':
#>   method             from     
#>   plot.net_stability Nestimate
#>   print.mcml         Nestimate
```

<img src="man/figures/README-htna-plot-1.png" alt="" width="60%" style="display: block; margin: auto;" />

### Extract path patterns

By default `extract_meta_paths()` enumerates concrete state-level
patterns and tags each row with the type-level template it instantiates:

``` r
extract_meta_paths(net)
#> Patterns (state-level) over 429 sequences
#> Rows: 7952 | Lengths: 2, 3, 4 | Gaps: 0
#>                schema  meta_schema length gap count n_seq support frequency
#>     Investigate->Plan       AI->AI      2   0   962   346   0.807     0.051
#>      Specify->Execute    Human->AI      2   0   796   261   0.608     0.042
#>  Specify->Investigate    Human->AI      2   0   764   289   0.674     0.040
#>      Command->Execute    Human->AI      2   0   722   267   0.622     0.038
#>      Execute->Command    AI->Human      2   0   659   225   0.524     0.035
#>      Request->Specify Human->Human      2   0   573   295   0.688     0.030
#>      Command->Specify Human->Human      2   0   469   246   0.573     0.025
#>         Plan->Command    AI->Human      2   0   375   220   0.513     0.020
#>      Specify->Command Human->Human      2   0   361   354   0.825     0.019
#>         Execute->Plan       AI->AI      2   0   337   181   0.422     0.018
#>  lift
#>  5.07
#>  1.66
#>  2.23
#>  2.14
#>  1.95
#>  3.69
#>  1.55
#>  2.23
#>  1.19
#>  1.26
#> ... (7942 more)
```

Filter to concrete instances of a type-level template. Schema parts can
mix type names, concrete codes, and `*`:

``` r
extract_meta_paths(net, schema = "Human->AI->Human")
#> State-level instances of schema 'Human->AI->Human' over 429 sequences
#> Rows: 345 | Lengths: 3 | Gaps: 0
#>                         schema      meta_schema length gap count n_seq support
#>      Command->Execute->Command Human->AI->Human      3   0   288   150   0.350
#>      Specify->Execute->Command Human->AI->Human      3   0   111    83   0.193
#>  Command->Investigate->Command Human->AI->Human      3   0    84    60   0.140
#>      Command->Execute->Inquire Human->AI->Human      3   0    79    64   0.149
#>  Specify->Investigate->Command Human->AI->Human      3   0    78    62   0.145
#>      Command->Execute->Request Human->AI->Human      3   0    70    63   0.147
#>      Specify->Execute->Request Human->AI->Human      3   0    65    47   0.110
#>       Verify->Execute->Command Human->AI->Human      3   0    62    52   0.121
#>    Specify->Execute->Frustrate Human->AI->Human      3   0    58    41   0.096
#>    Command->Execute->Frustrate Human->AI->Human      3   0    57    50   0.117
#>  frequency lift
#>      0.080 8.22
#>      0.031 2.23
#>      0.023 3.37
#>      0.022 5.43
#>      0.022 2.20
#>      0.019 3.90
#>      0.018 2.54
#>      0.017 7.26
#>      0.016 2.54
#>      0.016 3.56
#> ... (335 more)
extract_meta_paths(net, schema = "Human->Ask->*")
#> State-level instances of schema 'Human->Ask->*' over 429 sequences
#> Rows: 39 | Lengths: 3 | Gaps: 0
#>                   schema      meta_schema length gap count n_seq support
#>    Inquire->Ask->Explain    Human->AI->AI      3   0    12    11   0.026
#>  Specify->Ask->Interrupt Human->AI->Human      3   0     5     5   0.012
#>    Inquire->Ask->Command Human->AI->Human      3   0     4     4   0.009
#>    Command->Ask->Correct Human->AI->Human      3   0     4     4   0.009
#>    Request->Ask->Explain    Human->AI->AI      3   0     4     4   0.009
#>    Specify->Ask->Explain    Human->AI->AI      3   0     4     4   0.009
#>  Inquire->Ask->Interrupt Human->AI->Human      3   0     4     4   0.009
#>  Frustrate->Ask->Correct Human->AI->Human      3   0     3     3   0.007
#>    Command->Ask->Explain    Human->AI->AI      3   0     3     3   0.007
#>    Command->Ask->Command Human->AI->Human      3   0     2     2   0.005
#>  frequency   lift
#>      0.146 106.21
#>      0.061   7.61
#>      0.049   9.04
#>      0.049   9.66
#>      0.049  28.71
#>      0.049  10.34
#>      0.049  20.84
#>      0.037  15.84
#>      0.037  11.04
#>      0.024   1.88
#> ... (29 more)
```

Pass `level = "type"` for the type-level meta-path summary:

``` r
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
```

## Related packages

- [`tna`](https://CRAN.R-project.org/package=tna) – Transition Network
  Analysis for homogeneous sequences.
- [`Nestimate`](https://CRAN.R-project.org/package=Nestimate) – Network
  estimation, bootstrap, permutation, reliability, and centrality.
- [`cograph`](https://CRAN.R-project.org/package=cograph) – Network
  visualisation and rendering.
- [`codyna`](https://CRAN.R-project.org/package=codyna) – Sequence
  patterns, outcomes, and indices.
