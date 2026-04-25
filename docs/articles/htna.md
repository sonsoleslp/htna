# Getting Started with htna

Heterogeneous Transition Network Analysis (HTNA) extends traditional TNA
to sequences that mix two or more actor groups (e.g. Human and AI)
within the same interaction. The `htna` package provides a thin, focused
API on top of the
[Nestimate](https://CRAN.R-project.org/package=Nestimate) estimation
engine and the [cograph](https://CRAN.R-project.org/package=cograph)
rendering engine.

## Building a heterogeneous network

Start by loading the package and the example data shipped with
Nestimate:

``` r

library(htna)
data(human_long, ai_long, package = "Nestimate")
```

[`build_htna()`](https://mohsaqr.github.io/htna/reference/build_htna.md)
accepts a named list of long-format data frames, one per actor group. It
combines the sequences, estimates transition probabilities, and stores
the actor partition on the resulting network object:

``` r

net <- build_htna(list(Human = human_long, AI = ai_long))
net
#> Transition Network (relative probabilities) [directed]
#>   Weights: [0.001, 0.646]  |  mean: 0.070
#> 
#>   Weight matrix:
#>                 Ask Command Correct Delegate Execute Explain Frustrate Inquire
#>   Ask         0.000   0.108   0.129    0.000   0.011   0.387     0.022   0.065
#>   Command     0.006   0.003   0.054    0.014   0.353   0.030     0.016   0.000
#>   Correct     0.011   0.000   0.038    0.011   0.264   0.050     0.088   0.005
#>   Delegate    0.000   0.106   0.018    0.000   0.011   0.018     0.053   0.032
#>   Execute     0.000   0.209   0.053    0.000   0.074   0.003     0.088   0.088
#>   Explain     0.000   0.166   0.080    0.000   0.000   0.000     0.066   0.099
#>   Frustrate   0.005   0.009   0.064    0.012   0.208   0.035     0.078   0.033
#>   Inquire     0.029   0.011   0.054    0.027   0.285   0.163     0.025   0.033
#>   Interrupt   0.000   0.151   0.067    0.074   0.007   0.000     0.066   0.094
#>   Investigate 0.000   0.126   0.043    0.000   0.023   0.003     0.069   0.063
#>   Plan        0.000   0.242   0.070    0.000   0.015   0.000     0.112   0.083
#>   Refine      0.001   0.003   0.038    0.006   0.206   0.023     0.000   0.014
#>   Repair      0.043   0.040   0.012    0.028   0.391   0.103     0.020   0.047
#>   Report      0.000   0.205   0.123    0.000   0.035   0.000     0.099   0.111
#>   Request     0.010   0.005   0.001    0.028   0.173   0.026     0.000   0.029
#>   Specify     0.007   0.124   0.005    0.040   0.273   0.033     0.004   0.011
#>   Verify      0.010   0.004   0.002    0.022   0.646   0.052     0.000   0.004
#>               Interrupt Investigate  Plan Refine Repair Report Request Specify
#>   Ask             0.161       0.000 0.022  0.022  0.022  0.011   0.011   0.022
#>   Command         0.007       0.138 0.008  0.014  0.004  0.008   0.096   0.229
#>   Correct         0.005       0.039 0.006  0.068  0.039  0.006   0.076   0.251
#>   Delegate        0.057       0.000 0.611  0.014  0.000  0.000   0.025   0.035
#>   Execute         0.055       0.061 0.107  0.069  0.002  0.007   0.083   0.065
#>   Explain         0.037       0.136 0.058  0.047  0.039  0.103   0.076   0.068
#>   Frustrate       0.006       0.162 0.002  0.122  0.052  0.003   0.069   0.075
#>   Inquire         0.014       0.222 0.009  0.016  0.042  0.007   0.018   0.029
#>   Interrupt       0.086       0.227 0.005  0.067  0.000  0.000   0.055   0.098
#>   Investigate     0.044       0.019 0.425  0.053  0.003  0.010   0.049   0.053
#>   Plan            0.103       0.000 0.003  0.086  0.005  0.026   0.083   0.095
#>   Refine          0.008       0.141 0.013  0.000  0.025  0.000   0.102   0.413
#>   Repair          0.024       0.198 0.016  0.024  0.004  0.000   0.032   0.020
#>   Report          0.088       0.000 0.023  0.094  0.041  0.000   0.099   0.058
#>   Request         0.003       0.146 0.007  0.001  0.012  0.000   0.000   0.545
#>   Specify         0.092       0.262 0.015  0.002  0.013  0.004   0.002   0.101
#>   Verify          0.002       0.214 0.012  0.000  0.024  0.000   0.002   0.006
#>               Verify
#>   Ask          0.011
#>   Command      0.021
#>   Correct      0.043
#>   Delegate     0.021
#>   Execute      0.034
#>   Explain      0.023
#>   Frustrate    0.065
#>   Inquire      0.015
#>   Interrupt    0.005
#>   Investigate  0.017
#>   Plan         0.076
#>   Refine       0.008
#>   Repair       0.000
#>   Report       0.023
#>   Request      0.016
#>   Specify      0.012
#>   Verify       0.000 
#> 
#>   Initial probabilities:
#>   Specify       0.818  ████████████████████████████████████████
#>   Command       0.128  ██████
#>   Request       0.028  █
#>   Interrupt     0.021  █
#>   Frustrate     0.002  
#>   Refine        0.002  
#>   Ask           0.000  
#>   Correct       0.000  
#>   Delegate      0.000  
#>   Execute       0.000  
#>   Explain       0.000  
#>   Inquire       0.000  
#>   Investigate   0.000  
#>   Plan          0.000  
#>   Repair        0.000  
#>   Report        0.000  
#>   Verify        0.000
```

The actor partition is available in two forms:

- `net$nodes$groups` – actor label per node (auto-detected by
  [`plot_htna()`](https://sonsoles.me/cograph/reference/plot_htna.html)).
- `net$node_groups` – canonical `(node, group)` data frame used by
  [`cograph::get_groups()`](https://sonsoles.me/cograph/reference/get_groups.html)
  and
  [`cograph::cluster_summary()`](https://sonsoles.me/cograph/reference/cluster_summary.html).

## Plotting

[`plot_htna()`](https://sonsoles.me/cograph/reference/plot_htna.html)
(re-exported from cograph) auto-detects the actor groups and renders
them with distinct colours:

``` r

plot_htna(net, threshold = 0.05, layout = "circular")
```

![](htna_files/figure-html/unnamed-chunk-3-1.png)

## Meta-paths

Meta-paths are type-level patterns that abstract away from concrete
states and ask how often a typed template appears in the data. For
example, `Human->AI->Human` captures every length-3 path that starts
with a Human code, transitions to an AI code, and returns to a Human
code.

``` r

# Enumerate all meta-paths of length 2 to 4
mp <- extract_meta_paths(net)
mp
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

You can search for a specific schema (with wildcards):

``` r

# Any length-3 path that returns to Human
extract_meta_paths(net, schema = "Human->*->Human")
#> Meta-paths (type-level) over 429 sequences
#> Rows: 2 | Lengths: 3 | Gaps: 0
#>               schema length gap count n_seq support frequency lift
#>     Human->AI->Human      3   0  3593   402   0.937     0.709 1.41
#>  Human->Human->Human      3   0  1472   396   0.923     0.291 0.46
```

Gapped meta-paths allow skipping positions:

``` r

extract_meta_paths(net, length = 3, type = "gapped", gap = 1:2)
#> Meta-paths (type-level) over 429 sequences
#> Rows: 16 | Lengths: 3 | Gaps: 1, 2
#>               schema length gap count n_seq support frequency lift
#>     Human->AI->Human      3   1  3265   408   0.951     0.185 1.35
#>     Human->Human->AI      3   1  2722   413   0.963     0.154 1.12
#>        AI->Human->AI      3   1  2515   379   0.883     0.143 1.31
#>     AI->Human->Human      3   1  2242   391   0.911     0.127 0.92
#>  Human->Human->Human      3   1  2146   372   0.867     0.122 0.70
#>        Human->AI->AI      3   1  1748   384   0.895     0.099 0.91
#>        AI->AI->Human      3   1  1618   360   0.839     0.092 0.84
#>           AI->AI->AI      3   1  1382   304   0.709     0.078 0.91
#>  Human->Human->Human      3   2  2948   399   0.930     0.176 1.01
#>     Human->Human->AI      3   2  2341   390   0.909     0.139 1.01
#> ... (6 more)
```

Filter by lift to find over-represented meta-paths:

``` r

extract_meta_paths(net, length = 3, min_lift = 1.2)
#> Meta-paths (type-level) over 429 sequences
#> Rows: 3 | Lengths: 3 | Gaps: 0
#>            schema length gap count n_seq support frequency lift
#>  Human->AI->Human      3   0  3593   402   0.937     0.194 1.41
#>  Human->Human->AI      3   0  3172   422   0.984     0.172 1.25
#>     AI->Human->AI      3   0  2744   383   0.893     0.148 1.36
```
