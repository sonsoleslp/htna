# Print and summarize the bootstraps of a grouped network

[`print()`](https://rdrr.io/r/base/print.html) and
[`summary()`](https://rdrr.io/r/base/summary.html) read the bootstraps
of a grouped network: [`print()`](https://rdrr.io/r/base/print.html)
shows each cohort's report in turn under its name, and
[`summary()`](https://rdrr.io/r/base/summary.html) stacks the cohorts'
edge tables into one data frame, so the edges of the cohorts can be read
and compared together. A grouped network bootstrapped with
[`bootstrap_htna()`](https://sonsoles.me/htna/reference/bootstrap_htna.md)
gives one bootstrap per cohort.

## Usage

``` r
# S3 method for class 'htna_bootstrap_group'
print(x, ...)

# S3 method for class 'htna_bootstrap_group'
summary(object, ...)
```

## Arguments

- x, object:

  An `htna_bootstrap_group` from
  [`bootstrap_htna()`](https://sonsoles.me/htna/reference/bootstrap_htna.md).

- ...:

  Passed to the method of each cohort.

## Value

[`print()`](https://rdrr.io/r/base/print.html) returns `x` invisibly.
[`summary()`](https://rdrr.io/r/base/summary.html) returns a data frame
with one row per cohort and edge: a first column `group`, then the
columns of the per-cohort summary (`from`, `to`, `weight`, `mean`, `sd`,
`p_value`, `sig`, `ci_lower`, `ci_upper`, `cr_lower`, `cr_upper`).

## See also

[`bootstrap_htna()`](https://sonsoles.me/htna/reference/bootstrap_htna.md),
[`plot_htna_bootstrap()`](https://sonsoles.me/htna/reference/plot_htna_bootstrap.md).

## Examples

``` r
data(human_ai)
grp <- build_htna(human_ai, actor_type = "actor_type", group = "phase")
#> Metadata aggregated per session: ties resolved by first occurrence in 'cluster' (24 sessions), 'actor_type' (9 sessions)
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (18 sessions), 'actor_type' (15 sessions)
boots <- bootstrap_htna(grp, iter = 20, seed = 1)
boots
#> HTNA bootstrap, 2 cohorts
#> 
#> --- Early ---
#>   Edge                   Mean     95% CI          p
#>   -----------------------------------------------
#>   Delegate → Plan        0.686  [0.623, 0.736]  *  
#>   Ask → Plan             0.437  [0.404, 0.466]  *  
#>   Check → Execute        0.433  [0.394, 0.462]  *  
#>   Refine → Specify       0.408  [0.320, 0.454]  *  
#>   Plan → Request         0.346  [0.304, 0.377]  *  
#>   ... and 30 more significant edges
#> 
#> Bootstrap Network  [Transition Network (relative) | directed]
#>   Iterations : 20  |  Nodes : 12
#>   Edges      : 35 significant / 135 total
#>   CI         : 95%  |  Inference: stability  |  CR [0.75, 1.25]
#> 
#> --- Late ---
#>   Edge                   Mean     95% CI          p
#>   -----------------------------------------------
#>   Delegate → Plan        0.520  [0.478, 0.599]  *  
#>   Refine → Specify       0.422  [0.368, 0.457]  *  
#>   Ask → Plan             0.391  [0.355, 0.432]  *  
#>   Check → Execute        0.383  [0.348, 0.418]  *  
#>   Repair → Execute       0.374  [0.291, 0.446]  *  
#>   ... and 30 more significant edges
#> 
#> Bootstrap Network  [Transition Network (relative) | directed]
#>   Iterations : 20  |  Nodes : 12
#>   Edges      : 35 significant / 134 total
#>   CI         : 95%  |  Inference: stability  |  CR [0.75, 1.25]
head(summary(boots))
#>   group from        to     weight       mean          sd    p_value   sig
#> 1 Early  Ask       Ask 0.01827676 0.01776920 0.003659615 0.33333333 FALSE
#> 2 Early  Ask     Check 0.06875544 0.06993415 0.009490308 0.09523810 FALSE
#> 3 Early  Ask   Execute 0.03307224 0.03294129 0.006434542 0.19047619 FALSE
#> 4 Early  Ask Frustrate 0.09921671 0.09759578 0.009900554 0.04761905  TRUE
#> 5 Early  Ask   Inquire 0.05395997 0.05352095 0.008149553 0.09523810 FALSE
#> 6 Early  Ask      Plan 0.43342037 0.43720429 0.017663924 0.04761905  TRUE
#>     ci_lower   ci_upper   cr_lower   cr_upper
#> 1 0.01222994 0.02515550 0.01370757 0.02284595
#> 2 0.05630883 0.08927505 0.05156658 0.08594430
#> 3 0.02410374 0.04570021 0.02480418 0.04134030
#> 4 0.08516043 0.11876978 0.07441253 0.12402089
#> 5 0.04414132 0.07000975 0.04046997 0.06744996
#> 6 0.40420839 0.46627655 0.32506527 0.54177546
```
