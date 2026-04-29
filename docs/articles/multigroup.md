# Multi-Group HTNA (3+ Actor Groups)

HTNA is not limited to two actor groups. This vignette demonstrates how
to build and visualise a heterogeneous transition network with three
groups.

## Simulating a three-actor interaction

We simulate a collaborative coding session with three actors – a
Student, an AI assistant, and a System (e.g. IDE or automated tooling) –
each with four distinct codes:

``` r

library(htna)

set.seed(42)

student_codes <- c("Ask", "Confirm", "Revise", "Evaluate")
ai_codes      <- c("Suggest", "Explain", "Execute", "Clarify")
system_codes  <- c("Log", "Notify", "Compile", "Sync")
sessions      <- paste0("S", sprintf("%03d", 1:30))

make_events <- function(codes, sessions, offset) {
  rows <- lapply(sessions, function(sid) {
    n <- sample(15:25, 1)
    data.frame(
      session_id       = sid,
      code             = sample(codes, n, replace = TRUE),
      order_in_session = seq_len(n) * 3L - offset,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

student_data <- make_events(student_codes, sessions, 2L)
ai_data      <- make_events(ai_codes, sessions, 1L)
system_data  <- make_events(system_codes, sessions, 0L)
```

## Building a 3-group network

Pass all three groups as a named list:

``` r

net3 <- build_htna(
  list(Student = student_data, AI = ai_data, System = system_data)
)
net3
#> Transition Network (relative probabilities) [directed]
#>   Weights: [0.006, 0.289]  |  mean: 0.108
#> 
#>   Weight matrix:
#>              Ask Clarify Compile Confirm Evaluate Execute Explain   Log Notify
#>   Ask      0.000   0.204   0.020   0.007    0.000   0.279   0.224 0.027  0.007
#>   Clarify  0.007   0.007   0.271   0.007    0.014   0.000   0.014 0.264  0.188
#>   Compile  0.230   0.025   0.006   0.261    0.211   0.012   0.012 0.000  0.012
#>   Confirm  0.000   0.208   0.023   0.000    0.000   0.231   0.243 0.006  0.012
#>   Evaluate 0.000   0.219   0.008   0.000    0.000   0.242   0.289 0.000  0.016
#>   Execute  0.013   0.020   0.260   0.020    0.000   0.007   0.000 0.227  0.200
#>   Explain  0.026   0.000   0.237   0.000    0.013   0.020   0.000 0.204  0.289
#>   Log      0.228   0.020   0.000   0.248    0.195   0.027   0.007 0.040  0.007
#>   Notify   0.218   0.027   0.014   0.286    0.197   0.000   0.027 0.014  0.014
#>   Revise   0.000   0.281   0.016   0.000    0.008   0.195   0.266 0.000  0.016
#>   Suggest  0.007   0.014   0.246   0.021    0.007   0.007   0.007 0.246  0.261
#>   Sync     0.237   0.007   0.007   0.230    0.193   0.022   0.007 0.015  0.022
#>            Revise Suggest  Sync
#>   Ask       0.000   0.218 0.014
#>   Clarify   0.007   0.000 0.222
#>   Compile   0.211   0.012 0.006
#>   Confirm   0.006   0.260 0.012
#>   Evaluate  0.000   0.211 0.016
#>   Execute   0.000   0.020 0.233
#>   Explain   0.013   0.000 0.197
#>   Log       0.195   0.020 0.013
#>   Notify    0.177   0.020 0.007
#>   Revise    0.000   0.188 0.031
#>   Suggest   0.007   0.000 0.176
#>   Sync      0.222   0.030 0.007 
#> 
#>   Initial probabilities:
#>   Confirm       0.433  ████████████████████████████████████████
#>   Ask           0.200  ██████████████████
#>   Revise        0.200  ██████████████████
#>   Evaluate      0.167  ███████████████
#>   Clarify       0.000  
#>   Compile       0.000  
#>   Execute       0.000  
#>   Explain       0.000  
#>   Log           0.000  
#>   Notify        0.000  
#>   Suggest       0.000  
#>   Sync          0.000
```

The actor partition now contains three levels:

``` r

table(net3$node_groups$group)
#> 
#> Student      AI  System 
#>       4       4       4
```

## Plotting

With three or more groups,
[`plot_htna()`](https://sonsoles.me/htna/reference/plot_htna.md)
automatically switches to a polygon (triangle) layout. The three colours
from the default HTNA palette are applied in order:

``` r

plot_htna(net3)
```

![](multigroup_files/figure-html/unnamed-chunk-4-1.png)

You can switch to other layouts as well:

``` r

plot_htna(net3, layout = "auto")
```

![](multigroup_files/figure-html/unnamed-chunk-5-1.png)

## Meta-paths across three actors

Meta-paths now span all three actor types. For example, you can search
for paths where the System mediates between Student and AI:

``` r

extract_meta_paths(net3, schema = "Student->System->AI")
#> Meta-paths (type-level) over 30 sequences
#> (no rows met the filters)
```

Or enumerate all length-3 meta-paths:

``` r

extract_meta_paths(net3, length = 3)
#> Meta-paths (type-level) over 30 sequences
#> Rows: 17 | Lengths: 3 | Gaps: 0
#>                    schema length gap count n_seq support frequency lift
#>       Student->AI->System      3   0   510    30   1.000     0.295 7.98
#>       AI->System->Student      3   0   497    30   1.000     0.288 7.78
#>       System->Student->AI      3   0   487    30   1.000     0.282 7.62
#>            AI->System->AI      3   0    41    10   0.333     0.024 0.62
#>        System->AI->System      3   0    37    10   0.333     0.021 0.56
#>   System->Student->System      3   0    32    10   0.333     0.019 0.50
#>  Student->System->Student      3   0    27     9   0.300     0.016 0.44
#>      Student->AI->Student      3   0    24     7   0.233     0.014 0.39
#>           AI->Student->AI      3   0    24     7   0.233     0.014 0.38
#>    System->System->System      3   0    16     7   0.233     0.009 0.24
#> ... (7 more)
```
