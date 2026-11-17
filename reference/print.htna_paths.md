# Print a table of paths or meta-paths

[`print()`](https://rdrr.io/r/base/print.html) prints the result of
[`extract_meta_paths()`](https://sonsoles.me/htna/reference/extract_meta_paths.md):
a header with the level (concrete paths or meta-paths), the template if
one was given, the number of sessions, the lengths and gaps counted, and
then the first `n` rows with support, frequency and lift rounded for
reading. It returns `x` invisibly. The result is itself a data frame, so
all rows and full precision are available with
[`utils::head()`](https://rdrr.io/r/utils/head.html),
[`subset()`](https://rdrr.io/r/base/subset.html) or
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html).

## Usage

``` r
# S3 method for class 'htna_paths'
print(x, n = 10L, ...)
```

## Arguments

- x:

  A result of
  [`extract_meta_paths()`](https://sonsoles.me/htna/reference/extract_meta_paths.md).

- n:

  Number of rows to print (default 10).

- ...:

  Unused.

## Value

`x`, invisibly.

## See also

[`extract_meta_paths()`](https://sonsoles.me/htna/reference/extract_meta_paths.md).

## Examples

``` r
data(human_ai)
net <- build_htna(human_ai, actor_type = "actor_type")
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions), 'actor_type' (24 sessions)
paths <- extract_meta_paths(net, length = 3)
print(paths, n = 5)
#> Patterns (state-level) over 429 sequences
#> Rows: 1058 | Lengths: 3 | Gaps: 0
#>                     schema      meta_schema length gap count n_seq support
#>  Request->Execute->Request Human->AI->Human      3   0   401   194   0.452
#>  Execute->Request->Execute    AI->Human->AI      3   0   391   180   0.420
#>      Request->Specify->Ask Human->Human->AI      3   0   344   202   0.471
#>         Specify->Ask->Plan    Human->AI->AI      3   0   340   197   0.459
#>         Ask->Plan->Request    AI->AI->Human      3   0   303   197   0.459
#>  frequency  lift
#>      0.022  5.00
#>      0.021  4.65
#>      0.019  6.15
#>      0.018 11.65
#>      0.016  9.77
#> ... (1053 more)
```
