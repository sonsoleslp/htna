# Print Method for htna Path/Pattern Objects

Shared print method for objects returned by
[`extract_meta_paths()`](https://sonsoles.me/htna/reference/extract_meta_paths.md)
(class `htna_meta_paths`). Dispatches on the `htna_paths` parent class.

## Usage

``` r
# S3 method for class 'htna_paths'
print(x, n = 10L, ...)
```

## Arguments

- x:

  An `htna_paths` object.

- n:

  Maximum number of rows to print. Default `10`.

- ...:

  Unused.

## Value

The input object, invisibly.

## Examples

``` r
# \donttest{
data(human_ai)
net <- build_htna(human_ai, actor_type = "actor_type")
#> Warning: A network with one long sequence is not recommended and can't be validated using bootstrap and other confirmatory testings.
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions), 'actor_type' (24 sessions)
p <- extract_meta_paths(net, length = 3)
print(p, n = 5)
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
# }
```
