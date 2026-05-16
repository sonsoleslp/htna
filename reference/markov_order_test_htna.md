# Markov-Order Adequacy Test

htna-named alias of
[`Nestimate::markov_order_test()`](https://rdrr.io/pkg/Nestimate/man/markov_order_test.html).
Tests whether a first-order Markov model is adequate for the observed
sequences, or whether a higher order is required, by comparing the
empirical transition structure against orders `1..max_order` via
permutation.

## Usage

``` r
markov_order_test_htna(
  data,
  max_order = 3L,
  n_perm = 500L,
  alpha = 0.05,
  parallel = FALSE,
  n_cores = 2L,
  seed = NULL
)
```

## Arguments

- data:

  A data.frame (wide format, one sequence per row) or list of character
  vectors (one per trajectory). NAs are treated as end of sequence.

- max_order:

  Integer. Highest Markov order to test. Default 3.

- n_perm:

  Integer. Number of within-\\w\\ permutations per order. Default 500.

- alpha:

  Numeric. Significance level for order selection. Default 0.05.

- parallel:

  Logical. Use
  [`parallel::mclapply`](https://rdrr.io/r/parallel/mclapply.html) for
  permutations. Default `FALSE` (set `TRUE` only on Unix-like systems).

- n_cores:

  Integer. Cores for parallel execution. Default 2.

- seed:

  Optional integer seed for reproducibility.

## Value

An object of class `markov_order_test` with components `test_table`,
`optimal_order`, and the inputs. See
[`Nestimate::markov_order_test()`](https://rdrr.io/pkg/Nestimate/man/markov_order_test.html)
for full details and the corresponding
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) method.

## Details

Operates on raw sequence data (a list of character vectors or a
wide-format data frame), not on an htna network object. Use alongside
[`reliability_htna()`](https://sonsoles.me/htna/reference/reliability_htna.md)
and
[`casedrop_reliability_htna()`](https://sonsoles.me/htna/reference/casedrop_reliability_htna.md)
when assessing whether a Markov-1 transition network is appropriate
before trusting downstream htna analyses.

Suffixed `_htna` to avoid clashing with
[`Nestimate::markov_order_test()`](https://rdrr.io/pkg/Nestimate/man/markov_order_test.html)
when both packages are loaded.

## See also

[`reliability_htna()`](https://sonsoles.me/htna/reference/reliability_htna.md),
[`casedrop_reliability_htna()`](https://sonsoles.me/htna/reference/casedrop_reliability_htna.md),
[`centrality_stability_htna()`](https://sonsoles.me/htna/reference/centrality_stability_htna.md).

## Examples

``` r
# \donttest{
data(human_long, ai_long, package = "Nestimate")
net <- build_htna(list(Human = human_long, AI = ai_long))
#> Warning: A network with one long sequence is not recommended and can't be validated using bootstrap and other confirmatory testings.
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions)
markov_order_test_htna(net$data, max_order = 2, n_perm = 50, seed = 1)
#> Markov Order Test  [within-w permutation, n_perm = 50, alpha = 0.050]
#>   428 sequences / 19346 observations / 17 states
#> 
#>   Selected order  BIC: 1   AIC: 2   permutation-LRT: 2
#> 
#>  order    loglik      AIC      BIC   df       g2 p_permutation p_asymptotic
#>      0 -48676.14 97384.28 97510.21   NA       NA            NA           NA
#>      1 -40117.17 80724.35 82652.56  256 17081.82    0.01960784            0
#>      2 -35329.94 74293.88 88594.10 3118  9023.34    0.01960784            0
#>  significant
#>           NA
#>         TRUE
#>         TRUE
# }
```
