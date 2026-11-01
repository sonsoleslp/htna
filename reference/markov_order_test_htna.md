# Markov-Order Adequacy Test

htna-named wrapper for
[`Nestimate::markov_order_test()`](https://pak.dynasite.org/Nestimate/reference/markov_order_test.html).
Tests whether a first-order Markov model is adequate for the observed
sequences, or whether a higher order is required, by comparing the
empirical transition structure against orders `1..max_order` via
permutation.

## Usage

``` r
markov_order_test_htna(data, ...)
```

## Arguments

- data:

  An htna network from
  [`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md), or
  raw sequence data: a list of character vectors or a wide-format data
  frame. See
  [`Nestimate::markov_order_test()`](https://pak.dynasite.org/Nestimate/reference/markov_order_test.html).

- ...:

  Additional arguments passed to
  [`Nestimate::markov_order_test()`](https://pak.dynasite.org/Nestimate/reference/markov_order_test.html),
  such as `max_order`, `n_perm`, `alpha`, `parallel`, `n_cores`, and
  `seed`. See that function for the full, current argument list.

## Value

An object of class `net_markov_order` with components `optimal_order`
(sequential permutation-test order), `bic_order`, `aic_order`,
`test_table` (a data frame with one row per tested order and columns
`order`, `loglik`, `AIC`, `BIC`, `df`, `g2`, `p_permutation`,
`p_asymptotic`, `significant`), the permutation null distributions,
fitted transition matrices, and the inputs.
[`print()`](https://rdrr.io/r/base/print.html),
[`summary()`](https://rdrr.io/r/base/summary.html) and
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) methods are
supplied by Nestimate; see
[`Nestimate::markov_order_test()`](https://pak.dynasite.org/Nestimate/reference/markov_order_test.html).

## Details

Accepts an htna network directly (its session sequences are tested), or
raw sequence data (a list of character vectors or a wide-format data
frame). Use alongside
[`reliability_htna()`](https://sonsoles.me/htna/reference/reliability_htna.md)
and
[`casedrop_reliability_htna()`](https://sonsoles.me/htna/reference/casedrop_reliability_htna.md)
when assessing whether a Markov-1 transition network is appropriate
before trusting downstream htna analyses.

The orders are tested sequentially: `optimal_order` is the highest order
`k` for which every step from order `j - 1` to `j`, `j = 1, ..., k`, is
significant at `alpha`. It is therefore one less than the first order
whose step is not significant, and equals `max_order` when every step is
significant (in which case the search should be widened). The BIC- and
AIC-preferred orders are reported alongside.

Suffixed `_htna` to avoid clashing with
[`Nestimate::markov_order_test()`](https://pak.dynasite.org/Nestimate/reference/markov_order_test.html)
when both packages are loaded.

## See also

[`reliability_htna()`](https://sonsoles.me/htna/reference/reliability_htna.md),
[`casedrop_reliability_htna()`](https://sonsoles.me/htna/reference/casedrop_reliability_htna.md),
[`centrality_stability_htna()`](https://sonsoles.me/htna/reference/centrality_stability_htna.md).

## Examples

``` r
data(human_ai)
net <- build_htna(human_ai, actor_type = "actor_type")
#> A network with one long sequence is not recommended and can't be validated using bootstrap and other confirmatory testings.
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions), 'actor_type' (24 sessions)
# Few permutations to keep the example fast; use the default 500 in practice.
markov_order_test_htna(net, max_order = 2, n_perm = 20, seed = 1)
#> Markov Order Test  [within-w permutation, n_perm = 20, alpha = 0.050]
#>   428 sequences / 19346 observations / 12 states
#> 
#>   Selected order  BIC: 1   AIC: 2   permutation-LRT: 2
#> 
#>  order    loglik      AIC      BIC   df       g2 p_permutation p_asymptotic
#>      0 -43741.91 87505.83 87592.40   NA       NA            NA           NA
#>      1 -37315.96 74899.92 75954.53  121 12815.99    0.04761905            0
#>      2 -33812.56 70683.12 82716.72 1272  6481.76    0.04761905            0
#>  significant
#>           NA
#>         TRUE
#>         TRUE
```
