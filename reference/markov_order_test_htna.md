# Test the Markov order of the sessions

`markov_order_test_htna()` tests the Markov order of the session
sequences of an htna network (or of raw sequence data). The Markov-order
test compares the fit of models of increasing order (zero, one, two and
so on) with likelihood-ratio statistics, judged against a permutation
null, and reports the information criteria of each order. A transition
network is a first-order Markov model: the next code is assumed to
depend only on the current one. When the next code also depends on the
codes before it, a first-order network misses part of the process and a
higher order describes it better.

The orders are tested in turn: the selected `optimal_order` is the
highest order reached before the first step that is not significant at
`alpha`, and equals `max_order` when every step is significant, in which
case the search should be widened. The orders preferred by BIC and AIC
are reported alongside. `markov_order_test_htna()` runs Nestimate's
`markov_order_test()` on the sequences and returns a `net_markov_order`
object.

Use it before interpreting a first-order network, together with
[`reliability_htna()`](https://sonsoles.me/htna/reference/reliability_htna.md)
and
[`casedrop_reliability_htna()`](https://sonsoles.me/htna/reference/casedrop_reliability_htna.md)
for the stability of the estimated weights.

## Usage

``` r
markov_order_test_htna(data, ...)
```

## Arguments

- data:

  An htna network from
  [`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md), or
  sequence data (a wide data frame or a list of character vectors).

- ...:

  Settings: `max_order` (default 3), `n_perm` (permutations, 500),
  `alpha` (0.05), `parallel` (`FALSE`), `n_cores` (2), `seed` and `dof`
  (`"paths"` or `"observed"`).

## Value

An object of class `net_markov_order` with
[`print()`](https://rdrr.io/r/base/print.html),
[`summary()`](https://rdrr.io/r/base/summary.html) and
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) methods.
[`summary()`](https://rdrr.io/r/base/summary.html) returns a data frame
with one row per tested order and columns `order`, `loglik`, `AIC`,
`BIC`, `df`, `g2`, `p_permutation`, `p_asymptotic` and `significant`.
The object also holds `optimal_order`, `bic_order` and `aic_order`.

## See also

[`reliability_htna()`](https://sonsoles.me/htna/reference/reliability_htna.md),
[`casedrop_reliability_htna()`](https://sonsoles.me/htna/reference/casedrop_reliability_htna.md),
[`centrality_stability_htna()`](https://sonsoles.me/htna/reference/centrality_stability_htna.md).

## Examples

``` r
data(human_ai)
net <- build_htna(human_ai, actor_type = "actor_type")
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions), 'actor_type' (24 sessions)
# n_perm = 20 keeps the example fast; use the default 500 in practice.
mo <- markov_order_test_htna(net, max_order = 2, n_perm = 20, seed = 1)
mo
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
summary(mo)
#>   order    loglik      AIC      BIC   df        g2 p_permutation p_asymptotic
#> 1     0 -43741.91 87505.83 87592.40   NA        NA            NA           NA
#> 2     1 -37315.96 74899.92 75954.53  121 12815.987    0.04761905            0
#> 3     2 -33812.56 70683.12 82716.72 1272  6481.756    0.04761905            0
#>   significant
#> 1          NA
#> 2        TRUE
#> 3        TRUE
```
