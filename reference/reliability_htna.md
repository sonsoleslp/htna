# Network Reliability for an htna Network

Thin wrapper around
[`Nestimate::network_reliability()`](https://rdrr.io/pkg/Nestimate/man/network_reliability.html)
that preserves the htna actor partition on every model in the returned
`$models` slot, so downstream htna-aware code (plotting, centralities,
etc.) keeps working on the reliability output.

## Usage

``` r
reliability_htna(..., iter = 1000L, split = 0.5, scale = "none", seed = NULL)
```

## Arguments

- ...:

  One or more htna networks built by
  [`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md).

- iter:

  Integer. Number of split-half iterations. Default `1000`.

- split:

  Numeric in (0, 1). Proportion of sessions used for the first
  half-network. Default `0.5`.

- scale:

  One of `"none"`, `"minmax"`, `"standardize"`, `"proportion"`.
  Forwarded to
  [`Nestimate::network_reliability()`](https://rdrr.io/pkg/Nestimate/man/network_reliability.html).

- seed:

  Optional integer seed for reproducibility. Default `NULL` (no seed
  reset).

## Value

An object of class `c("htna_reliability", "net_reliability")`, with the
same components as
[`Nestimate::network_reliability()`](https://rdrr.io/pkg/Nestimate/man/network_reliability.html)
— `iterations`, `summary`, `models`, `iter`, `split`, `scale` — and each
entry of `$models` carrying the htna actor partition (`$nodes$groups`,
`$node_groups`, `$actor_levels`).

## Details

Mirrors the signature of the underlying function: pass one or more
networks via `...` plus the optional `iter`, `split`, `scale`, and
`seed` arguments. All networks passed through `...` must be htna
networks built by
[`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md).

## See also

[`Nestimate::network_reliability()`](https://rdrr.io/pkg/Nestimate/man/network_reliability.html),
[`bootstrap_htna()`](https://sonsoles.me/htna/reference/bootstrap_htna.md).

## Examples

``` r
if (FALSE) { # \dontrun{
data(human_long, ai_long, package = "Nestimate")
net <- build_htna(list(Human = human_long, AI = ai_long))
rel <- reliability_htna(net, iter = 100, seed = 1)
rel$summary
} # }
```
