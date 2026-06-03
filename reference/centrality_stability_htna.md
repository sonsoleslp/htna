# Centrality Stability for an htna Network

Thin wrapper around
[`Nestimate::centrality_stability()`](https://saqr.me/Nestimate/reference/centrality_stability.html)
that validates the input is an htna network (or `htna_group`), forwards
the call, and tags the result with an `htna_stability` class so callers
can dispatch on the htna form.

## Usage

``` r
centrality_stability_htna(
  x,
  measures = c("InStrength", "OutStrength", "Betweenness"),
  iter = 1000L,
  drop_prop = seq(0.1, 0.9, by = 0.1),
  threshold = 0.7,
  certainty = 0.95,
  method = "pearson",
  centrality_fn = NULL,
  loops = FALSE,
  seed = NULL
)
```

## Arguments

- x:

  An htna network from
  [`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md) or
  an `htna_group`.

- measures:

  Character vector of centrality measures to assess. Default
  `c("InStrength", "OutStrength", "Betweenness")` — the three measures
  whose values are bit-equal between htna's `cograph` engine and
  Nestimate's default centrality implementation. To assess htna's nine
  measures (including the three that differ between engines:
  `BetweennessRSP`, `Diffusion`, `Clustering`), pass them explicitly
  together with a custom `centrality_fn`.

- iter:

  Integer. Number of resamples per drop proportion. Default `1000`.

- drop_prop:

  Numeric vector. Proportions of sessions to drop. Default
  `seq(0.1, 0.9, by = 0.1)`.

- threshold:

  Numeric in \[0, 1\]. Correlation threshold for the case-dropping
  stability coefficient. Default `0.7`.

- certainty:

  Numeric in \[0, 1\]. Probability of meeting the threshold required for
  a drop proportion to count as stable. Default `0.95`.

- method:

  Correlation method, one of `"pearson"`, `"spearman"`, `"kendall"`.
  Default `"pearson"`.

- centrality_fn:

  Optional function to compute centralities. Default `NULL` (use
  Nestimate's internal implementation). See
  [`Nestimate::centrality_stability()`](https://saqr.me/Nestimate/reference/centrality_stability.html)
  for the expected signature.

- loops:

  Logical. Whether to retain self-loops. Default `FALSE`.

- seed:

  Optional integer seed for reproducibility. Default `NULL` (no seed
  reset).

## Value

For a single htna network, an object of class
`c("htna_stability", "net_stability")` with the same components as
[`Nestimate::centrality_stability()`](https://saqr.me/Nestimate/reference/centrality_stability.html):
`cs` (the stability coefficients, one per measure), `correlations`
(per-drop-proportion correlation traces), and the parameters that were
used. For an `htna_group`, a named list of such objects with class
`c("htna_stability_group", "list")`.

## Details

For an `htna_group`, the call is iterated per cohort and the result is a
named list (one `htna_stability` per cohort), with class
`c("htna_stability_group", "list")`.

## See also

[`Nestimate::centrality_stability()`](https://saqr.me/Nestimate/reference/centrality_stability.html),
[`bootstrap_htna()`](https://sonsoles.me/htna/reference/bootstrap_htna.md),
[`reliability_htna()`](https://sonsoles.me/htna/reference/reliability_htna.md).

## Examples

``` r
if (FALSE) { # \dontrun{
data(human_long, ai_long, package = "Nestimate")
net <- build_htna(list(Human = human_long, AI = ai_long))
cs  <- centrality_stability_htna(net, iter = 100, seed = 1)
cs$cs
} # }
```
