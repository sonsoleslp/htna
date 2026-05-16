# Compute Centrality Measures for an htna Network

Computes a fixed panel of centrality measures via
[cograph::cograph](https://sonsoles.me/cograph/reference/cograph.html)
and returns them as a tidy data frame: one row per node, one column per
measure (plus `node`, an `actor` column when the htna partition is set,
and a `group` column when the input is an `htna_group`).

## Usage

``` r
centralities_htna(
  x,
  measures = c("OutStrength", "InStrength", "ClosenessIn", "ClosenessOut", "Closeness",
    "Betweenness", "BetweennessRSP", "Diffusion", "Clustering"),
  ...
)
```

## Arguments

- x:

  An htna network from
  [`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md) or
  an `htna_group`.

- measures:

  Character vector of measure names. Defaults to all nine.

- ...:

  Forwarded to the underlying cograph centrality functions.

## Value

A data frame with one row per node.

## Details

The measures are: `OutStrength`, `InStrength`, `ClosenessIn`,
`ClosenessOut`, `Closeness`, `Betweenness`, `BetweennessRSP`,
`Diffusion`, `Clustering`. Path-based measures (closeness and
betweenness variants) are computed with `invert_weights = TRUE`, since
in transition networks larger weight = stronger link = shorter distance.
Strength and the closed-form measures (`Diffusion`, `Clustering`) use
raw weights.

Mapping from measure name to cograph implementation:

- `OutStrength` →
  [`cograph::centrality_outstrength()`](https://sonsoles.me/cograph/reference/centrality_strength.html)

- `InStrength` →
  [`cograph::centrality_instrength()`](https://sonsoles.me/cograph/reference/centrality_strength.html)

- `ClosenessIn` →
  [`cograph::centrality_incloseness()`](https://sonsoles.me/cograph/reference/centrality_closeness.html)
  (inverted)

- `ClosenessOut` →
  [`cograph::centrality_outcloseness()`](https://sonsoles.me/cograph/reference/centrality_closeness.html)
  (inverted)

- `Closeness` →
  [`cograph::centrality_closeness()`](https://sonsoles.me/cograph/reference/centrality_closeness.html)
  (inverted)

- `Betweenness` →
  [`cograph::centrality_betweenness()`](https://sonsoles.me/cograph/reference/centrality_betweenness.html)
  (inverted)

- `BetweennessRSP` →
  [`cograph::centrality_current_flow_betweenness()`](https://sonsoles.me/cograph/reference/centrality_current_flow_betweenness.html)

- `Diffusion` →
  [`cograph::centrality_diffusion()`](https://sonsoles.me/cograph/reference/centrality_diffusion.html)

- `Clustering` →
  [`cograph::centrality_transitivity()`](https://sonsoles.me/cograph/reference/centrality_transitivity.html)

## Examples

``` r
if (FALSE) { # \dontrun{
data(human_long, ai_long, package = "Nestimate")
net <- build_htna(list(Human = human_long, AI = ai_long))
centralities_htna(net)
} # }
```
