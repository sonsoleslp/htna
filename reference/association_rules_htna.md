# Association Rule Mining over Transitions

htna-named alias of
[`Nestimate::association_rules()`](https://rdrr.io/pkg/Nestimate/man/association_rules.html).
Mines frequent itemsets and association rules over a transition-count
matrix under support / confidence / lift thresholds.

## Usage

``` r
association_rules_htna(
  x,
  min_support = 0.1,
  min_confidence = 0.5,
  min_lift = 1,
  max_length = 5L
)
```

## Arguments

- x:

  Input data. Accepts:

  netobject

  :   Uses `$data` sequences — each sequence becomes a transaction of
      its unique states.

  list

  :   Each element is a character vector of items (one transaction).

  data.frame

  :   Wide format: each row is a transaction, character columns are item
      occurrences. Or a binary matrix (0/1).

  matrix

  :   Binary transaction matrix (rows = transactions, columns = items).

- min_support:

  Numeric. Minimum support threshold. Default: 0.1.

- min_confidence:

  Numeric. Minimum confidence threshold. Default: 0.5.

- min_lift:

  Numeric. Minimum lift threshold. Default: 1.0.

- max_length:

  Integer. Maximum itemset size. Default: 5.

## Value

A list with the discovered rules and frequent itemsets. See
[`Nestimate::association_rules()`](https://rdrr.io/pkg/Nestimate/man/association_rules.html)
for details.

## Details

Foundation function in the htna-aware exploratory family. Works on
transition-count input produced by
[`frequencies_htna()`](https://sonsoles.me/htna/reference/frequencies_htna.md)
or
[`Nestimate::build_network()`](https://rdrr.io/pkg/Nestimate/man/build_network.html);
the rules carry over to htna networks since the underlying transition
counts are the same.

Suffixed `_htna` to avoid clashing with
[`Nestimate::association_rules()`](https://rdrr.io/pkg/Nestimate/man/association_rules.html)
when both packages are loaded.

## See also

[`frequencies_htna()`](https://sonsoles.me/htna/reference/frequencies_htna.md),
[`mosaic_plot_htna()`](https://sonsoles.me/htna/reference/mosaic_plot_htna.md).

## Examples

``` r
# \donttest{
data(human_long, ai_long, package = "Nestimate")
net <- build_htna(list(Human = human_long, AI = ai_long))
#> Warning: A network with one long sequence is not recommended and can't be validated using bootstrap and other confirmatory testings.
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions)
association_rules_htna(net)
#> Association Rules  [87185 rules | 17 items | 429 transactions]
#>   Support >= 0.10  |  Confidence >= 0.50  |  Lift >= 1.00
#> 
#>   Top rules (by lift):
#>     1. Frustrate, Inquire, Report -> Explain, Repair  (sup=0.105 conf=0.570 lift=2.44)
#>     2. Repair, Report -> Explain, Frustrate, Inquire  (sup=0.105 conf=0.776 lift=2.43)
#>     3. Inquire, Repair -> Explain, Frustrate, Verify  (sup=0.152 conf=0.602 lift=2.39)
#>     4. Explain, Frustrate, Verify -> Inquire, Repair  (sup=0.152 conf=0.602 lift=2.39)
#>     5. Correct, Repair -> Explain, Frustrate, Verify  (sup=0.147 conf=0.594 lift=2.36)
#>     6. Explain, Frustrate, Verify -> Correct, Repair  (sup=0.147 conf=0.583 lift=2.36)
#>     7. Repair, Report -> Explain, Frustrate, Request  (sup=0.105 conf=0.776 lift=2.33)
#>     8. Explain, Frustrate, Report -> Inquire, Repair  (sup=0.105 conf=0.584 lift=2.32)
#>     9. Refine, Repair -> Explain, Frustrate, Verify  (sup=0.117 conf=0.581 lift=2.31)
#>     10. Explain, Inquire, Repair -> Frustrate, Report  (sup=0.105 conf=0.506 lift=2.31)
#>     ... and 87175 more rules
# }
```
