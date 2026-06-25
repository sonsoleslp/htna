# Association Rule Mining over Transitions

htna-named alias of
[`Nestimate::association_rules()`](https://saqr.me/Nestimate/reference/association_rules.html).
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
[`Nestimate::association_rules()`](https://saqr.me/Nestimate/reference/association_rules.html)
for details.

## Details

Foundation function in the htna-aware exploratory family. Works on
transition-count input produced by
[`frequencies_htna()`](https://sonsoles.me/htna/reference/frequencies_htna.md)
or
[`Nestimate::build_network()`](https://saqr.me/Nestimate/reference/build_network.html);
the rules carry over to htna networks since the underlying transition
counts are the same.

Suffixed `_htna` to avoid clashing with
[`Nestimate::association_rules()`](https://saqr.me/Nestimate/reference/association_rules.html)
when both packages are loaded.

## See also

[`frequencies_htna()`](https://sonsoles.me/htna/reference/frequencies_htna.md),
[`mosaic_plot_htna()`](https://sonsoles.me/htna/reference/mosaic_plot_htna.md).

## Examples

``` r
# \donttest{
data(human_ai)
net <- build_htna(human_ai, actor_type = "actor_type")
#> Warning: A network with one long sequence is not recommended and can't be validated using bootstrap and other confirmatory testings.
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions), 'actor_type' (24 sessions)
association_rules_htna(net, max_length = 3L)
#> Association Rules  [1054 rules | 12 items | 429 transactions]
#>   Support >= 0.10  |  Confidence >= 0.50  |  Lift >= 1.00
#> 
#>   Top rules (by lift):
#>     1. Repair -> Inquire, Report  (sup=0.217 conf=0.721 lift=1.65)
#>     2. Repair -> Check, Report  (sup=0.233 conf=0.775 lift=1.61)
#>     3. Inquire, Repair -> Report  (sup=0.217 conf=0.861 lift=1.56)
#>     4. Repair -> Refine, Report  (sup=0.166 conf=0.550 lift=1.55)
#>     5. Repair -> Inquire, Refine  (sup=0.172 conf=0.574 lift=1.55)
#>     6. Repair -> Execute, Report  (sup=0.245 conf=0.814 lift=1.55)
#>     7. Repair -> Frustrate, Report  (sup=0.245 conf=0.814 lift=1.52)
#>     8. Repair -> Plan, Report  (sup=0.245 conf=0.814 lift=1.52)
#>     9. Check, Repair -> Report  (sup=0.233 conf=0.840 lift=1.52)
#>     10. Execute, Repair -> Report  (sup=0.245 conf=0.840 lift=1.52)
#>     ... and 1044 more rules
# }
```
