# Mine association rules from the sessions of a network

`association_rules_htna()` mines association rules from the sessions of
an htna network: rules that describe which codes tend to occur together
in the same session, regardless of their order. A rule "A, B =\> C"
states that sessions containing codes A and B also tend to contain code
C. Support is the share of sessions containing all codes of the rule,
confidence the share of sessions with the antecedent that also contain
the consequent, and lift the ratio of the confidence to the overall
share of the consequent. In a heterogeneous network a rule can join
codes of different actors, for example a human request and the AI
actions that accompany it.

`association_rules_htna()` runs Nestimate's `association_rules()` on the
network: it treats every session as one transaction (the set of codes it
contains), finds the frequent itemsets with the Apriori algorithm, and
keeps the rules that pass the support, confidence and lift thresholds.
It returns a `net_association_rules` object whose rules table has one
row per rule.

The rules ignore order. For ordered patterns use
[`extract_meta_paths()`](https://sonsoles.me/htna/reference/extract_meta_paths.md);
for associations between codes and sessions use
[`mosaic_plot_htna()`](https://sonsoles.me/htna/reference/mosaic_plot_htna.md).

## Usage

``` r
association_rules_htna(x, ...)
```

## Arguments

- x:

  An htna network from
  [`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md), or
  a binary session-by-code matrix.

- ...:

  Thresholds: `min_support` (default 0.1), `min_confidence` (0.5),
  `min_lift` (1) and `max_length` (largest itemset, 5).

## Value

An object of class `net_association_rules` with
[`print()`](https://rdrr.io/r/base/print.html),
[`summary()`](https://rdrr.io/r/base/summary.html) and
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) methods. Its
rules table has one row per rule, with columns `antecedent`,
`consequent`, `support`, `confidence`, `lift`, `conviction`, `count` and
`n_transactions`.

## See also

[`extract_meta_paths()`](https://sonsoles.me/htna/reference/extract_meta_paths.md)
for ordered patterns,
[`frequencies_htna()`](https://sonsoles.me/htna/reference/frequencies_htna.md),
[`mosaic_plot_htna()`](https://sonsoles.me/htna/reference/mosaic_plot_htna.md).

## Examples

``` r
data(human_ai)
net <- build_htna(human_ai, actor_type = "actor_type")
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
```
