# Compute node centralities of an htna network

`centralities_htna()` computes the node centralities of an htna network,
with the actor type of every code kept. Centrality measures rank the
codes of a transition network by their position in it: how much
transition probability a code sends and receives (out- and in-strength),
how close it is to the other codes (closeness), how often it lies on the
shortest paths between other codes (betweenness), and how far its
influence spreads (diffusion). Because the actor type is kept, the table
shows whether the human or the AI codes hold the central positions, for
example which actor's codes broker the hand-overs.

`centralities_htna()` computes nine measures: `OutStrength`,
`InStrength`, `ClosenessIn`, `ClosenessOut`, `Closeness`, `Betweenness`,
`BetweennessRSP` (randomized shortest-path betweenness), `Diffusion` and
`Clustering` (weighted clustering coefficient). Path-based measures
treat a larger transition weight as a shorter distance (the weights are
inverted); strength, diffusion and clustering use the weights as they
are. For a grouped network the measures are computed per cohort. It
returns a data frame with one row per code (per cohort and code for a
grouped network) and one column per measure.

Draw the result with
[`plot_centralities()`](https://sonsoles.me/htna/reference/plot_centralities.md).
To check whether the ranking of the codes would survive dropping
sessions use
[`centrality_stability_htna()`](https://sonsoles.me/htna/reference/centrality_stability_htna.md).

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

  Names of the measures to compute. Default: all nine.

- ...:

  Further arguments for the centrality computations.

## Value

A data frame of class `c("htna_centralities", "data.frame")`, one row
per node, with columns `node`, `actor` (its actor type) and one numeric
column per measure. For an `htna_group`, the rows of all cohorts are
stacked (one row per cohort and node) with a leading `group` column.

## Conditions

`htna_bad_input` when `x` is not an htna network or a measure name is
unknown; `htna_empty_input` for a grouped network without cohorts;
`htna_unnamed_nodes` when the nodes of the network have no names.

## See also

[`plot_centralities()`](https://sonsoles.me/htna/reference/plot_centralities.md),
[`centrality_stability_htna()`](https://sonsoles.me/htna/reference/centrality_stability_htna.md),
[`edge_betweenness_htna()`](https://sonsoles.me/htna/reference/edge_betweenness_htna.md)

## Examples

``` r
data(human_ai)
net <- build_htna(human_ai, actor_type = "actor_type")
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions), 'actor_type' (24 sessions)
centralities_htna(net)
#> Note: Weights inverted (1/w^1) for path-based measures (invert_weights=TRUE). Higher weights = shorter paths.
#> Note: Weights inverted (1/w^1) for path-based measures (invert_weights=TRUE). Higher weights = shorter paths.
#> Note: Weights inverted (1/w^1) for path-based measures (invert_weights=TRUE). Higher weights = shorter paths.
#> Note: Weights inverted (1/w^1) for path-based measures (invert_weights=TRUE). Higher weights = shorter paths.
#>         node actor OutStrength InStrength ClosenessIn ClosenessOut  Closeness
#> 1        Ask    AI   0.9817642  1.5266008 0.012587425  0.006192077 0.01670617
#> 2      Check Human   0.9499230  0.7522288 0.008035193  0.006492281 0.01340661
#> 3   Delegate    AI   1.0000000  0.1740318 0.003015314  0.007567519 0.01315340
#> 4    Execute    AI   0.9256198  2.0338718 0.016720393  0.006367002 0.01784416
#> 5  Frustrate Human   0.8861885  0.9672100 0.010511066  0.006322199 0.01169411
#> 6    Inquire Human   0.9671362  0.5147284 0.006592669  0.007311931 0.01399616
#> 7       Plan    AI   0.9967742  1.2496499 0.012325008  0.006926899 0.01675952
#> 8     Refine Human   1.0000000  0.4703420 0.005943026  0.006143417 0.01250189
#> 9     Repair    AI   0.9960474  0.1995057 0.002655997  0.007958047 0.01404200
#> 10    Report    AI   0.9225146  0.5246300 0.005125410  0.007099789 0.01180054
#> 11   Request Human   0.9329032  1.6926517 0.016089323  0.005980693 0.01812498
#> 12   Specify Human   0.8991424  1.3525625 0.013064153  0.006197358 0.01619607
#>    Betweenness BetweennessRSP Diffusion Clustering
#> 1           13             91  8.778380  0.1543463
#> 2            0             42  8.354335  0.2074548
#> 3            0              3  9.003808  0.1868121
#> 4           29            118  8.186741  0.1359230
#> 5            2             61  7.948727  0.2014196
#> 6           14             27  8.615216  0.1703483
#> 7           25             63  8.715568  0.1531488
#> 8            0             23  8.751161  0.2393259
#> 9            0              1  8.852837  0.2039989
#> 10           0             17  8.186046  0.1557107
#> 11          19            110  8.181829  0.1842378
#> 12          15             88  8.008017  0.2112340
centralities_htna(net, measures = c("InStrength", "Betweenness"))
#> Note: Weights inverted (1/w^1) for path-based measures (invert_weights=TRUE). Higher weights = shorter paths.
#>         node actor InStrength Betweenness
#> 1        Ask    AI  1.5266008          13
#> 2      Check Human  0.7522288           0
#> 3   Delegate    AI  0.1740318           0
#> 4    Execute    AI  2.0338718          29
#> 5  Frustrate Human  0.9672100           2
#> 6    Inquire Human  0.5147284          14
#> 7       Plan    AI  1.2496499          25
#> 8     Refine Human  0.4703420           0
#> 9     Repair    AI  0.1995057           0
#> 10    Report    AI  0.5246300           0
#> 11   Request Human  1.6926517          19
#> 12   Specify Human  1.3525625          15
```
