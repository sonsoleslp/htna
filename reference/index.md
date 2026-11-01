# Package index

## Build

Construct, inspect, and convert a heterogeneous transition network

- [`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md) :
  Build a Heterogeneous Transition Network (HTNA)
- [`summary(`*`<htna>`*`)`](https://sonsoles.me/htna/reference/summary.htna.md)
  [`summary(`*`<htna_group>`*`)`](https://sonsoles.me/htna/reference/summary.htna.md)
  : Summarise a Heterogeneous Transition Network
- [`as.igraph(`*`<htna>`*`)`](https://sonsoles.me/htna/reference/as.igraph.htna.md)
  [`as.igraph(`*`<htna_group>`*`)`](https://sonsoles.me/htna/reference/as.igraph.htna.md)
  : Convert an HTNA Network to igraph

## Pruning

Remove weak edges, restore them, and report what was removed

- [`prune_htna()`](https://sonsoles.me/htna/reference/prune_htna.md) :
  Prune Weak Edges from a Transition Network
- [`deprune_htna()`](https://sonsoles.me/htna/reference/deprune_htna.md)
  : Restore All Edges of a Pruned Transition Network
- [`reprune_htna()`](https://sonsoles.me/htna/reference/reprune_htna.md)
  : Re-apply a Previous Pruning Rule
- [`pruning_details_htna()`](https://sonsoles.me/htna/reference/pruning_details_htna.md)
  : Tidy Report of a Network's Pruning

## Frequencies and distributions

Marginal state-level summaries and chi-square mosaics

- [`frequencies_htna()`](https://sonsoles.me/htna/reference/frequencies_htna.md)
  : Tidy Per-Actor Frequency Table for an htna Network
- [`plot_frequencies_htna()`](https://sonsoles.me/htna/reference/plot_frequencies_htna.md)
  : Plot State Frequencies (htna-named)
- [`state_frequencies_htna()`](https://sonsoles.me/htna/reference/state_frequencies_htna.md)
  : Tidy State Frequency Table
- [`state_distribution_htna()`](https://sonsoles.me/htna/reference/state_distribution_htna.md)
  : State Distribution Across Time
- [`mosaic_plot_htna()`](https://sonsoles.me/htna/reference/mosaic_plot_htna.md)
  : Chi-square Mosaic Plot of a Transition Network

## Sequences

Per-session trajectories and k-gram pattern comparison

- [`sequence_plot_htna()`](https://sonsoles.me/htna/reference/sequence_plot_htna.md)
  [`plot_sequences(`*`<htna>`*`)`](https://sonsoles.me/htna/reference/sequence_plot_htna.md)
  : Plot Per-Actor Sequences From an htna Network
- [`plot_sequences()`](https://sonsoles.me/htna/reference/plot_sequences.md)
  : Sequence plot generic
- [`sequence_compare_htna()`](https://sonsoles.me/htna/reference/sequence_compare_htna.md)
  : Subsequence Pattern Comparison Across Groups

## Network plots

Visualise an htna network and contrasts between networks

- [`plot_htna()`](https://sonsoles.me/htna/reference/plot_htna.md)
  [`plot(`*`<htna>`*`)`](https://sonsoles.me/htna/reference/plot_htna.md)
  [`plot(`*`<htna_group>`*`)`](https://sonsoles.me/htna/reference/plot_htna.md)
  : Plot a Heterogeneous Transition Network
- [`plot_htna_bootstrap()`](https://sonsoles.me/htna/reference/plot_htna_bootstrap.md)
  [`plot(`*`<htna_bootstrap>`*`)`](https://sonsoles.me/htna/reference/plot_htna_bootstrap.md)
  : Plot an htna Bootstrap Result
- [`plot_htna_diff()`](https://sonsoles.me/htna/reference/plot_htna_diff.md)
  : Plot the Difference Between Two htna Networks
- [`htna_palette`](https://sonsoles.me/htna/reference/htna_palette.md) :
  Default HTNA colour palette

## Centrality

Compute and visualise node-level centralities

- [`centralities_htna()`](https://sonsoles.me/htna/reference/centralities_htna.md)
  : Compute Centrality Measures for an htna Network
- [`plot_centralities()`](https://sonsoles.me/htna/reference/plot_centralities.md)
  : Plot Centrality Measures

## Edge betweenness

Replace edge weights with shortest-path betweenness scores

- [`edge_betweenness_htna()`](https://sonsoles.me/htna/reference/edge_betweenness_htna.md)
  : Edge Betweenness Network
- [`plot_edge_betweenness_htna()`](https://sonsoles.me/htna/reference/plot_edge_betweenness_htna.md)
  : Plot the Edge Betweenness Network

## Paths and patterns

Discover meta-paths, state-level patterns, and association rules

- [`extract_meta_paths()`](https://sonsoles.me/htna/reference/extract_meta_paths.md)
  : Extract Path Patterns from a Heterogeneous Transition Network
- [`print(`*`<htna_paths>`*`)`](https://sonsoles.me/htna/reference/print.htna_paths.md)
  : Print Method for htna Path/Pattern Objects
- [`association_rules_htna()`](https://sonsoles.me/htna/reference/association_rules_htna.md)
  : Association Rule Mining over Transitions

## Comparing networks

Descriptive, permutation, and Bayesian comparison of htna networks

- [`compare_htna()`](https://sonsoles.me/htna/reference/compare_htna.md)
  : Compare Heterogeneous Transition Networks
- [`plot(`*`<htna_comparison_group>`*`)`](https://sonsoles.me/htna/reference/plot.htna_comparison_group.md)
  : Plot All Comparisons in an HTNA Comparison Group
- [`permutation_htna()`](https://sonsoles.me/htna/reference/permutation_htna.md)
  : Permutation Test for Network Differences
- [`bayes_compare_htna()`](https://sonsoles.me/htna/reference/bayes_compare_htna.md)
  : Bayesian Comparison of Two Networks

## Model assessment

Bootstrap, certainty, reliability, stability, and Markov-order adequacy

- [`bootstrap_htna()`](https://sonsoles.me/htna/reference/bootstrap_htna.md)
  [`bootstrap(`*`<htna>`*`)`](https://sonsoles.me/htna/reference/bootstrap_htna.md)
  : Bootstrap an HTNA Network
- [`bootstrap()`](https://sonsoles.me/htna/reference/bootstrap.md) :
  Bootstrap generic
- [`certainty_htna()`](https://sonsoles.me/htna/reference/certainty_htna.md)
  : Closed-form Edge Certainty of a Network
- [`reliability_htna()`](https://sonsoles.me/htna/reference/reliability_htna.md)
  : Network Reliability for an htna Network
- [`casedrop_reliability_htna()`](https://sonsoles.me/htna/reference/casedrop_reliability_htna.md)
  : Edge-Weight Case-Dropping Stability
- [`centrality_stability_htna()`](https://sonsoles.me/htna/reference/centrality_stability_htna.md)
  : Centrality Stability for an htna Network
- [`plot(`*`<htna_stability>`*`)`](https://sonsoles.me/htna/reference/plot.htna_stability.md)
  : Plot Method for htna Centrality Stability Objects
- [`plot(`*`<htna_stability_group>`*`)`](https://sonsoles.me/htna/reference/plot.htna_stability_group.md)
  : Plot Method for htna Stability Groups
- [`print(`*`<htna_stability>`*`)`](https://sonsoles.me/htna/reference/print.htna_stability.md)
  : Print Method for htna Centrality Stability Objects
- [`print(`*`<htna_stability_group>`*`)`](https://sonsoles.me/htna/reference/print.htna_stability_group.md)
  : Print Method for htna Stability Groups
- [`markov_order_test_htna()`](https://sonsoles.me/htna/reference/markov_order_test_htna.md)
  : Markov-Order Adequacy Test

## Datasets

Sample datasets

- [`human_ai`](https://sonsoles.me/htna/reference/human_ai.md) :
  Simplified Human + AI Interaction Sequences

- [`human_simplified`](https://sonsoles.me/htna/reference/human_simplified.md)
  : Simplified Human Interaction Sequences (per-actor frame)

- [`ai_simplified`](https://sonsoles.me/htna/reference/ai_simplified.md)
  : Simplified AI Interaction Sequences (per-actor frame)

- [`human_ai_codebook`](https://sonsoles.me/htna/reference/human_ai_codebook.md)
  :

  Code-to-Actor-Type Codebook for `human_ai`
