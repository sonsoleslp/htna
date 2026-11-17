# Package index

## Overview

- [`htna`](https://sonsoles.me/htna/reference/htna-package.md)
  [`htna-package`](https://sonsoles.me/htna/reference/htna-package.md) :
  htna: Heterogeneous Transition Network Analysis

## Building networks

Estimate a heterogeneous transition network, one per cohort, or one per
cluster, and read it as tables.

- [`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md) :
  Build a heterogeneous transition network
- [`summary(`*`<htna>`*`)`](https://sonsoles.me/htna/reference/summary.htna.md)
  [`summary(`*`<htna_group>`*`)`](https://sonsoles.me/htna/reference/summary.htna.md)
  [`print(`*`<summary.htna>`*`)`](https://sonsoles.me/htna/reference/summary.htna.md)
  [`print(`*`<summary.htna_group>`*`)`](https://sonsoles.me/htna/reference/summary.htna.md)
  [`as.data.frame(`*`<summary.htna>`*`)`](https://sonsoles.me/htna/reference/summary.htna.md)
  [`as.data.frame(`*`<summary.htna_group>`*`)`](https://sonsoles.me/htna/reference/summary.htna.md)
  : Summarize a heterogeneous transition network
- [`as.data.frame(`*`<htna>`*`)`](https://sonsoles.me/htna/reference/as.data.frame.htna.md)
  [`as.data.frame(`*`<htna_group>`*`)`](https://sonsoles.me/htna/reference/as.data.frame.htna.md)
  : Tabulate the edges and nodes of an htna network
- [`as.igraph(`*`<htna>`*`)`](https://sonsoles.me/htna/reference/as.igraph.htna.md)
  [`as.igraph(`*`<htna_group>`*`)`](https://sonsoles.me/htna/reference/as.igraph.htna.md)
  : Convert an htna network to an igraph graph

## Network plots

Draw a network with each actor’s codes on its own arc, and the
difference between two networks.

- [`plot_htna()`](https://sonsoles.me/htna/reference/plot_htna.md)
  [`plot(`*`<htna>`*`)`](https://sonsoles.me/htna/reference/plot_htna.md)
  [`plot(`*`<htna_group>`*`)`](https://sonsoles.me/htna/reference/plot_htna.md)
  : Plot a heterogeneous transition network
- [`plot_htna_diff()`](https://sonsoles.me/htna/reference/plot_htna_diff.md)
  : Plot the difference between two htna networks
- [`htna_palette`](https://sonsoles.me/htna/reference/htna_palette.md) :
  Colours of the actor types in htna plots

## Pruning

Set weak edges aside, restore them, and list what was removed.

- [`prune_htna()`](https://sonsoles.me/htna/reference/prune_htna.md) :
  Remove weak edges from a heterogeneous transition network
- [`deprune_htna()`](https://sonsoles.me/htna/reference/deprune_htna.md)
  : Restore the edges removed by pruning
- [`reprune_htna()`](https://sonsoles.me/htna/reference/reprune_htna.md)
  : Re-apply a recorded pruning rule
- [`pruning_details_htna()`](https://sonsoles.me/htna/reference/pruning_details_htna.md)
  : List the edges removed by pruning

## Frequencies and distributions

How often each code occurs, within each actor and overall, and the
associations between codes and sessions.

- [`frequencies_htna()`](https://sonsoles.me/htna/reference/frequencies_htna.md)
  : Count the codes of each actor in a network
- [`plot_frequencies_htna()`](https://sonsoles.me/htna/reference/plot_frequencies_htna.md)
  : Plot the frequencies of the codes of an htna network
- [`state_frequencies_htna()`](https://sonsoles.me/htna/reference/state_frequencies_htna.md)
  : Count the codes of a network over all actors
- [`state_distribution_htna()`](https://sonsoles.me/htna/reference/state_distribution_htna.md)
  : Tabulate the distribution of the codes within each actor
- [`mosaic_plot_htna()`](https://sonsoles.me/htna/reference/mosaic_plot_htna.md)
  : Plot a chi-square mosaic of the transitions of an htna network

## Sequences

The sessions as sequences of codes, and the comparison of short patterns
between groups of sessions.

- [`sequence_plot_htna()`](https://sonsoles.me/htna/reference/sequence_plot_htna.md)
  [`plot_sequences(`*`<htna>`*`)`](https://sonsoles.me/htna/reference/sequence_plot_htna.md)
  : Plot the sessions of an htna network as sequences
- [`plot_sequences()`](https://sonsoles.me/htna/reference/plot_sequences.md)
  : Plot sequences
- [`sequence_compare_htna()`](https://sonsoles.me/htna/reference/sequence_compare_htna.md)
  : Compare short sequence patterns between groups of sessions

## Paths and patterns

Recurring paths, meta-paths of turn-taking, and association rules.

- [`extract_meta_paths()`](https://sonsoles.me/htna/reference/extract_meta_paths.md)
  : Count recurring paths and meta-paths in the sessions
- [`print(`*`<htna_paths>`*`)`](https://sonsoles.me/htna/reference/print.htna_paths.md)
  : Print a table of paths or meta-paths
- [`association_rules_htna()`](https://sonsoles.me/htna/reference/association_rules_htna.md)
  : Mine association rules from the sessions of a network

## Centrality

The position of each code in the network and of each edge on shortest
paths.

- [`centralities_htna()`](https://sonsoles.me/htna/reference/centralities_htna.md)
  : Compute node centralities of an htna network
- [`plot_centralities()`](https://sonsoles.me/htna/reference/plot_centralities.md)
  : Plot node centralities of an htna network
- [`edge_betweenness_htna()`](https://sonsoles.me/htna/reference/edge_betweenness_htna.md)
  : Compute the edge betweenness of an htna network
- [`plot_edge_betweenness_htna()`](https://sonsoles.me/htna/reference/plot_edge_betweenness_htna.md)
  : Plot the edge betweenness of an htna network

## Comparing networks

Descriptive, permutation and Bayesian comparison of two networks or of
the cohorts of a grouped network.

- [`compare_htna()`](https://sonsoles.me/htna/reference/compare_htna.md)
  : Describe the differences between heterogeneous transition networks
- [`print(`*`<htna_comparison>`*`)`](https://sonsoles.me/htna/reference/htna_comparison-methods.md)
  [`summary(`*`<htna_comparison>`*`)`](https://sonsoles.me/htna/reference/htna_comparison-methods.md)
  [`as.data.frame(`*`<htna_comparison>`*`)`](https://sonsoles.me/htna/reference/htna_comparison-methods.md)
  [`print(`*`<htna_comparison_group>`*`)`](https://sonsoles.me/htna/reference/htna_comparison-methods.md)
  [`summary(`*`<htna_comparison_group>`*`)`](https://sonsoles.me/htna/reference/htna_comparison-methods.md)
  [`as.data.frame(`*`<htna_comparison_group>`*`)`](https://sonsoles.me/htna/reference/htna_comparison-methods.md)
  : Print, summarize and tabulate an htna network comparison
- [`plot(`*`<htna_comparison_group>`*`)`](https://sonsoles.me/htna/reference/plot.htna_comparison_group.md)
  : Plot the comparisons of a grouped network
- [`permutation_htna()`](https://sonsoles.me/htna/reference/permutation_htna.md)
  : Test differences between networks by permutation
- [`bayes_compare_htna()`](https://sonsoles.me/htna/reference/bayes_compare_htna.md)
  : Compare networks edge by edge with a Bayesian model

## Model assessment

Certainty of the edges, reliability and stability of the estimates, and
the adequacy of a first-order model.

- [`bootstrap_htna()`](https://sonsoles.me/htna/reference/bootstrap_htna.md)
  [`bootstrap(`*`<htna>`*`)`](https://sonsoles.me/htna/reference/bootstrap_htna.md)
  : Bootstrap the edges of a heterogeneous transition network
- [`bootstrap()`](https://sonsoles.me/htna/reference/bootstrap.md) :
  Bootstrap a network
- [`print(`*`<htna_bootstrap_group>`*`)`](https://sonsoles.me/htna/reference/print.htna_bootstrap_group.md)
  [`summary(`*`<htna_bootstrap_group>`*`)`](https://sonsoles.me/htna/reference/print.htna_bootstrap_group.md)
  : Print and summarize the bootstraps of a grouped network
- [`plot_htna_bootstrap()`](https://sonsoles.me/htna/reference/plot_htna_bootstrap.md)
  [`plot(`*`<htna_bootstrap>`*`)`](https://sonsoles.me/htna/reference/plot_htna_bootstrap.md)
  : Plot the bootstrap-assessed edges of an htna network
- [`certainty_htna()`](https://sonsoles.me/htna/reference/certainty_htna.md)
  : Estimate the closed-form certainty of the edge weights
- [`reliability_htna()`](https://sonsoles.me/htna/reference/reliability_htna.md)
  : Estimate the split-half reliability of a heterogeneous transition
  network
- [`casedrop_reliability_htna()`](https://sonsoles.me/htna/reference/casedrop_reliability_htna.md)
  : Assess the case-dropping stability of the edge weights
- [`centrality_stability_htna()`](https://sonsoles.me/htna/reference/centrality_stability_htna.md)
  : Assess the case-dropping stability of node centralities
- [`print(`*`<htna_stability>`*`)`](https://sonsoles.me/htna/reference/print.htna_stability.md)
  [`print(`*`<htna_stability_group>`*`)`](https://sonsoles.me/htna/reference/print.htna_stability.md)
  : Print centrality stability coefficients
- [`plot(`*`<htna_stability>`*`)`](https://sonsoles.me/htna/reference/plot.htna_stability.md)
  [`plot(`*`<htna_stability_group>`*`)`](https://sonsoles.me/htna/reference/plot.htna_stability.md)
  : Plot centrality stability against the proportion of sessions dropped
- [`as.data.frame(`*`<htna_stability>`*`)`](https://sonsoles.me/htna/reference/as.data.frame.htna_stability.md)
  [`as.data.frame(`*`<htna_stability_group>`*`)`](https://sonsoles.me/htna/reference/as.data.frame.htna_stability.md)
  [`summary(`*`<htna_stability_group>`*`)`](https://sonsoles.me/htna/reference/as.data.frame.htna_stability.md)
  : Tabulate centrality stability results
- [`markov_order_test_htna()`](https://sonsoles.me/htna/reference/markov_order_test_htna.md)
  : Test the Markov order of the sessions

## Data

Coded sessions of programmers working with an AI coding agent.

- [`human_ai`](https://sonsoles.me/htna/reference/human_ai.md) : Human
  and AI turns in vibe-coding sessions
- [`human_simplified`](https://sonsoles.me/htna/reference/human_simplified.md)
  : Human turns of the vibe-coding sessions
- [`ai_simplified`](https://sonsoles.me/htna/reference/ai_simplified.md)
  : AI turns of the vibe-coding sessions
- [`human_ai_codebook`](https://sonsoles.me/htna/reference/human_ai_codebook.md)
  : Code-to-actor codebook for the vibe-coding data
