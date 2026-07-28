# Changelog

## htna 0.3.1

### Fitted MMM conversion

- [`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md) now
  explicitly consumes the fitted `net_mmm` object returned by
  [`Nestimate::cluster_mmm()`](https://saqr.me/Nestimate/reference/cluster_mmm.html).
  It materializes one HTNA network per fitted sequence cluster without
  rerunning MMM and preserves assignments, posterior probabilities,
  mixing proportions, information criteria, fitted weights, and the
  original actor partition.
- [`Nestimate::as_htna()`](https://saqr.me/Nestimate/reference/as_htna.html)
  now supports the same fitted `net_mmm` input and returns an equivalent
  `htna_group`.
  [`Nestimate::cluster_network()`](https://saqr.me/Nestimate/reference/cluster_network.html)
  remains the function that directly returns materialized cluster
  networks.

### Datasets

- New per-actor long frames `human_simplified` and `ai_simplified`,
  ready for the named-list form of
  [`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md).
- New tidy code → actor-type lookup `human_ai_codebook`, ready to pass
  as `node_groups` to
  [`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md).
- `human_ai` gains a `phase` factor column tagging each session as
  `"Early"` or `"Late"` (chronological split), so grouped examples can
  call `build_htna(human_ai, group = "phase")` without pre-processing.

### Documentation

- `htna_palette` is now exported (previously an internal constant
  referenced as a default argument in exported plot functions).
- New article vignette **Clustering inputs to
  [`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md)**
  covering
  [`Nestimate::build_clusters()`](https://saqr.me/Nestimate/reference/build_clusters.html)
  and
  [`Nestimate::cluster_mmm()`](https://saqr.me/Nestimate/reference/cluster_mmm.html),
  model selection with `cluster_choice()` / `compare_mmm()` /
  `cluster_diagnostics()`, and cohort-pair comparison with
  [`compare_htna()`](https://sonsoles.me/htna/reference/compare_htna.md).

## htna 0.3.0

### Actor-aware interoperability and comparison

- Added
  [`compare_htna()`](https://sonsoles.me/htna/reference/compare_htna.md),
  an actor-aware wrapper over
  [`Nestimate::compare_model()`](https://saqr.me/Nestimate/reference/compare_model.html).
  It aligns partial node coverage, annotates every edge with source and
  target actors, reports summaries for each actor-pair block, retains
  the original HTNA networks, and compares every cohort pair when passed
  an `htna_group`.
- Added `as.igraph()` methods for `htna` and `htna_group`. Converted
  graphs preserve actor types as vertex attributes and canonical actor
  ordering as graph metadata; grouped inputs convert every cohort.
- [`summary.htna()`](https://sonsoles.me/htna/reference/summary.htna.md)
  and
  [`summary.htna_group()`](https://sonsoles.me/htna/reference/summary.htna.md)
  now include Nestimate’s standard network-level descriptive metrics
  alongside the existing actor-partition and actor-to-actor edge
  summaries.
- `build_htna(method = "co_occurrence")` is now documented and covered
  across all constructor forms, grouped networks, MCML expansion,
  Nestimate distance clustering, and legacy TNA clustering, with
  numerical equivalence tests against both Nestimate and
  [`tna::ctna()`](http://sonsoles.me/tna/reference/build_model.md).

## htna 0.2.2

### MCML expansion

- [`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md) now
  accepts a fitted
  [`Nestimate::build_mcml()`](https://saqr.me/Nestimate/reference/build_mcml.html)
  object. The MCML node clusters become actor groups, and one full
  node-level HTNA is rebuilt from the retained or explicitly supplied
  original source. This is equivalent to
  [`Nestimate::as_htna()`](https://saqr.me/Nestimate/reference/as_htna.html):
  it intentionally preserves global cross-cluster transitions rather
  than stitching together fitted macro or within-cluster weights.
- HTNA partition injection now consistently stores character labels in
  `$node_groups$group` and a factor with the full actor level set in
  `$nodes$groups`.

## htna 0.2.1

### Verification

- Added extensive randomized equivalence coverage for constructing HTNA
  models from Nestimate distance clustering, fitted mixed-Markov
  clustering, and legacy `tna` clustering results, including
  actor-partition preservation and no-refit guarantees.

## htna 0.2.0

### New features

- [`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md) now
  consumes Nestimate distance clustering (`net_clustering`), fitted MMM
  clustering (`cluster_mmm()` / `net_mmm`), materialized cluster
  networks (`netobject_group`), and legacy
  [`tna::cluster_sequences()`](http://sonsoles.me/tna/reference/cluster_data.md)
  results. It returns an `htna_group`, restores the actor partition on
  every child, and preserves clustering assignments, posterior
  probabilities, diagnostics, and fitted MMM weights. A partition
  preserved upstream is used automatically; older or
  independently-created clustering results can supply `node_groups`.

Six htna-named functions are added, extending htna’s coverage of the
major transition-network-analysis (tna) verbs. All are thin
`...`-forwarding wrappers over the ‘Nestimate’ engine and follow the
same durable design as the existing re-exports (stable documented
formals; new engine arguments flow through `...`).

- Pruning family —
  [`prune_htna()`](https://sonsoles.me/htna/reference/prune_htna.md),
  [`deprune_htna()`](https://sonsoles.me/htna/reference/deprune_htna.md),
  [`reprune_htna()`](https://sonsoles.me/htna/reference/reprune_htna.md),
  and
  [`pruning_details_htna()`](https://sonsoles.me/htna/reference/pruning_details_htna.md)
  (over
  [`Nestimate::net_prune()`](https://saqr.me/Nestimate/reference/net_prune.html)
  and friends). The actor partition and `htna` class are preserved
  across pruning, so the pruned network stays htna-aware and plots with
  \[plot_htna()\].
- [`bayes_compare_htna()`](https://sonsoles.me/htna/reference/bayes_compare_htna.md)
  — Bayesian complement of
  [`permutation_htna()`](https://sonsoles.me/htna/reference/permutation_htna.md)
  (over
  [`Nestimate::bayes_compare()`](https://saqr.me/Nestimate/reference/bayes_compare.html)).
  Preserves the actor partition on `$x`/`$y` and carries class
  `net_permutation`, so it renders with
  [`plot_htna_diff()`](https://sonsoles.me/htna/reference/plot_htna_diff.md).
- [`certainty_htna()`](https://sonsoles.me/htna/reference/certainty_htna.md)
  — closed-form counterpart of
  [`bootstrap_htna()`](https://sonsoles.me/htna/reference/bootstrap_htna.md)
  (over
  [`Nestimate::certainty()`](https://saqr.me/Nestimate/reference/certainty.html)),
  joining the confirmatory-analysis family alongside
  [`reliability_htna()`](https://sonsoles.me/htna/reference/reliability_htna.md)
  and
  [`casedrop_reliability_htna()`](https://sonsoles.me/htna/reference/casedrop_reliability_htna.md).

## htna 0.1.2

- The six re-exported Nestimate functions
  ([`permutation_htna()`](https://sonsoles.me/htna/reference/permutation_htna.md),
  [`association_rules_htna()`](https://sonsoles.me/htna/reference/association_rules_htna.md),
  [`casedrop_reliability_htna()`](https://sonsoles.me/htna/reference/casedrop_reliability_htna.md),
  [`markov_order_test_htna()`](https://sonsoles.me/htna/reference/markov_order_test_htna.md),
  [`state_distribution_htna()`](https://sonsoles.me/htna/reference/state_distribution_htna.md),
  [`state_frequencies_htna()`](https://sonsoles.me/htna/reference/state_frequencies_htna.md))
  are now thin `...`-forwarding wrappers instead of direct aliases.
  Their documented formals are fixed, so a future change to any of these
  functions’ signatures in Nestimate can no longer produce a
  code/documentation-mismatch (codoc) `WARNING` in `R CMD check`. No
  behavioural change: each call forwards unchanged to Nestimate
  (including S3 dispatch for
  [`state_distribution_htna()`](https://sonsoles.me/htna/reference/state_distribution_htna.md)),
  and extra arguments pass straight through `...`.
- [`sequence_compare_htna()`](https://sonsoles.me/htna/reference/sequence_compare_htna.md)
  no longer restates Nestimate’s `sequence_compare()` arguments (`sub`,
  `min_freq`, `test`, `iter`, `adjust`) as its own formals; they now
  pass through `...`. Its signature is `x, group, level, ...` — the
  htna-specific `level` argument is retained. Same defaults and
  behaviour, but new Nestimate arguments flow through automatically and
  the code/documentation coupling is removed.
  [`mosaic_plot_htna()`](https://sonsoles.me/htna/reference/mosaic_plot_htna.md)’s
  documentation is likewise decoupled from Nestimate’s signature (it was
  already a `...` wrapper).

## htna 0.1.1

- Compatibility with Nestimate 0.8.0.
  [`Nestimate::permutation()`](https://saqr.me/Nestimate/reference/permutation.html)
  gained a `measures` argument (for centrality permutation tests); since
  [`permutation_htna()`](https://sonsoles.me/htna/reference/permutation_htna.md)
  re-exports that function, its documentation is regenerated to include
  the new argument, resolving a code/documentation mismatch flagged by
  `R CMD check`. No behavioural change.
- `Imports: Nestimate (>= 0.8.0)`.

## htna 0.1.0

CRAN release: 2026-07-06

- Initial release.
- Builds heterogeneous transition networks for sequences mixing two or
  more actor groups (e.g. Human and AI) within the same interaction, via
  [`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md).
- Re-exports the most commonly used `Nestimate` and `cograph` downstream
  functions under `_htna`-suffixed aliases so callers do not need to
  attach `Nestimate` or `cograph` explicitly:
  [`permutation_htna()`](https://sonsoles.me/htna/reference/permutation_htna.md),
  [`casedrop_reliability_htna()`](https://sonsoles.me/htna/reference/casedrop_reliability_htna.md),
  [`markov_order_test_htna()`](https://sonsoles.me/htna/reference/markov_order_test_htna.md),
  [`mosaic_plot_htna()`](https://sonsoles.me/htna/reference/mosaic_plot_htna.md),
  [`state_distribution_htna()`](https://sonsoles.me/htna/reference/state_distribution_htna.md),
  [`state_frequencies_htna()`](https://sonsoles.me/htna/reference/state_frequencies_htna.md),
  [`association_rules_htna()`](https://sonsoles.me/htna/reference/association_rules_htna.md),
  [`sequence_compare_htna()`](https://sonsoles.me/htna/reference/sequence_compare_htna.md).
- htna-side wrappers add ergonomics over the upstream API:
  [`frequencies_htna()`](https://sonsoles.me/htna/reference/frequencies_htna.md),
  [`plot_frequencies_htna()`](https://sonsoles.me/htna/reference/plot_frequencies_htna.md)
  (treemap / bars / facet views),
  [`centralities_htna()`](https://sonsoles.me/htna/reference/centralities_htna.md),
  [`plot_centralities()`](https://sonsoles.me/htna/reference/plot_centralities.md),
  [`plot_htna()`](https://sonsoles.me/htna/reference/plot_htna.md),
  [`plot_htna_bootstrap()`](https://sonsoles.me/htna/reference/plot_htna_bootstrap.md),
  [`plot_htna_diff()`](https://sonsoles.me/htna/reference/plot_htna_diff.md),
  [`sequence_plot_htna()`](https://sonsoles.me/htna/reference/sequence_plot_htna.md),
  [`reliability_htna()`](https://sonsoles.me/htna/reference/reliability_htna.md),
  [`centrality_stability_htna()`](https://sonsoles.me/htna/reference/centrality_stability_htna.md),
  [`edge_betweenness_htna()`](https://sonsoles.me/htna/reference/edge_betweenness_htna.md),
  [`extract_meta_paths()`](https://sonsoles.me/htna/reference/extract_meta_paths.md)
  (state-level by default; `level = "type"` for the type-level meta-path
  rollup; supports schemas mixing types, concrete codes, and `*`).
