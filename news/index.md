# Changelog

## htna 0.3.6

- [`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md) has
  a `time` argument naming the column with the time of each event. With
  it the events are ordered by time and the `order` column is not
  needed. It makes `method = "latency"` of Nestimate 1.0.20 available, a
  network in which each reply is weighted by its speed.
- [`summary()`](https://rdrr.io/r/base/summary.html) of an undirected
  network (`method = "co_occurrence"`) counts each edge once, in the
  header and in the actor-pair table, which has one row per unordered
  pair of actor types. The counts agree with the network metrics of
  Nestimate 1.0.7 and later and with
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html).
- [`print()`](https://rdrr.io/r/base/print.html) of a
  [`centrality_stability_htna()`](https://sonsoles.me/htna/reference/centrality_stability_htna.md)
  result says when whole actors were dropped (`actor`), and its plot
  labels the axis accordingly.
- [`certainty_htna()`](https://sonsoles.me/htna/reference/certainty_htna.md)
  keeps the actor types on its result, and
  [`plot_htna_bootstrap()`](https://sonsoles.me/htna/reference/plot_htna_bootstrap.md)
  draws it.
- Help pages name the settings recent Nestimate versions added: nested
  mosaics, `target` for case-dropping reliability, `format` for
  association rules, and the estimation methods `"reverse"`, `"ngram"`,
  `"gap"` and `"latency"`.

## htna 0.3.5

This section covers all changes since the CRAN release 0.3.1.

### New features

- [`bootstrap()`](https://sonsoles.me/htna/reference/bootstrap.md) and
  [`plot_sequences()`](https://sonsoles.me/htna/reference/plot_sequences.md)
  reach the htna methods whichever of htna and tna is attached last: the
  methods are also registered on tna’s generics of the same names.
  Through
  [`tna::bootstrap()`](https://sonsoles.me/tna/reference/bootstrap.html),
  tna’s argument names (`level`, `method`, `threshold`) are mapped onto
  those of
  [`bootstrap_htna()`](https://sonsoles.me/htna/reference/bootstrap_htna.md)
  (`ci_level`, `inference`, `edge_threshold`).
- [`centrality_stability_htna()`](https://sonsoles.me/htna/reference/centrality_stability_htna.md)
  passes further arguments to
  [`Nestimate::centrality_stability()`](https://pak.dynasite.org/Nestimate/reference/centrality_stability.html)
  through `...`, as the other wrappers do, so Nestimate’s `actor`
  argument (resample whole actors) reaches it.
- Result objects have tidy accessors.
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) returns
  the edges (with the actor types of source and target) or the nodes of
  an htna network or grouped network; the tables of a
  [`summary()`](https://rdrr.io/r/base/summary.html) (`what = "actors"`,
  `"actor_pairs"`, `"metrics"`); the actor-pair, edge, metric and
  network tables of a
  [`compare_htna()`](https://sonsoles.me/htna/reference/compare_htna.md)
  result; and the CS coefficients or correlation traces of
  [`centrality_stability_htna()`](https://sonsoles.me/htna/reference/centrality_stability_htna.md).
  Grouped results are stacked with a `group` or `comparison` column.
- [`compare_htna()`](https://sonsoles.me/htna/reference/compare_htna.md)
  results have [`print()`](https://rdrr.io/r/base/print.html) and
  [`summary()`](https://rdrr.io/r/base/summary.html) methods; a grouped
  comparison prints one block per cohort pair instead of the raw list.
- Grouped bootstraps
  ([`bootstrap_htna()`](https://sonsoles.me/htna/reference/bootstrap_htna.md)
  on an `htna_group`) have
  [`print()`](https://rdrr.io/r/base/print.html) and
  [`summary()`](https://rdrr.io/r/base/summary.html) methods, and
  grouped stability results a
  [`summary()`](https://rdrr.io/r/base/summary.html).
- [`plot_htna_diff()`](https://sonsoles.me/htna/reference/plot_htna_diff.md)
  draws a grouped
  [`bayes_compare_htna()`](https://sonsoles.me/htna/reference/bayes_compare_htna.md)
  result, one difference network per cohort pair.
- Errors are classed conditions inheriting from `htna_error`
  (`htna_bad_input`, `htna_missing_column`, `htna_actor_conflict`,
  `htna_unassigned_node`, `htna_empty_input`, `htna_missing_package`,
  `htna_unnamed_nodes`), documented in the Conditions section of each
  help page.

### Changes

- [`summary()`](https://rdrr.io/r/base/summary.html) of an htna network
  or grouped network returns a `summary.htna` / `summary.htna_group`
  object that prints the same report; it no longer prints as a side
  effect of the call.
- `build_htna(group = )` returns the cohorts in the order of the levels
  when the grouping column is a factor (for `human_ai`, `Early` then
  `Late`), so grouped comparisons are named `"Early vs Late"`. Character
  grouping columns keep their previous order.
- [`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md)
  detects a code used by two actors also when there are three or more
  actor types and the code is not used by all of them.
- [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) on an
  htna network now returns htna’s edge table (with actor columns)
  instead of cograph’s.
- The centrality stability plot uses the Okabe-Ito palette,
  distinguishes the measures by line type and point shape as well as
  colour, titles each cohort’s chart with the cohort name, and raises a
  classed error when ‘ggplot2’ is not installed.
- Unused suggested packages ‘codyna’ and ‘janitor’ were removed.

### Documentation

- Help pages were rewritten to describe the method each function
  implements, with return structures, conditions and runnable examples
  on the bundled data. Examples no longer reach into result objects.
- New vignette “Grouped heterogeneous transition networks”; the main
  vignette sets out the model and a complete analysis.
- New guides “Comparing networks” and “Sequences and patterns”
  (replacing the sequence plot and sequence comparison articles), and
  two case studies on the vibe-coding sessions. “Input formats” is now a
  guide on the package website.
- The examples of
  [`sequence_plot_htna()`](https://sonsoles.me/htna/reference/sequence_plot_htna.md)
  and
  [`plot_sequences()`](https://sonsoles.me/htna/reference/plot_sequences.md)
  use one project, so they run quickly when the ‘stringdist’ package is
  not available.
- Corrected return-value documentation of
  [`markov_order_test_htna()`](https://sonsoles.me/htna/reference/markov_order_test_htna.md),
  [`sequence_compare_htna()`](https://sonsoles.me/htna/reference/sequence_compare_htna.md),
  [`state_distribution_htna()`](https://sonsoles.me/htna/reference/state_distribution_htna.md),
  [`pruning_details_htna()`](https://sonsoles.me/htna/reference/pruning_details_htna.md)
  and [`plot_htna()`](https://sonsoles.me/htna/reference/plot_htna.md)
  (0.3.2).

### Tests

- The shipped tests no longer compare against ‘Nestimate’ or ‘tna’;
  those comparisons moved to a local equivalence suite. The Edge Count
  test of [`summary()`](https://rdrr.io/r/base/summary.html) follows the
  network’s `directed` flag, for CRAN ‘Nestimate’ and ‘Nestimate’ 1.0.7
  or later (0.3.3, 0.3.4).

## htna 0.3.1

CRAN release: 2026-07-28

### Fitted MMM conversion

- [`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md) now
  explicitly consumes the fitted `net_mmm` object returned by
  [`Nestimate::cluster_mmm()`](https://pak.dynasite.org/Nestimate/reference/cluster_mmm.html).
  It materializes one HTNA network per fitted sequence cluster without
  rerunning MMM and preserves assignments, posterior probabilities,
  mixing proportions, information criteria, fitted weights, and the
  original actor partition.
- [`Nestimate::as_htna()`](https://pak.dynasite.org/Nestimate/reference/as_htna.html)
  now supports the same fitted `net_mmm` input and returns an equivalent
  `htna_group`.
  [`Nestimate::cluster_network()`](https://pak.dynasite.org/Nestimate/reference/cluster_network.html)
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
  [`Nestimate::build_clusters()`](https://pak.dynasite.org/Nestimate/reference/build_clusters.html)
  and
  [`Nestimate::cluster_mmm()`](https://pak.dynasite.org/Nestimate/reference/cluster_mmm.html),
  model selection with `cluster_choice()` / `compare_mmm()` /
  `cluster_diagnostics()`, and cohort-pair comparison with
  [`compare_htna()`](https://sonsoles.me/htna/reference/compare_htna.md).

## htna 0.3.0

### Actor-aware interoperability and comparison

- Added
  [`compare_htna()`](https://sonsoles.me/htna/reference/compare_htna.md),
  an actor-aware wrapper over
  [`Nestimate::compare_model()`](https://pak.dynasite.org/Nestimate/reference/compare_model.html).
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
  [`tna::ctna()`](https://sonsoles.me/tna/reference/build_model.html).

## htna 0.2.2

### MCML expansion

- [`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md) now
  accepts a fitted
  [`Nestimate::build_mcml()`](https://pak.dynasite.org/Nestimate/reference/build_mcml.html)
  object. The MCML node clusters become actor groups, and one full
  node-level HTNA is rebuilt from the retained or explicitly supplied
  original source. This is equivalent to
  [`Nestimate::as_htna()`](https://pak.dynasite.org/Nestimate/reference/as_htna.html):
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
  [`tna::cluster_sequences()`](https://sonsoles.me/tna/reference/cluster_data.html)
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
  [`Nestimate::net_prune()`](https://pak.dynasite.org/Nestimate/reference/net_prune.html)
  and friends). The actor partition and `htna` class are preserved
  across pruning, so the pruned network stays htna-aware and plots with
  \[plot_htna()\].
- [`bayes_compare_htna()`](https://sonsoles.me/htna/reference/bayes_compare_htna.md)
  — Bayesian complement of
  [`permutation_htna()`](https://sonsoles.me/htna/reference/permutation_htna.md)
  (over
  [`Nestimate::bayes_compare()`](https://pak.dynasite.org/Nestimate/reference/bayes_compare.html)).
  Preserves the actor partition on `$x`/`$y` and carries class
  `net_permutation`, so it renders with
  [`plot_htna_diff()`](https://sonsoles.me/htna/reference/plot_htna_diff.md).
- [`certainty_htna()`](https://sonsoles.me/htna/reference/certainty_htna.md)
  — closed-form counterpart of
  [`bootstrap_htna()`](https://sonsoles.me/htna/reference/bootstrap_htna.md)
  (over
  [`Nestimate::certainty()`](https://pak.dynasite.org/Nestimate/reference/certainty.html)),
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
  [`Nestimate::permutation()`](https://pak.dynasite.org/Nestimate/reference/permutation.html)
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
