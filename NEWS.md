# htna 0.3.7

* The start and end states added with `build_htna(start = , end = )` have an
  actor type of their own, `"Boundary"`, so such a network can be plotted,
  summarized, compared and converted like any other. In the plots the
  actor types keep the positions they have without these states; the start
  state is drawn at the top centre and the end state at the bottom centre.
* The legend of `plot_htna_bootstrap()` and `plot_htna_diff()` showed the
  colours of the actor types in reverse order; it now matches the nodes.
* A cohort built with `node_groups` and `group` that lacks a code is
  consistent throughout (node and edge counts, edge list), and
  `permutation_htna()` and `bayes_compare_htna()` compare cohorts that do
  not hold the same codes.
* `frequencies_htna()` accepts a grouped network and returns one table for
  all cohorts, with columns `group` (the cohort) and `actor`.
* `sequence_compare_htna()` takes the name of a session-level column in
  `group`, for example `group = "phase"`, as well as a vector.
* `build_htna(method = "latency")` stops with a clear error of class
  `htna_tied_times` when the event times are tied within sessions, as in
  the bundled `human_ai`, which records one time per session.
* Sequence plots leave out an actor type that has no event in the sessions.

# htna 0.3.6

* `build_htna()` has a `time` argument naming the column with the time of
  each event. With it the events are ordered by time and the `order` column
  is not needed. It makes `method = "latency"` of Nestimate 1.0.20
  available, a network in which each reply is weighted by its speed.
* `summary()` of an undirected network (`method = "co_occurrence"`) counts
  each edge once, in the header and in the actor-pair table, which has one
  row per unordered pair of actor types. The counts agree with the network
  metrics of Nestimate 1.0.7 and later and with `as.data.frame()`.
* `print()` of a `centrality_stability_htna()` result says when whole actors
  were dropped (`actor`), and its plot labels the axis accordingly.
* `certainty_htna()` keeps the actor types on its result, and
  `plot_htna_bootstrap()` draws it.
* Help pages name the settings recent Nestimate versions added: nested
  mosaics, `target` for case-dropping reliability, `format` for association
  rules, and the estimation methods `"reverse"`, `"ngram"`, `"gap"` and
  `"latency"`.

# htna 0.3.5

This section covers all changes since the CRAN release 0.3.1.

## New features

* `bootstrap()` and `plot_sequences()` reach the htna methods whichever of
  htna and tna is attached last: the methods are also registered on tna's
  generics of the same names. Through `tna::bootstrap()`, tna's argument
  names (`level`, `method`, `threshold`) are mapped onto those of
  `bootstrap_htna()` (`ci_level`, `inference`, `edge_threshold`).
* `centrality_stability_htna()` passes further arguments to
  `Nestimate::centrality_stability()` through `...`, as the other wrappers
  do, so Nestimate's `actor` argument (resample whole actors) reaches it.
* Result objects have tidy accessors. `as.data.frame()` returns the edges
  (with the actor types of source and target) or the nodes of an htna
  network or grouped network; the tables of a `summary()`
  (`what = "actors"`, `"actor_pairs"`, `"metrics"`); the actor-pair, edge,
  metric and network tables of a `compare_htna()` result; and the CS
  coefficients or correlation traces of `centrality_stability_htna()`.
  Grouped results are stacked with a `group` or `comparison` column.
* `compare_htna()` results have `print()` and `summary()` methods; a grouped
  comparison prints one block per cohort pair instead of the raw list.
* Grouped bootstraps (`bootstrap_htna()` on an `htna_group`) have `print()`
  and `summary()` methods, and grouped stability results a `summary()`.
* `plot_htna_diff()` draws a grouped `bayes_compare_htna()` result, one
  difference network per cohort pair.
* Errors are classed conditions inheriting from `htna_error`
  (`htna_bad_input`, `htna_missing_column`, `htna_actor_conflict`,
  `htna_unassigned_node`, `htna_empty_input`, `htna_missing_package`,
  `htna_unnamed_nodes`), documented in the Conditions section of each help
  page.

## Changes

* `summary()` of an htna network or grouped network returns a
  `summary.htna` / `summary.htna_group` object that prints the same report;
  it no longer prints as a side effect of the call.
* `build_htna(group = )` returns the cohorts in the order of the levels when
  the grouping column is a factor (for `human_ai`, `Early` then `Late`), so
  grouped comparisons are named `"Early vs Late"`. Character grouping
  columns keep their previous order.
* `build_htna()` detects a code used by two actors also when there are three
  or more actor types and the code is not used by all of them.
* `as.data.frame()` on an htna network now returns htna's edge table (with
  actor columns) instead of cograph's.
* The centrality stability plot uses the Okabe-Ito palette, distinguishes
  the measures by line type and point shape as well as colour, titles each
  cohort's chart with the cohort name, and raises a classed error when
  'ggplot2' is not installed.
* Unused suggested packages 'codyna' and 'janitor' were removed.

## Documentation

* Help pages were rewritten to describe the method each function implements,
  with return structures, conditions and runnable examples on the bundled
  data. Examples no longer reach into result objects.
* New vignette "Grouped heterogeneous transition networks"; the main
  vignette sets out the model and a complete analysis.
* New guides "Comparing networks" and "Sequences and patterns" (replacing
  the sequence plot and sequence comparison articles), and two case studies
  on the vibe-coding sessions. "Input formats" is now a guide on the
  package website.
* The examples of `sequence_plot_htna()` and `plot_sequences()` use one
  project, so they run quickly when the 'stringdist' package is not
  available.
* Corrected return-value documentation of `markov_order_test_htna()`,
  `sequence_compare_htna()`, `state_distribution_htna()`,
  `pruning_details_htna()` and `plot_htna()` (0.3.2).

## Tests

* The shipped tests no longer compare against 'Nestimate' or 'tna'; those
  comparisons moved to a local equivalence suite. The Edge Count test of
  `summary()` follows the network's `directed` flag, for CRAN 'Nestimate'
  and 'Nestimate' 1.0.7 or later (0.3.3, 0.3.4).

# htna 0.3.1

## Fitted MMM conversion

* `build_htna()` now explicitly consumes the fitted `net_mmm` object returned
  by `Nestimate::cluster_mmm()`. It materializes one HTNA network per fitted
  sequence cluster without rerunning MMM and preserves assignments, posterior
  probabilities, mixing proportions, information criteria, fitted weights,
  and the original actor partition.
* `Nestimate::as_htna()` now supports the same fitted `net_mmm` input and
  returns an equivalent `htna_group`. `Nestimate::cluster_network()` remains
  the function that directly returns materialized cluster networks.

## Datasets

* New per-actor long frames `human_simplified` and `ai_simplified`, ready
  for the named-list form of `build_htna()`.
* New tidy code → actor-type lookup `human_ai_codebook`, ready to pass as
  `node_groups` to `build_htna()`.
* `human_ai` gains a `phase` factor column tagging each session as
  `"Early"` or `"Late"` (chronological split), so grouped examples can call
  `build_htna(human_ai, group = "phase")` without pre-processing.

## Documentation

* `htna_palette` is now exported (previously an internal constant referenced
  as a default argument in exported plot functions).
* New article vignette **Clustering inputs to `build_htna()`** covering
  `Nestimate::build_clusters()` and `Nestimate::cluster_mmm()`, model
  selection with `cluster_choice()` / `compare_mmm()` /
  `cluster_diagnostics()`, and cohort-pair comparison with `compare_htna()`.

# htna 0.3.0

## Actor-aware interoperability and comparison

* Added `compare_htna()`, an actor-aware wrapper over
  `Nestimate::compare_model()`. It aligns partial node coverage, annotates every
  edge with source and target actors, reports summaries for each actor-pair
  block, retains the original HTNA networks, and compares every cohort pair
  when passed an `htna_group`.
* Added `as.igraph()` methods for `htna` and `htna_group`. Converted graphs
  preserve actor types as vertex attributes and canonical actor ordering as
  graph metadata; grouped inputs convert every cohort.
* `summary.htna()` and `summary.htna_group()` now include Nestimate's standard
  network-level descriptive metrics alongside the existing actor-partition and
  actor-to-actor edge summaries.
* `build_htna(method = "co_occurrence")` is now documented and covered across
  all constructor forms, grouped networks, MCML expansion, Nestimate distance
  clustering, and legacy TNA clustering, with numerical equivalence tests
  against both Nestimate and `tna::ctna()`.

# htna 0.2.2

## MCML expansion

* `build_htna()` now accepts a fitted `Nestimate::build_mcml()` object. The
  MCML node clusters become actor groups, and one full node-level HTNA is
  rebuilt from the retained or explicitly supplied original source. This is
  equivalent to `Nestimate::as_htna()`: it intentionally preserves global
  cross-cluster transitions rather than stitching together fitted macro or
  within-cluster weights.
* HTNA partition injection now consistently stores character labels in
  `$node_groups$group` and a factor with the full actor level set in
  `$nodes$groups`.

# htna 0.2.1

## Verification

* Added extensive randomized equivalence coverage for constructing HTNA models
  from Nestimate distance clustering, fitted mixed-Markov clustering, and
  legacy `tna` clustering results, including actor-partition preservation and
  no-refit guarantees.

# htna 0.2.0

## New features

* `build_htna()` now consumes Nestimate distance clustering
  (`net_clustering`), fitted MMM clustering (`cluster_mmm()` / `net_mmm`),
  materialized cluster networks (`netobject_group`), and legacy
  `tna::cluster_sequences()` results. It returns
  an `htna_group`, restores the actor partition on every child, and preserves
  clustering assignments, posterior probabilities, diagnostics, and fitted
  MMM weights. A partition preserved upstream is used automatically; older or
  independently-created clustering results can supply `node_groups`.

Six htna-named functions are added, extending htna's coverage of the major
transition-network-analysis (tna) verbs. All are thin `...`-forwarding wrappers
over the 'Nestimate' engine and follow the same durable design as the existing
re-exports (stable documented formals; new engine arguments flow through `...`).

* Pruning family — `prune_htna()`, `deprune_htna()`, `reprune_htna()`, and
  `pruning_details_htna()` (over `Nestimate::net_prune()` and friends). The
  actor partition and `htna` class are preserved across pruning, so the pruned
  network stays htna-aware and plots with [plot_htna()].
* `bayes_compare_htna()` — Bayesian complement of `permutation_htna()` (over
  `Nestimate::bayes_compare()`). Preserves the actor partition on `$x`/`$y` and
  carries class `net_permutation`, so it renders with `plot_htna_diff()`.
* `certainty_htna()` — closed-form counterpart of `bootstrap_htna()` (over
  `Nestimate::certainty()`), joining the confirmatory-analysis family alongside
  `reliability_htna()` and `casedrop_reliability_htna()`.

# htna 0.1.2

* The six re-exported Nestimate functions (`permutation_htna()`,
  `association_rules_htna()`, `casedrop_reliability_htna()`,
  `markov_order_test_htna()`, `state_distribution_htna()`,
  `state_frequencies_htna()`) are now thin `...`-forwarding wrappers
  instead of direct aliases. Their documented formals are fixed, so a
  future change to any of these functions' signatures in Nestimate can
  no longer produce a code/documentation-mismatch (codoc) `WARNING` in
  `R CMD check`. No behavioural change: each call forwards unchanged to
  Nestimate (including S3 dispatch for `state_distribution_htna()`), and
  extra arguments pass straight through `...`.
* `sequence_compare_htna()` no longer restates Nestimate's `sequence_compare()`
  arguments (`sub`, `min_freq`, `test`, `iter`, `adjust`) as its own formals;
  they now pass through `...`. Its signature is `x, group, level, ...` — the
  htna-specific `level` argument is retained. Same defaults and behaviour, but
  new Nestimate arguments flow through automatically and the code/documentation
  coupling is removed. `mosaic_plot_htna()`'s documentation is likewise
  decoupled from Nestimate's signature (it was already a `...` wrapper).

# htna 0.1.1

* Compatibility with Nestimate 0.8.0. `Nestimate::permutation()` gained a
  `measures` argument (for centrality permutation tests); since
  `permutation_htna()` re-exports that function, its documentation is
  regenerated to include the new argument, resolving a code/documentation
  mismatch flagged by `R CMD check`. No behavioural change.
* `Imports: Nestimate (>= 0.8.0)`.

# htna 0.1.0

* Initial release.
* Builds heterogeneous transition networks for sequences mixing two or
  more actor groups (e.g. Human and AI) within the same interaction,
  via `build_htna()`.
* Re-exports the most commonly used `Nestimate` and `cograph`
  downstream functions under `_htna`-suffixed aliases so callers do
  not need to attach `Nestimate` or `cograph` explicitly:
  `permutation_htna()`, `casedrop_reliability_htna()`,
  `markov_order_test_htna()`, `mosaic_plot_htna()`,
  `state_distribution_htna()`, `state_frequencies_htna()`,
  `association_rules_htna()`, `sequence_compare_htna()`.
* htna-side wrappers add ergonomics over the upstream API:
  `frequencies_htna()`, `plot_frequencies_htna()` (treemap / bars /
  facet views), `centralities_htna()`, `plot_centralities()`,
  `plot_htna()`, `plot_htna_bootstrap()`, `plot_htna_diff()`,
  `sequence_plot_htna()`, `reliability_htna()`,
  `centrality_stability_htna()`, `edge_betweenness_htna()`,
  `extract_meta_paths()` (state-level by default; `level = "type"` for
  the type-level meta-path rollup; supports schemas mixing types,
  concrete codes, and `*`).
