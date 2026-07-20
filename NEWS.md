# htna 0.2.1

## Verification

* Added extensive randomized equivalence coverage for constructing HTNA models
  from Nestimate distance clustering, fitted mixed-Markov clustering, and
  legacy `tna` clustering results, including actor-partition preservation and
  no-refit guarantees.

# htna 0.2.0

## New features

* `build_htna()` now consumes Nestimate distance clustering
  (`net_clustering`), fitted MMM clustering (`cluster_mmm()` /
  `netobject_group`), and legacy `tna::cluster_sequences()` results. It returns
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
