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
