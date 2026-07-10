# Changelog

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
