# htna: Heterogeneous Transition Network Analysis

Build, analyze, and visualize transition networks over sequences that
mix two or more actor groups (e.g. Human and AI) within the same
interaction.

## Details

The package is a thin layer on top of two foundations:

- Nestimate - estimation, bootstrap, permutation, reliability,
  centrality, and Markov machinery.

- cograph - rendering (`splot`, `plot_htna`, transitions, communities,
  chord, heatmap).

htna adds the heterogeneous-actor wrapper
[`build_htna`](https://mohsaqr.github.io/htna/reference/build_htna.md)
and re-exports the most commonly used downstream functions so users do
not need to attach Nestimate or cograph explicitly.

## See also

Useful links:

- <https://github.com/mohsaqr/htna>

- Report bugs at <https://github.com/mohsaqr/htna/issues>

## Author

**Maintainer**: Mohammed Saqr <saqr@saqr.me> \[copyright holder\]
