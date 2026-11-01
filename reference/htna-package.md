# htna: Heterogeneous Transition Network Analysis

Build, analyze, and visualize transition networks over sequences that
mix two or more actor groups (e.g. Human and AI) within the same
interaction.

## Details

The package is a thin layer on top of two foundations:

- Nestimate - estimation, bootstrap, permutation, reliability,
  centrality, and Markov machinery.

- cograph - rendering. Every network plot drawn by htna is rendered by
  [`cograph::plot_htna()`](https://sonsoles.me/cograph/reference/plot_htna.html)
  or
  [`cograph::splot()`](https://sonsoles.me/cograph/reference/splot.html);
  the htna plotting functions only supply the actor-aware layout,
  colours and legend.

htna adds the heterogeneous-actor constructor
[`build_htna`](https://sonsoles.me/htna/reference/build_htna.md) and
`_htna`-named wrappers around the most commonly used downstream
functions, so users do not need to attach Nestimate or cograph
explicitly. Analysis wrappers restore the actor partition on their
results, so the output stays htna-aware.

## See also

Useful links:

- <https://sonsoles.me/htna/>

- Report bugs at <https://github.com/sonsoleslp/htna/issues>

## Author

**Maintainer**: Sonsoles López-Pernas <sonsoles.lopez@uef.fi>
\[copyright holder\]

Authors:

- Kamila Misiejuk \[copyright holder\]

- Mohammed Saqr <saqr@saqr.me> \[copyright holder\]
