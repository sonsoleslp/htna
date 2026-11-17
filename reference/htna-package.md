# htna: Heterogeneous Transition Network Analysis

`htna` estimates, describes, plots and tests heterogeneous transition
networks of sequences of events produced by two or more types of actor,
such as a learner and an AI assistant, a student and a teacher, or a
clinician and a patient. Heterogeneous transition network analysis
(HTNA) models such sequences as one transition network in which each
node is a code belonging to one actor type and each edge is the
transition from one code to the next (Lopez-Pernas, Misiejuk and Saqr,
2026). Keeping the actor partition on the network separates the dynamics
within each actor from the hand-overs between actors, so the estimation,
inference and visualisation tools of transition network analysis can
show what the AI does after the learner asks for help, and how the
learner responds.

`htna` builds the network from long event data, describes it by actor
type, draws it with each actor type in its own colour, shape and arc of
the layout, and keeps the partition on the results of every downstream
analysis.

## Building and describing

[`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md)
estimates the network (or one network per cohort or cluster);
[`summary.htna()`](https://sonsoles.me/htna/reference/summary.htna.md),
[`as.data.frame.htna()`](https://sonsoles.me/htna/reference/as.data.frame.htna.md)
and
[`as.igraph.htna()`](https://sonsoles.me/htna/reference/as.igraph.htna.md)
describe and convert it.
[`prune_htna()`](https://sonsoles.me/htna/reference/prune_htna.md),
[`deprune_htna()`](https://sonsoles.me/htna/reference/deprune_htna.md),
[`reprune_htna()`](https://sonsoles.me/htna/reference/reprune_htna.md)
and
[`pruning_details_htna()`](https://sonsoles.me/htna/reference/pruning_details_htna.md)
remove and restore weak edges.

## Plots

[`plot_htna()`](https://sonsoles.me/htna/reference/plot_htna.md) draws
the network,
[`plot_htna_diff()`](https://sonsoles.me/htna/reference/plot_htna_diff.md)
the difference between two networks,
[`plot_htna_bootstrap()`](https://sonsoles.me/htna/reference/plot_htna_bootstrap.md)
the bootstrap-significant edges,
[`sequence_plot_htna()`](https://sonsoles.me/htna/reference/sequence_plot_htna.md)
the sessions,
[`plot_frequencies_htna()`](https://sonsoles.me/htna/reference/plot_frequencies_htna.md)
and
[`mosaic_plot_htna()`](https://sonsoles.me/htna/reference/mosaic_plot_htna.md)
the code frequencies,
[`plot_centralities()`](https://sonsoles.me/htna/reference/plot_centralities.md)
and
[`plot_edge_betweenness_htna()`](https://sonsoles.me/htna/reference/plot_edge_betweenness_htna.md)
node and edge importance.

## Comparing networks

[`compare_htna()`](https://sonsoles.me/htna/reference/compare_htna.md)
describes the differences between networks by actor-pair block;
[`permutation_htna()`](https://sonsoles.me/htna/reference/permutation_htna.md)
and
[`bayes_compare_htna()`](https://sonsoles.me/htna/reference/bayes_compare_htna.md)
test them.

## Model assessment

[`bootstrap_htna()`](https://sonsoles.me/htna/reference/bootstrap_htna.md),
[`certainty_htna()`](https://sonsoles.me/htna/reference/certainty_htna.md),
[`reliability_htna()`](https://sonsoles.me/htna/reference/reliability_htna.md),
[`casedrop_reliability_htna()`](https://sonsoles.me/htna/reference/casedrop_reliability_htna.md),
[`centrality_stability_htna()`](https://sonsoles.me/htna/reference/centrality_stability_htna.md)
and
[`markov_order_test_htna()`](https://sonsoles.me/htna/reference/markov_order_test_htna.md)
assess the stability of the edges and centralities and the adequacy of a
first-order model.

## Patterns and frequencies

[`extract_meta_paths()`](https://sonsoles.me/htna/reference/extract_meta_paths.md)
finds recurring paths at the level of codes or actor types,
[`association_rules_htna()`](https://sonsoles.me/htna/reference/association_rules_htna.md)
association rules,
[`sequence_compare_htna()`](https://sonsoles.me/htna/reference/sequence_compare_htna.md)
patterns that differ between cohorts, and
[`frequencies_htna()`](https://sonsoles.me/htna/reference/frequencies_htna.md),
[`state_frequencies_htna()`](https://sonsoles.me/htna/reference/state_frequencies_htna.md)
and
[`state_distribution_htna()`](https://sonsoles.me/htna/reference/state_distribution_htna.md)
the frequencies of the codes.

## References

Lopez-Pernas, S., Misiejuk, K., & Saqr, M. (2026). How AI-generated
feedback hinders or helps learning: A heterogeneous TNA study of
learning dynamics. *Journal of Computer Assisted Learning*, 42(4),
e70285. [doi:10.1002/jcal.70285](https://doi.org/10.1002/jcal.70285)

## See also

The bundled data
[human_ai](https://sonsoles.me/htna/reference/human_ai.md); the 'tna'
package for transition network analysis of sequences with a single actor
type.

## Author

**Maintainer**: Sonsoles López-Pernas <sonsoles.lopez@uef.fi>
([ORCID](https://orcid.org/0000-0002-9621-1392)) \[copyright holder\]

Authors:

- Sonsoles López-Pernas <sonsoles.lopez@uef.fi>
  ([ORCID](https://orcid.org/0000-0002-9621-1392)) \[copyright holder\]

- Kamila Misiejuk \[copyright holder\]

- Mohammed Saqr <saqr@saqr.me>
  ([ORCID](https://orcid.org/0000-0001-5881-3109)) \[copyright holder\]
