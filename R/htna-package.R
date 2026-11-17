#' htna: Heterogeneous Transition Network Analysis
#'
#' @description
#' `htna` estimates, describes, plots and tests heterogeneous transition
#' networks of sequences of events produced by two or more types of actor,
#' such as a learner and an AI assistant, a student and a teacher, or a
#' clinician and a patient. Heterogeneous transition network analysis (HTNA)
#' models such sequences as one transition network in which each node is a
#' code belonging to one actor type and each edge is the transition from one
#' code to the next (Lopez-Pernas, Misiejuk and Saqr, 2026). Keeping the actor
#' partition on the network separates the dynamics within each actor from the
#' hand-overs between actors, so the estimation, inference and visualisation
#' tools of transition network analysis can show what the AI does after the
#' learner asks for help, and how the learner responds.
#'
#' `htna` builds the network from long event data, describes it by actor
#' type, draws it with each actor type in its own colour, shape and arc of
#' the layout, and keeps the partition on the results of every downstream
#' analysis.
#'
#' @section Building and describing:
#' [build_htna()] estimates the network (or one network per cohort or
#' cluster); [summary.htna()], [as.data.frame.htna()] and
#' [as.igraph.htna()] describe and convert it. [prune_htna()],
#' [deprune_htna()], [reprune_htna()] and [pruning_details_htna()] remove and
#' restore weak edges.
#'
#' @section Plots:
#' [plot_htna()] draws the network, [plot_htna_diff()] the difference between
#' two networks, [plot_htna_bootstrap()] the bootstrap-significant edges,
#' [sequence_plot_htna()] the sessions, [plot_frequencies_htna()] and
#' [mosaic_plot_htna()] the code frequencies, [plot_centralities()] and
#' [plot_edge_betweenness_htna()] node and edge importance.
#'
#' @section Comparing networks:
#' [compare_htna()] describes the differences between networks by actor-pair
#' block; [permutation_htna()] and [bayes_compare_htna()] test them.
#'
#' @section Model assessment:
#' [bootstrap_htna()], [certainty_htna()], [reliability_htna()],
#' [casedrop_reliability_htna()], [centrality_stability_htna()] and
#' [markov_order_test_htna()] assess the stability of the edges and
#' centralities and the adequacy of a first-order model.
#'
#' @section Patterns and frequencies:
#' [extract_meta_paths()] finds recurring paths at the level of codes or actor
#' types, [association_rules_htna()] association rules,
#' [sequence_compare_htna()] patterns that differ between cohorts, and
#' [frequencies_htna()], [state_frequencies_htna()] and
#' [state_distribution_htna()] the frequencies of the codes.
#'
#' @references
#' Lopez-Pernas, S., Misiejuk, K., & Saqr, M. (2026). How AI-generated
#' feedback hinders or helps learning: A heterogeneous TNA study of learning
#' dynamics. *Journal of Computer Assisted Learning*, 42(4), e70285.
#' \doi{10.1002/jcal.70285}
#'
#' @seealso The bundled data [human_ai]; the 'tna' package for transition
#'   network analysis of sequences with a single actor type.
#'
#' @importFrom stats setNames
"_PACKAGE"

# `.data` is the 'ggplot2' tidy-evaluation pronoun used by the ggplot2 charts
# (plot_centralities(), plot_frequencies_htna(), plot.htna_stability()).
utils::globalVariables(".data")
