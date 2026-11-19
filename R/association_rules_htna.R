#' Mine association rules from the sessions of a network
#'
#' @description
#' `association_rules_htna()` mines association rules from the sessions of
#' an htna network: rules that describe which codes tend to occur together
#' in the same session, regardless of their order. A rule "A, B => C" states
#' that sessions containing codes A and B also tend to contain code C.
#' Support is the share of sessions containing all codes of the rule,
#' confidence the share of sessions with the antecedent that also contain
#' the consequent, and lift the ratio of the confidence to the overall share
#' of the consequent. In a heterogeneous network a rule can join codes of
#' different actors, for example a human request and the AI actions that
#' accompany it.
#'
#' `association_rules_htna()` runs Nestimate's `association_rules()` on the
#' network: it treats every session as one transaction (the set of codes it
#' contains), finds the frequent itemsets with the Apriori algorithm, and
#' keeps the rules that pass the support, confidence and lift thresholds. It
#' returns a `net_association_rules` object whose rules table has one row
#' per rule.
#'
#' The rules ignore order. For ordered patterns use [extract_meta_paths()];
#' for associations between codes and sessions use [mosaic_plot_htna()].
#'
#' @param x An htna network from [build_htna()], or a binary
#'   session-by-code matrix (declared with `format = "binary"` from
#'   Nestimate 1.0.9). A grouped network is not accepted; pass one cohort.
#' @param ... Thresholds: `min_support` (default 0.1), `min_confidence`
#'   (0.5), `min_lift` (1), `max_length` (largest itemset, 5) and `format`.
#'
#' @return An object of class `net_association_rules` with `print()`,
#'   `summary()` and `plot()` methods. Its rules table has one row per
#'   rule, with columns `antecedent`, `consequent`, `support`,
#'   `confidence`, `lift`, `conviction`, `count` and `n_transactions`.
#'
#' @seealso [extract_meta_paths()] for ordered patterns,
#'   [frequencies_htna()], [mosaic_plot_htna()].
#' @examples
#' data(human_ai)
#' net <- build_htna(human_ai, actor_type = "actor_type")
#' association_rules_htna(net, max_length = 3L)
#' @export
association_rules_htna <- function(x, ...) {
  Nestimate::association_rules(x, ...)
}
