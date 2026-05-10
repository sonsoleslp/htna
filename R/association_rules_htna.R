#' Association Rule Mining over Transitions
#'
#' htna-named alias of [Nestimate::association_rules()]. Mines
#' frequent itemsets and association rules over a transition-count
#' matrix under support / confidence / lift thresholds.
#'
#' Foundation function in the htna-aware exploratory family. Works on
#' transition-count input produced by [frequencies_htna()] or
#' [Nestimate::build_network()]; the rules carry over to htna networks
#' since the underlying transition counts are the same.
#'
#' Suffixed `_htna` to avoid clashing with
#' `Nestimate::association_rules()` when both packages are loaded.
#'
#' @inheritParams Nestimate::association_rules
#'
#' @return A list with the discovered rules and frequent itemsets. See
#'   [Nestimate::association_rules()] for details.
#'
#' @seealso [frequencies_htna()], [mosaic_plot_htna()].
#' @examples
#' \donttest{
#' data(human_long, ai_long, package = "Nestimate")
#' net <- build_htna(list(Human = human_long, AI = ai_long))
#' association_rules_htna(net)
#' }
#' @export
association_rules_htna <- Nestimate::association_rules
