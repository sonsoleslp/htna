#' Assess the case-dropping stability of the edge weights
#'
#' @description
#' `casedrop_reliability_htna()` measures how far the estimated edge weights
#' of an htna network depend on a few sessions, by case-dropping stability
#' (Epskamp et al., 2018). Networks are re-estimated after dropping
#' increasing proportions of the sessions, and the edge weights of each
#' reduced network are correlated with those of the full network. The
#' case-dropping stability (CS) coefficient is the largest proportion of
#' sessions that can be dropped while the correlation stays above a
#' threshold (0.7 by default) in a required share (95 percent) of the
#' resamples; Epskamp et al. (2018) recommend a coefficient of at least 0.25
#' and preferably above 0.5.
#'
#' `casedrop_reliability_htna()` applies the procedure to the edge-weight
#' vector of an htna network, or to every cohort of a grouped network, and
#' also reports the mean and maximum absolute weight differences at each
#' drop proportion. It runs Nestimate's `casedrop_reliability()` on the
#' network and returns a `net_casedrop_reliability` object (a
#' `net_casedrop_reliability_group` for a grouped network).
#'
#' For the stability of node centralities use
#' [centrality_stability_htna()]; for split-half agreement use
#' [reliability_htna()]; for edge-level confidence intervals use
#' [bootstrap_htna()].
#'
#' @param x An htna network from [build_htna()] or an `htna_group`.
#' @param ... Settings: `iter` (resamples per drop proportion, default
#'   1000), `drop_prop` (default `seq(0.1, 0.9, by = 0.1)`), `threshold`
#'   (0.7), `certainty` (0.95), `method` (correlation, `"spearman"`),
#'   `include_diag` (`FALSE`), `seed`, `target` (the edges assessed:
#'   `"auto"`, `"existing"` or `"all"`; Nestimate 1.0.2 or later) and
#'   `actor` (drop whole actors instead of sessions; Nestimate 1.0.19 or
#'   later).
#'
#' @return An object of class `net_casedrop_reliability` (one network) or
#'   `net_casedrop_reliability_group` (a grouped network), with `print()`,
#'   `summary()` and `plot()` methods; `summary()` returns a data frame
#'   with one row per metric and drop proportion (columns `metric`,
#'   `drop_prop`, `mean`, `sd`, `median`, `mad`, `q025`, `q975`).
#'
#' @references
#' Epskamp, S., Borsboom, D., & Fried, E. I. (2018). Estimating
#' psychological networks and their accuracy: A tutorial paper.
#' *Behavior Research Methods*, 50(1), 195-212.
#' \doi{10.3758/s13428-017-0862-1}
#'
#' @seealso [centrality_stability_htna()], [reliability_htna()],
#'   [bootstrap_htna()].
#' @examples
#' data(human_ai)
#' net <- build_htna(human_ai, actor_type = "actor_type")
#' casedrop_reliability_htna(net, iter = 20, seed = 1)
#' @export
casedrop_reliability_htna <- function(x, ...) {
  Nestimate::casedrop_reliability(x, ...)
}
