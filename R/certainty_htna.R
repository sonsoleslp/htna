#' Estimate the closed-form certainty of the edge weights
#'
#' @description
#' `certainty_htna()` estimates the sampling uncertainty of the edge weights
#' of an htna network in closed form, instead of by resampling. The edge
#' weights of a relative transition network are estimated proportions, so
#' their uncertainty can be derived analytically: for each edge the
#' closed-form model gives a mean, a standard deviation, a confidence
#' interval, a consistency range around the observed weight and a verdict
#' on whether the edge is supported by the data. It serves the same purpose
#' as a bootstrap, showing which edges are estimated precisely enough to
#' interpret, at a fraction of its cost.
#'
#' `certainty_htna()` computes these quantities for an htna network or for
#' every cohort of a grouped network. With `inference = "stability"` an
#' edge is supported when its interval lies within the consistency range;
#' with `"threshold"` when it lies above `edge_threshold`. It runs
#' Nestimate's `certainty()` on the network and returns a `net_certainty`
#' object (a `net_certainty_group` for a grouped network).
#'
#' Use [bootstrap_htna()] for the resampling estimate, which also covers
#' frequency and co-occurrence networks, and [prune_htna()] with
#' `method = "bootstrap"` to keep only supported edges.
#'
#' @param x An htna network from [build_htna()] or an `htna_group`.
#' @param ... Settings: `prior` (default 0.5), `ci_level` (0.05),
#'   `inference` (`"stability"` or `"threshold"`), `consistency_range`
#'   (`c(0.75, 1.25)`) and `edge_threshold`.
#'
#' @return An object of class `c("net_certainty", "net_bootstrap")` with
#'   `print()`, `summary()` and `plot()` methods. `summary()` returns a
#'   data frame with one row per edge and columns `from`, `to`, `weight`,
#'   `mean`, `sd`, `p_value`, `sig`, `ci_lower`, `ci_upper`, `cr_lower`
#'   and `cr_upper`. A grouped network gives a `net_certainty_group`, a
#'   named list with one result per cohort.
#'
#' @seealso [bootstrap_htna()] for the resampling estimate,
#'   [reliability_htna()], [casedrop_reliability_htna()].
#' @examples
#' data(human_ai)
#' net <- build_htna(human_ai, actor_type = "actor_type")
#' cert <- certainty_htna(net)
#' head(summary(cert))
#' @export
certainty_htna <- function(x, ...) {
  Nestimate::certainty(x, ...)
}
