#' Compare networks edge by edge with a Bayesian model
#'
#' @description
#' `bayes_compare_htna()` compares two heterogeneous transition networks,
#' for example of early and late sessions, edge by edge with a Bayesian
#' model: how much more or less likely a human request is to be followed by
#' an AI plan in one network than in the other, and how precisely the data
#' estimate that difference. The model treats the transitions out of each
#' code as a multinomial draw with a Dirichlet prior, so every transition
#' probability has a Beta posterior. For each edge it reports the posterior
#' mean difference, a credible interval, and the probability that the
#' difference lies on its side of zero. An edge whose source code has few
#' outgoing transitions gets a wide interval even when its probabilities
#' look far apart.
#'
#' `bayes_compare_htna()` runs Nestimate's `bayes_compare()` with the actor
#' partition kept. It compares two htna networks built with
#' `method = "relative"` or `"frequency"` on the same codes, or every pair
#' of cohorts of a grouped network. An edge is flagged as credibly
#' different when its interval excludes zero and its mean and nearest bound
#' pass `mean_threshold` and `bound_threshold`; no correction is made for
#' testing many edges, and every transition is treated as an independent
#' draw, so the nesting of transitions in sessions is ignored. It returns a
#' `net_bayes` object, or one per cohort pair for a grouped network.
#'
#' The permutation test of [permutation_htna()] respects that nesting and
#' asks whether a difference exceeds chance; [compare_htna()] gives
#' descriptive similarity measures and the differences per actor pair.
#' [plot_htna_diff()] draws the result.
#'
#' @param x,y Two htna networks, or an `htna_group` in `x` with `y = NULL`,
#'   in which case every pair of cohorts is compared.
#' @param ... Settings: `prior` (Dirichlet concentration, default 0.5),
#'   `draws` (Monte Carlo draws, 10000), `ci` (credible level, 0.95),
#'   `mean_threshold` (0.01), `bound_threshold` (0.001) and `seed`.
#'
#' @return For two networks, an object of class
#'   `c("net_bayes", "netdifference", "net_permutation")` with `print()`,
#'   `summary()` and `plot()` methods. For a grouped network, a named list
#'   of class `c("net_bayes_group", "list")` with one result per cohort
#'   pair (for example `"Early vs Late"`). `summary()` returns a data frame
#'   with one row per edge (and pair) and columns including `from`, `to`,
#'   `weight_x`, `weight_y`, `diff`, `effect_size`, `ci_lower`, `ci_upper`,
#'   `p_difference`, `p_value` and `sig`. Differences are `x - y`; in a
#'   grouped comparison `x` is the first cohort of the pair.
#'
#' @seealso [permutation_htna()], [compare_htna()], [plot_htna_diff()].
#' @examples
#' data(human_ai)
#' grp <- build_htna(human_ai, actor_type = "actor_type", group = "phase")
#' bc <- bayes_compare_htna(grp, draws = 200, seed = 1)
#' head(summary(bc))
#' plot_htna_diff(bc)
#' @export
bayes_compare_htna <- function(x, y = NULL, ...) {
  Nestimate::bayes_compare(x, y = y, ...)
}
