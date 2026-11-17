#' Test differences between networks by permutation
#'
#' @description
#' `permutation_htna()` tests by permutation whether two heterogeneous
#' transition networks, for example of two groups of learners or two phases
#' of a study, differ more than chance would produce. The sessions behind
#' both networks are pooled, the group labels are shuffled many times, both
#' networks are re-estimated on every shuffle, and each observed difference
#' is compared with the shuffled ones. Each edge gets a p-value and an
#' effect size (the observed difference divided by the standard deviation
#' of the shuffled differences), and the networks as a whole are tested
#' with the sum and the maximum of the absolute differences. Because whole
#' sessions are shuffled, the nesting of transitions in sessions is
#' respected.
#'
#' `permutation_htna()` runs Nestimate's `permutation()` with the actor
#' partition kept. It tests two htna networks estimated with the same
#' method on the same codes, or every pair of cohorts of a grouped network.
#' With many edges some fall below `alpha` by chance; set `adjust` (for
#' example `"BH"`) to correct for multiple testing. With `measures`, node
#' centralities are tested as well. It returns a `net_permutation` object,
#' or one per cohort pair for a grouped network.
#'
#' A small p-value says that a difference is larger than the shuffles
#' produce, not how large or how probable it is, which
#' [bayes_compare_htna()] estimates. [compare_htna()] gives descriptive
#' differences per actor pair, and [plot_htna_diff()] draws the result.
#'
#' @param x,y Two htna networks, or an `htna_group` in `x` with `y = NULL`,
#'   in which case every pair of cohorts is tested.
#' @param ... Settings: `iter` (shuffles, default 1000), `alpha` (0.05),
#'   `paired` (`FALSE`), `adjust` (multiplicity correction, `"none"`),
#'   `measures` (centralities to test), `nlambda`, `seed` and `actor`
#'   (shuffle whole actors).
#'
#' @return For two networks, an object of class `net_permutation`; for a
#'   grouped network, a named list of class
#'   `c("net_permutation_group", "list")` with one result per cohort pair
#'   (for example `"Early vs Late"`). Both have `print()` and `summary()`
#'   methods; `summary()` returns a data frame with one row per edge (and
#'   pair) and columns `from`, `to`, `weight_x`, `weight_y`, `diff`,
#'   `effect_size`, `p_value` and `sig`. Differences are `x - y`.
#'
#' @seealso [bayes_compare_htna()], [compare_htna()], [plot_htna_diff()].
#' @examples
#' data(human_ai)
#' grp <- build_htna(human_ai, actor_type = "actor_type", group = "phase")
#' perm <- permutation_htna(grp, iter = 50, seed = 1)
#' perm
#' head(summary(perm))
#' @export
permutation_htna <- function(x, y = NULL, ...) {
  Nestimate::permutation(x, y = y, ...)
}
