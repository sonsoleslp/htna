#' Bayesian Comparison of Two Networks
#'
#' htna-named wrapper for [Nestimate::bayes_compare()]. The Bayesian
#' complement of [permutation_htna()]: instead of a permutation null, it
#' places a prior on the transition structure and returns the posterior
#' distribution of edge-weight differences between two networks.
#'
#' Works on htna networks: the actor partition (`$node_groups`,
#' `$actor_levels`, `htna` class) is preserved on `result$x` /
#' `result$y`, and the result carries class `net_permutation`, so
#' [plot_htna_diff()] can render it with htna's colour and layout
#' conventions — exactly like a [permutation_htna()] result.
#'
#' Suffixed `_htna` to avoid clashing with the tna verb `compare()` and
#' to sit alongside [permutation_htna()] in the htna API.
#'
#' @param x,y The two networks to compare. See
#'   [Nestimate::bayes_compare()].
#' @param ... Additional arguments passed to [Nestimate::bayes_compare()],
#'   such as `prior`, `draws`, `ci`, `mean_threshold`,
#'   `bound_threshold`, and `seed`. See that function for the full,
#'   current argument list.
#'
#' @return An object of class `net_bayes` (also inheriting
#'   `netdifference` and `net_permutation`). See
#'   [Nestimate::bayes_compare()] for the full slot list.
#'
#' @seealso [permutation_htna()] for the permutation-test analogue,
#'   [plot_htna_diff()] to plot the result.
#' @examples
#' \donttest{
#' data(human_ai)
#' grp <- build_htna(human_ai, actor_type = "actor_type", group = "phase")
#' # Pass the grouped network as it is; all pairwise comparisons are returned.
#' bayes_compare_htna(grp, draws = 200, seed = 1)
#' }
#' @export
bayes_compare_htna <- function(x, y = NULL, ...) {
  Nestimate::bayes_compare(x, y = y, ...)
}
