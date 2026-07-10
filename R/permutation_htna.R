#' Permutation Test for Network Differences
#'
#' htna-named alias of [Nestimate::permutation()]. Tests whether
#' observed edge-weight differences between two networks (or all
#' pairwise differences within a `netobject_group`) exceed what would
#' be expected under a null of identical generating processes.
#'
#' Works on htna networks: the actor partition (`$node_groups`,
#' `$actor_levels`, `htna` class) is preserved on `result$x` /
#' `result$y`, so [plot_htna_diff()] can render the result with
#' htna's colour and layout conventions.
#'
#' Suffixed `_htna` to avoid clashing with `Nestimate::permutation()`
#' when both packages are loaded.
#'
#' @param x,y The two networks to compare, or a single
#'   `netobject_group` in `x` (with `y = NULL`) for all pairwise
#'   comparisons. See [Nestimate::permutation()].
#' @param ... Additional arguments passed to [Nestimate::permutation()],
#'   such as `iter`, `alpha`, `paired`, `adjust`, `measures`, `nlambda`,
#'   and `seed`. See that function for the full, current argument list.
#'
#' @return An object of class `net_permutation` (single pair) or
#'   `net_permutation_group` (multiple pairs). See
#'   [Nestimate::permutation()] for the full slot list.
#'
#' @seealso [plot_htna_diff()] to plot the result.
#' @examples
#' \donttest{
#' data(human_ai)
#' grp <- build_htna(human_ai, actor_type = "actor_type", group = "phase")
#' permutation_htna(grp$Early, grp$Late, iter = 50)
#' }
#' @export
permutation_htna <- function(x, y = NULL, ...) {
  Nestimate::permutation(x, y = y, ...)
}
