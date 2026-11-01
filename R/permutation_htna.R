#' Permutation Test for Network Differences
#'
#' htna-named wrapper for [Nestimate::permutation()]. Tests whether
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
#' @param x,y The two networks to compare, or a grouped network
#'   (`htna_group`, which is also a `netobject_group`) in `x` with
#'   `y = NULL` for all pairwise cohort comparisons. See
#'   [Nestimate::permutation()].
#' @param ... Additional arguments passed to [Nestimate::permutation()],
#'   such as `iter`, `alpha`, `paired`, `adjust`, `measures`, `nlambda`,
#'   `seed`, and `actor`. See that function for the full, current argument
#'   list.
#'
#' @return An object of class `net_permutation` (single pair) or a named
#'   list of class `c("net_permutation_group", "list")` with one
#'   `net_permutation` per cohort pair. Both render with
#'   [plot_htna_diff()]. See [Nestimate::permutation()] for the full slot
#'   list.
#'
#' @seealso [plot_htna_diff()] to plot the result.
#' @examples
#' data(human_ai)
#' grp <- build_htna(human_ai, actor_type = "actor_type", group = "phase")
#' # Pass the grouped network as it is; every cohort pair is tested.
#' permutation_htna(grp, iter = 50, seed = 1)
#' @export
permutation_htna <- function(x, y = NULL, ...) {
  Nestimate::permutation(x, y = y, ...)
}
