#' Permutation Test for Network Differences
#'
#' Re-export of [Nestimate::permutation()]. Tests whether observed
#' edge-weight differences between two networks (or all pairwise
#' differences within a `netobject_group`) exceed what would be expected
#' under a null of identical generating processes.
#'
#' Works on htna networks: the actor partition (`$node_groups`,
#' `$actor_levels`, `htna` class) is preserved on `result$x` / `result$y`,
#' so [plot_htna_diff()] can render the result with htna's colour and
#' layout conventions.
#'
#' @return An object of class `net_permutation` (single pair) or
#'   `net_permutation_group` (multiple pairs). See
#'   [Nestimate::permutation()] for the full slot list.
#'
#' @seealso [plot_htna_diff()] to plot the result.
#' @export
#' @importFrom Nestimate permutation
#' @name permutation
NULL
