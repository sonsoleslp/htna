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
#' @inheritParams Nestimate::permutation
#'
#' @return An object of class `net_permutation` (single pair) or
#'   `net_permutation_group` (multiple pairs). See
#'   [Nestimate::permutation()] for the full slot list.
#'
#' @seealso [plot_htna_diff()] to plot the result.
#' @examples
#' \donttest{
#' data(human_long, ai_long, package = "Nestimate")
#' n <- nrow(human_long)
#' early <- build_htna(list(Human = human_long[seq_len(n %/% 2), ],
#'                          AI    = ai_long[seq_len(n %/% 2), ]))
#' late  <- build_htna(list(Human = human_long[(n %/% 2 + 1):n, ],
#'                          AI    = ai_long[(n %/% 2 + 1):n, ]))
#' permutation_htna(early, late, iter = 100)
#' }
#' @export
permutation_htna <- Nestimate::permutation
