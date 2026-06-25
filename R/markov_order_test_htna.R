#' Markov-Order Adequacy Test
#'
#' htna-named alias of [Nestimate::markov_order_test()]. Tests
#' whether a first-order Markov model is adequate for the observed
#' sequences, or whether a higher order is required, by comparing the
#' empirical transition structure against orders `1..max_order` via
#' permutation.
#'
#' Operates on raw sequence data (a list of character vectors or a
#' wide-format data frame), not on an htna network object. Use
#' alongside [reliability_htna()] and [casedrop_reliability_htna()]
#' when assessing whether a Markov-1 transition network is
#' appropriate before trusting downstream htna analyses.
#'
#' Suffixed `_htna` to avoid clashing with
#' `Nestimate::markov_order_test()` when both packages are loaded.
#'
#' @inheritParams Nestimate::markov_order_test
#'
#' @return An object of class `markov_order_test` with components
#'   `test_table`, `optimal_order`, and the inputs. See
#'   [Nestimate::markov_order_test()] for full details and the
#'   corresponding `plot()` method.
#'
#' @seealso [reliability_htna()], [casedrop_reliability_htna()],
#'   [centrality_stability_htna()].
#' @examples
#' \donttest{
#' data(human_ai)
#' net <- build_htna(human_ai, actor_type = "actor_type")
#' markov_order_test_htna(net$data, max_order = 2, n_perm = 50, seed = 1)
#' }
#' @export
markov_order_test_htna <- Nestimate::markov_order_test
