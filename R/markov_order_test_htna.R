#' Markov-Order Adequacy Test
#'
#' htna-named wrapper for [Nestimate::markov_order_test()]. Tests
#' whether a first-order Markov model is adequate for the observed
#' sequences, or whether a higher order is required, by comparing the
#' empirical transition structure against orders `1..max_order` via
#' permutation.
#'
#' Accepts an htna network directly (its session sequences are tested),
#' or raw sequence data (a list of character vectors or a wide-format data
#' frame). Use alongside [reliability_htna()] and
#' [casedrop_reliability_htna()] when assessing whether a Markov-1
#' transition network is appropriate before trusting downstream htna
#' analyses.
#'
#' The orders are tested sequentially: `optimal_order` is the highest order
#' `k` for which every step from order `j - 1` to `j`, `j = 1, ..., k`, is
#' significant at `alpha`. It is therefore one less than the first order
#' whose step is not significant, and equals `max_order` when every step is
#' significant (in which case the search should be widened). The BIC- and
#' AIC-preferred orders are reported alongside.
#'
#' Suffixed `_htna` to avoid clashing with
#' `Nestimate::markov_order_test()` when both packages are loaded.
#'
#' @param data An htna network from [build_htna()], or raw sequence data:
#'   a list of character vectors or a wide-format data frame. See
#'   [Nestimate::markov_order_test()].
#' @param ... Additional arguments passed to
#'   [Nestimate::markov_order_test()], such as `max_order`, `n_perm`,
#'   `alpha`, `parallel`, `n_cores`, and `seed`. See that function for
#'   the full, current argument list.
#'
#' @return An object of class `net_markov_order` with components
#'   `optimal_order` (sequential permutation-test order), `bic_order`,
#'   `aic_order`, `test_table` (a data frame with one row per tested order
#'   and columns `order`, `loglik`, `AIC`, `BIC`, `df`, `g2`,
#'   `p_permutation`, `p_asymptotic`, `significant`), the permutation null
#'   distributions, fitted transition matrices, and the inputs. `print()`,
#'   `summary()` and `plot()` methods are supplied by Nestimate; see
#'   [Nestimate::markov_order_test()].
#'
#' @seealso [reliability_htna()], [casedrop_reliability_htna()],
#'   [centrality_stability_htna()].
#' @examples
#' data(human_ai)
#' net <- build_htna(human_ai, actor_type = "actor_type")
#' # Few permutations to keep the example fast; use the default 500 in practice.
#' markov_order_test_htna(net, max_order = 2, n_perm = 20, seed = 1)
#' @export
markov_order_test_htna <- function(data, ...) {
  Nestimate::markov_order_test(data, ...)
}
