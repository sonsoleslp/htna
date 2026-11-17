#' Test the Markov order of the sessions
#'
#' @description
#' `markov_order_test_htna()` tests the Markov order of the session
#' sequences of an htna network (or of raw sequence data). The Markov-order
#' test compares the fit of models of increasing order (zero, one, two and
#' so on) with likelihood-ratio statistics, judged against a permutation
#' null, and reports the information criteria of each order. A transition
#' network is a first-order Markov model: the next code is assumed to depend
#' only on the current one. When the next code also depends on the codes
#' before it, a first-order network misses part of the process and a higher
#' order describes it better.
#'
#' The orders are tested in turn: the selected `optimal_order` is the
#' highest order reached before the first step that is not significant at
#' `alpha`, and equals `max_order` when every step is significant, in which
#' case the search should be widened. The orders preferred by BIC and AIC
#' are reported alongside. `markov_order_test_htna()` runs Nestimate's
#' `markov_order_test()` on the sequences and returns a `net_markov_order`
#' object.
#'
#' Use it before interpreting a first-order network, together with
#' [reliability_htna()] and [casedrop_reliability_htna()] for the
#' stability of the estimated weights.
#'
#' @param data An htna network from [build_htna()], or sequence data (a
#'   wide data frame or a list of character vectors).
#' @param ... Settings: `max_order` (default 3), `n_perm` (permutations,
#'   500), `alpha` (0.05), `parallel` (`FALSE`), `n_cores` (2), `seed` and
#'   `dof` (`"paths"` or `"observed"`).
#'
#' @return An object of class `net_markov_order` with `print()`,
#'   `summary()` and `plot()` methods. `summary()` returns a data frame
#'   with one row per tested order and columns `order`, `loglik`, `AIC`,
#'   `BIC`, `df`, `g2`, `p_permutation`, `p_asymptotic` and `significant`.
#'   The object also holds `optimal_order`, `bic_order` and `aic_order`.
#'
#' @seealso [reliability_htna()], [casedrop_reliability_htna()],
#'   [centrality_stability_htna()].
#' @examples
#' data(human_ai)
#' net <- build_htna(human_ai, actor_type = "actor_type")
#' # n_perm = 20 keeps the example fast; use the default 500 in practice.
#' mo <- markov_order_test_htna(net, max_order = 2, n_perm = 20, seed = 1)
#' mo
#' summary(mo)
#' @export
markov_order_test_htna <- function(data, ...) {
  Nestimate::markov_order_test(data, ...)
}
