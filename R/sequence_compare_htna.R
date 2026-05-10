#' Subsequence Pattern Comparison Across Groups
#'
#' htna-named alias of [Nestimate::sequence_compare()]. Extracts all
#' k-gram patterns (subsequences of length `k`) from sequences in
#' each cohort, computes standardised residuals against the
#' independence model, and optionally runs a permutation or
#' chi-square test for differences in pattern rates between cohorts.
#'
#' Operates on grouped htna networks (built via
#' `build_htna(..., group = ...)`), single htna networks paired with a
#' `group` argument, or wide-format sequence data with an explicit
#' `group` argument. The actor partition itself is not consumed by
#' the test — sequence comparison is between cohorts of sessions, not
#' between actors.
#'
#' Suffixed `_htna` to avoid clashing with
#' `Nestimate::sequence_compare()` when both packages are loaded.
#'
#' @inheritParams Nestimate::sequence_compare
#'
#' @return An object of class `net_sequence_compare`. See
#'   [Nestimate::sequence_compare()] for full details and the
#'   corresponding `plot()` method.
#'
#' @seealso [permutation_htna()] for whole-network differences,
#'   [mosaic_plot_htna()] for single-step transition residuals.
#' @examples
#' \donttest{
#' data(human_long, ai_long, package = "Nestimate")
#' net <- build_htna(list(Human = human_long, AI = ai_long))
#' n   <- nrow(net$data)
#' grp <- rep(c("early", "late"), length.out = n)
#' sequence_compare_htna(net, group = grp, iter = 50)
#' }
#' @export
sequence_compare_htna <- Nestimate::sequence_compare
