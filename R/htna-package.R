#' htna: Heterogeneous Transition Network Analysis
#'
#' Build, analyze, and visualize transition networks over sequences that mix
#' two or more actor groups (e.g. Human and AI) within the same interaction.
#'
#' The package is a thin layer on top of two foundations:
#' \itemize{
#'   \item \pkg{Nestimate} - estimation, bootstrap, permutation, reliability,
#'     centrality, and Markov machinery.
#'   \item \pkg{cograph} - rendering (\code{splot}, \code{plot_htna},
#'     transitions, communities, chord, heatmap).
#' }
#' \pkg{htna} adds the heterogeneous-actor wrapper \code{\link{build_htna}}
#' and re-exports the most commonly used downstream functions so users do
#' not need to attach \pkg{Nestimate} or \pkg{cograph} explicitly.
#'
#' @keywords internal
#' @importFrom stats setNames
"_PACKAGE"

if (getRversion() >= "2.15.1") {
  # `.data` is ggplot2's tidy-evaluation pronoun.
  # `.param_get` is referenced inside the body of
  # `Nestimate::casedrop_reliability` (which is re-exported as
  # `casedrop_reliability_htna` via assignment); R CMD check sees it as
  # an unresolved global in this package's R-code scan even though it
  # resolves at call time inside the Nestimate namespace.
  utils::globalVariables(c(".data", ".param_get"))
}
