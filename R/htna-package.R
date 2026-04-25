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
"_PACKAGE"
