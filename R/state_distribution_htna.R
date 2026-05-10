#' State Distribution Across Time
#'
#' htna-named alias of [Nestimate::state_distribution()]. Returns the
#' per-timestep distribution of states across sequences, suitable for
#' driving stacked-area or bar plots.
#'
#' Designed with htna in mind: Nestimate ships an explicit
#' `state_distribution.htna` S3 method that uses the actor partition
#' carried by [build_htna()] networks.
#'
#' Suffixed `_htna` to avoid clashing with
#' `Nestimate::state_distribution()` when both packages are loaded.
#'
#' @inheritParams Nestimate::state_distribution
#'
#' @return A data frame with one row per (timestep, state). See
#'   [Nestimate::state_distribution()] for full details.
#'
#' @seealso [state_frequencies_htna()] for the within-network summary.
#' @examples
#' data(human_long, ai_long, package = "Nestimate")
#' net <- build_htna(list(Human = human_long, AI = ai_long))
#' state_distribution_htna(net)
#' @export
state_distribution_htna <- Nestimate::state_distribution
