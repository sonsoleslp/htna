#' Count the codes of each actor in a network
#'
#' @description
#' `frequencies_htna()` counts the codes of each actor in an htna network:
#' how often each code occurs and how each actor distributes its activity
#' over its own codes, such as which requests a human makes most often and
#' which actions the AI takes. These counts describe a heterogeneous network
#' before its transitions are read.
#'
#' `frequencies_htna()` counts the occurrences of every code in the
#' sessions of an htna network and reports each code under its actor type,
#' with its share of that actor's events. The proportions therefore sum to
#' one within each actor, not over all events. It runs Nestimate's
#' `state_distribution()` on the network and returns a data frame with one
#' row per actor and code.
#'
#' [plot_frequencies_htna()] draws the table. [state_frequencies_htna()]
#' gives the pooled table without actors, with shares of all events;
#' [state_distribution_htna()] returns the same table as this function.
#'
#' @param x An htna network from [build_htna()].
#'
#' @return A data frame with one row per actor and code, sorted by
#'   decreasing count, and columns `group` (the actor type), `state`,
#'   `count` and `proportion` (the code's share of its actor's events).
#'
#' @section Conditions:
#' An error of class `htna_bad_input` when `x` is not an htna network.
#'
#' @seealso [plot_frequencies_htna()], [state_frequencies_htna()],
#'   [mosaic_plot_htna()].
#' @examples
#' data(human_ai)
#' net <- build_htna(human_ai, actor_type = "actor_type")
#' frequencies_htna(net)
#' @export
frequencies_htna <- function(x) {
  if (!inherits(x, "htna")) {
    .htna_stop("`frequencies_htna()` needs an htna network from build_htna().")
  }
  Nestimate::state_distribution(x)
}
