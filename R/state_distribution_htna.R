#' Tabulate the distribution of the codes within each actor
#'
#' @description
#' `state_distribution_htna()` returns the state distribution of a network:
#' how often each code occurs and its share of the events of its group. For
#' an htna network the group is the actor type, so the table shows how each
#' actor divides its activity over its own codes; for a network without an
#' actor partition all codes form one group, `"all"`.
#'
#' `state_distribution_htna()` runs Nestimate's `state_distribution()` and
#' returns a data frame with one row per group and code. For an htna network
#' it is the same table as [frequencies_htna()]; the function is provided
#' under its general name for code written for other network types.
#'
#' @param x An htna network from [build_htna()], or another network.
#' @param ... Unused for htna networks.
#'
#' @return A data frame with one row per group and code and columns
#'   `group`, `state`, `count` and `proportion` (proportions sum to one
#'   within each group).
#'
#' @seealso [frequencies_htna()], [state_frequencies_htna()] for the
#'   pooled table.
#' @examples
#' data(human_ai)
#' net <- build_htna(human_ai, actor_type = "actor_type")
#' state_distribution_htna(net)
#' @export
state_distribution_htna <- function(x, ...) {
  Nestimate::state_distribution(x, ...)
}
