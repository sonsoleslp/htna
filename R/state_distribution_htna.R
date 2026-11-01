#' State Distribution Across Time
#'
#' htna-named wrapper for [Nestimate::state_distribution()]. Returns the
#' within-network distribution of states: how often each state occurs and
#' its share of the events in its group.
#'
#' Designed with htna in mind: Nestimate ships an explicit
#' `state_distribution.htna` S3 method that uses the actor partition
#' carried by [build_htna()] networks, so each state is reported under its
#' actor type. For an htna network the result is the same table as
#' [frequencies_htna()].
#'
#' Suffixed `_htna` to avoid clashing with
#' `Nestimate::state_distribution()` when both packages are loaded.
#'
#' @param x An htna network (or other object accepted by
#'   [Nestimate::state_distribution()]); S3 dispatch on `x` is
#'   preserved, so `htna` objects use `state_distribution.htna`.
#' @param ... Additional arguments passed to
#'   [Nestimate::state_distribution()]. See that function for details.
#'
#' @return A data frame with one row per `(group, state)` pair and
#'   columns `group`, `state`, `count` and `proportion` (proportions sum
#'   to 1 within each group). For an htna network `group` is the actor
#'   type; for a network without an actor partition it is `"all"`. See [Nestimate::state_distribution()].
#'
#' @seealso [frequencies_htna()], [state_frequencies_htna()] for the
#'   pooled (actor-blind) frequency table.
#' @examples
#' data(human_ai)
#' net <- build_htna(human_ai, actor_type = "actor_type")
#' state_distribution_htna(net)
#' @export
state_distribution_htna <- function(x, ...) {
  Nestimate::state_distribution(x, ...)
}
