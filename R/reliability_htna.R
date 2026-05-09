#' Network Reliability for an htna Network
#'
#' Thin wrapper around [Nestimate::network_reliability()] that preserves
#' the htna actor partition on every model in the returned `$models`
#' slot, so downstream htna-aware code (plotting, centralities, etc.)
#' keeps working on the reliability output.
#'
#' Mirrors the signature of the underlying function: pass one or more
#' networks via `...` plus the optional `iter`, `split`, `scale`, and
#' `seed` arguments. All networks passed through `...` must be htna
#' networks built by [build_htna()].
#'
#' @param ... One or more htna networks built by [build_htna()].
#' @param iter Integer. Number of split-half iterations. Default `1000`.
#' @param split Numeric in (0, 1). Proportion of sessions used for the
#'   first half-network. Default `0.5`.
#' @param scale One of `"none"`, `"minmax"`, `"standardize"`,
#'   `"proportion"`. Forwarded to [Nestimate::network_reliability()].
#' @param seed Optional integer seed for reproducibility. Default `NULL`
#'   (no seed reset).
#'
#' @return An object of class `c("htna_reliability", "net_reliability")`,
#'   with the same components as [Nestimate::network_reliability()] —
#'   `iterations`, `summary`, `models`, `iter`, `split`, `scale` — and
#'   each entry of `$models` carrying the htna actor partition
#'   (`$nodes$groups`, `$node_groups`, `$actor_levels`).
#'
#' @seealso [Nestimate::network_reliability()], [bootstrap_htna()].
#'
#' @examples
#' \dontrun{
#' data(human_long, ai_long, package = "Nestimate")
#' net <- build_htna(list(Human = human_long, AI = ai_long))
#' rel <- reliability_htna(net, iter = 100, seed = 1)
#' rel$summary
#' }
#'
#' @export
reliability_htna <- function(..., iter = 1000L, split = 0.5,
                             scale = "none", seed = NULL) {
  nets <- list(...)
  if (length(nets) == 0L) {
    stop("`reliability_htna()` needs at least one htna network.",
         call. = FALSE)
  }
  ok <- vapply(nets, function(n) inherits(n, "htna"), logical(1L))
  if (!all(ok)) {
    stop("All inputs to `reliability_htna()` must be htna networks ",
         "built by build_htna().", call. = FALSE)
  }

  rel <- Nestimate::network_reliability(..., iter = iter, split = split,
                                        scale = scale, seed = seed)

  # Re-inject the htna actor partition onto each model. Nestimate
  # builds models via build_network() which strips the partition;
  # we restore it slot-for-slot from the matching input network.
  if (!is.null(rel$models) && length(rel$models) == length(nets)) {
    for (i in seq_along(rel$models)) {
      m <- rel$models[[i]]
      src <- nets[[i]]
      if (is.null(m)) next
      if (is.null(m$nodes$groups) && !is.null(src$nodes$groups)) {
        m$nodes$groups <- src$nodes$groups
      }
      if (is.null(m$node_groups) && !is.null(src$node_groups)) {
        m$node_groups <- src$node_groups
      }
      if (is.null(m$actor_levels) && !is.null(src$actor_levels)) {
        m$actor_levels <- src$actor_levels
      }
      if (!inherits(m, "htna")) {
        class(m) <- c("htna", class(m))
      }
      rel$models[[i]] <- m
    }
  }

  if (!inherits(rel, "htna_reliability")) {
    class(rel) <- c("htna_reliability", class(rel))
  }
  rel
}
