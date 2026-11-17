#' Assess the case-dropping stability of node centralities
#'
#' @description
#' `centrality_stability_htna()` computes the case-dropping stability (CS)
#' coefficients of the node centralities of an htna network, or of every
#' cohort of a grouped network (Epskamp et al., 2018). The procedure
#' re-estimates the network after dropping increasing proportions of the
#' sessions and correlates the centralities of each reduced network with
#' those of the full network; the CS coefficient of a measure is the largest
#' proportion of sessions that can be dropped while the correlation stays
#' above `threshold` with probability `certainty`. Epskamp et al. (2018)
#' recommend a coefficient of at least 0.25 and preferably above 0.5.
#'
#' Centrality measures rank the codes of a network, for example by how much
#' transition weight flows into them (in-strength) or how often they lie on
#' shortest paths (betweenness); before such a ranking is interpreted, it
#' should be shown not to depend on a few sessions.
#' `centrality_stability_htna()` runs Nestimate's `centrality_stability()`
#' on the network and returns an `htna_stability` object (an
#' `htna_stability_group` for a grouped network). Printing the result shows
#' the coefficients, [plot.htna_stability()] draws the correlation traces,
#' and [as.data.frame.htna_stability()] returns the coefficients or the
#' traces as data frames.
#'
#' For the stability of the edge weights use [casedrop_reliability_htna()];
#' for edge-level intervals use [bootstrap_htna()].
#'
#' @param x An htna network from [build_htna()] or an `htna_group`.
#' @param measures Centrality measures to assess (default
#'   `c("InStrength", "OutStrength", "Betweenness")`).
#' @param iter Resamples per drop proportion (default 1000).
#' @param drop_prop Proportions of sessions to drop (default
#'   `seq(0.1, 0.9, by = 0.1)`).
#' @param threshold Correlation the reduced networks must reach
#'   (default 0.7).
#' @param certainty Probability of reaching `threshold` required at a drop
#'   proportion (default 0.95).
#' @param method Correlation: `"pearson"` (default), `"spearman"` or
#'   `"kendall"`.
#' @param centrality_fn A function computing the centralities from a weight
#'   matrix; `NULL` (default) uses the built-in measures.
#' @param loops Keep self-transitions when computing centralities (default
#'   `FALSE`).
#' @param seed Random seed (default `NULL`, no seed is set).
#' @param ... Further arguments passed to
#'   `Nestimate::centrality_stability()`, for example `actor` (drop whole
#'   actors instead of sessions; Nestimate 1.0.19 or later).
#'
#' @return For a network, an object of class
#'   `c("htna_stability", "net_stability")` holding the CS coefficients
#'   (`cs`, one per measure), the correlation traces and the settings. For
#'   an `htna_group`, a named list of such objects with class
#'   `c("htna_stability_group", "list")`. `as.data.frame()` returns the
#'   coefficients (one row per measure) or, with
#'   `what = "correlations"`, the traces (one row per measure and drop
#'   proportion); `summary()` returns the traces.
#'
#' @section Conditions:
#' Errors of class `htna_bad_input` when `x` is not an htna network and
#' `htna_empty_input` when a group is empty.
#'
#' @seealso [plot.htna_stability()], [as.data.frame.htna_stability()],
#'   [casedrop_reliability_htna()], [bootstrap_htna()].
#'
#' @references
#' Epskamp, S., Borsboom, D., & Fried, E. I. (2018). Estimating
#' psychological networks and their accuracy: A tutorial paper.
#' *Behavior Research Methods*, 50(1), 195-212.
#' \doi{10.3758/s13428-017-0862-1}
#'
#' @examples
#' data(human_ai)
#' net <- build_htna(human_ai, actor_type = "actor_type")
#' cs  <- centrality_stability_htna(net, iter = 30, seed = 1)
#' cs
#' as.data.frame(cs, what = "correlations")
#'
#' @export
centrality_stability_htna <- function(
    x,
    measures      = c("InStrength", "OutStrength", "Betweenness"),
    iter          = 1000L,
    drop_prop     = seq(0.1, 0.9, by = 0.1),
    threshold     = 0.7,
    certainty     = 0.95,
    method        = "pearson",
    centrality_fn = NULL,
    loops         = FALSE,
    seed          = NULL,
    ...) {

  if (inherits(x, "htna_group") ||
      (is.list(x) && !inherits(x, "htna") && !is.data.frame(x))) {
    if (length(x) == 0L) .htna_stop("Empty htna_group.",
                                    class = "htna_empty_input")
    nms <- names(x) %||% as.character(seq_along(x))
    res <- lapply(seq_along(x), function(i) {
      centrality_stability_htna(
        x[[i]], measures = measures, iter = iter, drop_prop = drop_prop,
        threshold = threshold, certainty = certainty, method = method,
        centrality_fn = centrality_fn, loops = loops, seed = seed, ...
      )
    })
    names(res) <- nms
    class(res) <- c("htna_stability_group", "list")
    return(res)
  }
  if (!inherits(x, "htna")) {
    .htna_stop("`x` must be an htna network from build_htna() (or htna_group).")
  }

  cs <- Nestimate::centrality_stability(
    x, measures = measures, iter = iter, drop_prop = drop_prop,
    threshold = threshold, certainty = certainty, method = method,
    centrality_fn = centrality_fn, loops = loops, seed = seed, ...
  )

  if (!inherits(cs, "htna_stability")) {
    class(cs) <- c("htna_stability", class(cs))
  }
  cs
}
