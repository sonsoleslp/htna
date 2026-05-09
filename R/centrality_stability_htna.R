#' Centrality Stability for an htna Network
#'
#' Thin wrapper around [Nestimate::centrality_stability()] that
#' validates the input is an htna network (or `htna_group`), forwards
#' the call, and tags the result with an `htna_stability` class so
#' callers can dispatch on the htna form.
#'
#' For an `htna_group`, the call is iterated per cohort and the result
#' is a named list (one `htna_stability` per cohort), with class
#' `c("htna_stability_group", "list")`.
#'
#' @param x An htna network from [build_htna()] or an `htna_group`.
#' @param measures Character vector of centrality measures to assess.
#'   Default `c("InStrength", "OutStrength", "Betweenness")` — the
#'   three measures whose values are bit-equal between htna's
#'   `cograph` engine and Nestimate's default centrality
#'   implementation. To assess htna's nine measures (including the
#'   three that differ between engines: `BetweennessRSP`, `Diffusion`,
#'   `Clustering`), pass them explicitly together with a custom
#'   `centrality_fn`.
#' @param iter Integer. Number of resamples per drop proportion.
#'   Default `1000`.
#' @param drop_prop Numeric vector. Proportions of sessions to drop.
#'   Default `seq(0.1, 0.9, by = 0.1)`.
#' @param threshold Numeric in \[0, 1\]. Correlation threshold for the
#'   case-dropping stability coefficient. Default `0.7`.
#' @param certainty Numeric in \[0, 1\]. Probability of meeting the
#'   threshold required for a drop proportion to count as stable.
#'   Default `0.95`.
#' @param method Correlation method, one of `"pearson"`, `"spearman"`,
#'   `"kendall"`. Default `"pearson"`.
#' @param centrality_fn Optional function to compute centralities.
#'   Default `NULL` (use Nestimate's internal implementation). See
#'   [Nestimate::centrality_stability()] for the expected signature.
#' @param loops Logical. Whether to retain self-loops. Default `FALSE`.
#' @param seed Optional integer seed for reproducibility. Default
#'   `NULL` (no seed reset).
#'
#' @return For a single htna network, an object of class
#'   `c("htna_stability", "net_stability")` with the same components
#'   as [Nestimate::centrality_stability()]: `cs` (the stability
#'   coefficients, one per measure), `correlations` (per-drop-proportion
#'   correlation traces), and the parameters that were used.
#'   For an `htna_group`, a named list of such objects with class
#'   `c("htna_stability_group", "list")`.
#'
#' @seealso [Nestimate::centrality_stability()],
#'   [bootstrap_htna()], [reliability_htna()].
#'
#' @examples
#' \dontrun{
#' data(human_long, ai_long, package = "Nestimate")
#' net <- build_htna(list(Human = human_long, AI = ai_long))
#' cs  <- centrality_stability_htna(net, iter = 100, seed = 1)
#' cs$cs
#' }
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
    seed          = NULL) {

  if (inherits(x, "htna_group") ||
      (is.list(x) && !inherits(x, "htna") && !is.data.frame(x))) {
    if (length(x) == 0L) stop("Empty htna_group.", call. = FALSE)
    nms <- names(x) %||% as.character(seq_along(x))
    res <- lapply(seq_along(x), function(i) {
      centrality_stability_htna(
        x[[i]], measures = measures, iter = iter, drop_prop = drop_prop,
        threshold = threshold, certainty = certainty, method = method,
        centrality_fn = centrality_fn, loops = loops, seed = seed
      )
    })
    names(res) <- nms
    class(res) <- c("htna_stability_group", "list")
    return(res)
  }
  if (!inherits(x, "htna")) {
    stop("`x` must be an htna network from build_htna() (or htna_group).",
         call. = FALSE)
  }

  cs <- Nestimate::centrality_stability(
    x,
    measures      = measures,
    iter          = iter,
    drop_prop     = drop_prop,
    threshold     = threshold,
    certainty     = certainty,
    method        = method,
    centrality_fn = centrality_fn,
    loops         = loops,
    seed          = seed
  )

  if (!inherits(cs, "htna_stability")) {
    class(cs) <- c("htna_stability", class(cs))
  }
  cs
}
