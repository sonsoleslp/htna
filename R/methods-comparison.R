#' Print, summarize and tabulate an htna network comparison
#'
#' @description
#' `print()`, `summary()` and `as.data.frame()` read an `htna_comparison`
#' object from [compare_htna()], which describes how two heterogeneous
#' transition networks differ edge by edge. Beyond the overall similarity
#' and distance measures of the two weight matrices, the comparison splits
#' the differences by actor-pair block: the transitions within each actor
#' (for example Human to Human) and between actors (Human to AI, AI to
#' Human). The actor-pair table shows where in the actor structure the two
#' networks differ most.
#'
#' `print()` shows the overall measures and the actor-pair table; for a
#' grouped comparison it shows, for every cohort pair, the correlation and
#' mean absolute difference of the edge weights and the actor-pair table.
#' `summary()` returns the actor-pair table. `as.data.frame()` returns any
#' of the comparison's tables, chosen with `what`; for a grouped comparison
#' the tables of every pair are stacked with a leading `comparison` column.
#'
#' @param x,object An `htna_comparison` or `htna_comparison_group` from
#'   [compare_htna()].
#' @param row.names,optional Ignored; present for the generic.
#' @param what Table to return: `"actor_pairs"` (default; one row per
#'   directed actor pair), `"edges"` (one row per edge of the aligned
#'   networks), `"metrics"` (one row per similarity or distance measure) or
#'   `"network"` (one row per network-level metric, with the values of both
#'   networks).
#' @param ... Unused.
#'
#' @return `print()` returns `x` invisibly. `summary()` returns the
#'   actor-pair data frame. `as.data.frame()` returns a data frame:
#'
#'   * `"actor_pairs"`: `source_actor`, `target_actor`, `n_cells`,
#'     `n_nonzero_x`, `n_nonzero_y`, `n_changed`, `mean_difference`,
#'     `mean_absolute_difference`, `rms_difference`,
#'     `max_absolute_difference`; differences are `x - y`.
#'   * `"edges"`: `source`, `target`, `weight_x`, `weight_y`, the
#'     difference measures of each edge, `source_actor` and `target_actor`.
#'   * `"metrics"`: `category`, `metric`, `value`.
#'   * `"network"`: `metric`, `x`, `y`.
#'
#'   For a grouped comparison every table has a first column `comparison`
#'   (for example `"Early vs Late"`), and the first-named cohort is `x`.
#'
#' @seealso [compare_htna()], [plot.htna_comparison_group()]
#'
#' @examples
#' data(human_ai)
#' grp <- build_htna(human_ai, actor_type = "actor_type", group = "phase")
#' comparison <- compare_htna(grp)
#' comparison
#' summary(comparison)
#' as.data.frame(comparison, what = "metrics")
#' @name htna_comparison-methods
NULL

#' @rdname htna_comparison-methods
#' @export
print.htna_comparison <- function(x, ...) {
  labels <- x$comparison %||% c(x = "x", y = "y")
  cat("HTNA network comparison: ", labels[["x"]], " vs ", labels[["y"]],
      "\n\n", sep = "")
  NextMethod()
  cat("\nActor-pair blocks (differences are ", labels[["x"]], " - ",
      labels[["y"]], "):\n", sep = "")
  print(.format_actor_pairs(x$actor_pair_metrics), row.names = FALSE)
  invisible(x)
}

#' @rdname htna_comparison-methods
#' @export
summary.htna_comparison <- function(object, ...) {
  as.data.frame.htna_comparison(object, what = "actor_pairs")
}

#' @rdname htna_comparison-methods
#' @export
as.data.frame.htna_comparison <- function(x, row.names = NULL,
                                          optional = FALSE, ...,
                                          what = c("actor_pairs", "edges",
                                                   "metrics", "network")) {
  what <- match.arg(what)
  out <- switch(what,
    actor_pairs = x$actor_pair_metrics,
    edges       = x$edge_metrics,
    metrics     = x$summary_metrics,
    network     = x$network_metrics
  )
  class(out) <- "data.frame"
  rownames(out) <- NULL
  out
}

#' @rdname htna_comparison-methods
#' @export
print.htna_comparison_group <- function(x, ...) {
  cat("HTNA network comparisons: ", length(x), " cohort pair",
      if (length(x) != 1L) "s", "\n", sep = "")
  lapply(names(x), function(pair) {
    item <- x[[pair]]
    metrics <- item$summary_metrics
    pearson <- metrics$value[metrics$metric == "Pearson"]
    mad <- metrics$value[metrics$metric == "Mean Abs. Diff."]
    cat("\n--- ", pair, " ---\n", sep = "")
    cat(sprintf("  Pearson correlation of edge weights: %.3f\n", pearson))
    cat(sprintf("  Mean absolute difference:            %.4f\n", mad))
    print(.format_actor_pairs(item$actor_pair_metrics), row.names = FALSE)
  })
  invisible(x)
}

#' @rdname htna_comparison-methods
#' @export
summary.htna_comparison_group <- function(object, ...) {
  as.data.frame.htna_comparison_group(object, what = "actor_pairs")
}

#' @rdname htna_comparison-methods
#' @export
as.data.frame.htna_comparison_group <- function(x, row.names = NULL,
                                                optional = FALSE, ...,
                                                what = c("actor_pairs",
                                                         "edges", "metrics",
                                                         "network")) {
  what <- match.arg(what)
  .stack_by(x, "comparison", function(item) {
    as.data.frame.htna_comparison(item, what = what)
  })
}

#' Compact columns of the actor-pair table for printing
#' @keywords internal
#' @noRd
.format_actor_pairs <- function(pairs) {
  data.frame(
    source = pairs$source_actor,
    target = pairs$target_actor,
    cells = pairs$n_cells,
    changed = pairs$n_changed,
    mean_diff = signif(pairs$mean_difference, 3),
    mean_abs_diff = signif(pairs$mean_absolute_difference, 3),
    max_abs_diff = signif(pairs$max_absolute_difference, 3),
    stringsAsFactors = FALSE
  )
}
