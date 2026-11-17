#' Describe the differences between heterogeneous transition networks
#'
#' @description
#' `compare_htna()` describes the differences between two heterogeneous
#' transition networks, or between every pair of cohorts of a grouped
#' network: overall, in how similar their weight matrices are, and in where
#' the differences lie in the actor structure, within an actor (for example,
#' how humans move between their own codes) or between actors (how the
#' human hands over to the AI and back). It reports correlation, distance
#' and similarity measures of the two weight matrices, the network-level
#' metrics of each network, the difference of every edge, and a summary of
#' the differences in each directed actor-pair block.
#'
#' Networks that contain different subsets of the codes are aligned on the
#' union of their codes, in actor order, with the missing codes treated as
#' having no transitions; a code shared by both networks must belong to the
#' same actor in both. Differences are `x - y`. `compare_htna()` runs
#' Nestimate's `compare_model()` on the aligned networks and adds the
#' actor-pair summary. It returns an `htna_comparison` object (an
#' `htna_comparison_group` for a grouped network).
#'
#' The comparison is descriptive and gives no test. Use
#' [permutation_htna()] for a permutation test of the differences and
#' [bayes_compare_htna()] for credible intervals; [plot_htna_diff()] draws
#' the difference network. The tables of the result are returned by
#' `summary()` and `as.data.frame()` (see [htna_comparison-methods]).
#'
#' @param x An htna network, or an `htna_group`.
#' @param y A second htna network; `NULL` (default) when `x` is an
#'   `htna_group`, in which case every pair of cohorts is compared.
#' @param ... Settings of the comparison: `scaling`, `measures` and
#'   `network`.
#'
#' @return For two networks, an object of class
#'   `c("htna_comparison", "net_comparison")`. For an `htna_group`, a named
#'   list of class `c("htna_comparison_group", "list")` with one comparison
#'   per cohort pair (for example `"Early vs Late"`). `summary()` returns
#'   the actor-pair table, one row per directed actor pair;
#'   `as.data.frame(x, what = )` returns the actor-pair, edge, metric or
#'   network table (see [htna_comparison-methods]).
#'
#' @section Conditions:
#' Errors of class `htna_bad_input` when `x` and `y` are not htna networks,
#' when `y` is given with an `htna_group`, or when the networks use different
#' actor types; `htna_empty_input` when a group has fewer than two cohorts;
#' `htna_actor_conflict` when a shared code belongs to different actors.
#'
#' @seealso [htna_comparison-methods], [permutation_htna()],
#'   [bayes_compare_htna()], [plot_htna_diff()].
#'
#' @examples
#' data(human_ai)
#' grp <- build_htna(human_ai, actor_type = "actor_type", group = "phase")
#' comparison <- compare_htna(grp)
#' comparison
#' summary(comparison)
#'
#' @export
compare_htna <- function(x, y = NULL, ...) {
  if (inherits(x, "htna_group")) {
    if (!is.null(y)) {
      .htna_stop("When `x` is an `htna_group`, leave `y = NULL`; all cohort pairs ",
                 "are compared automatically.")
    }
    return(.compare_htna_group(x, ...))
  }

  if (!inherits(x, "htna") || !inherits(y, "htna")) {
    .htna_stop("`x` and `y` must both be `htna` networks, or `x` must be an ",
               "`htna_group`.")
  }

  .compare_htna_pair(x, y, labels = c("x", "y"), dots = list(...))
}

#' Plot the comparisons of a grouped network
#'
#' @description
#' `plot()` draws the plots of a descriptive network comparison for every
#' cohort pair of a grouped comparison from [compare_htna()]: a scatter plot
#' of the edge weights of the two networks, a heatmap of the edge
#' differences, a histogram of the differences, or the weight distributions.
#' It returns, invisibly, a named list with the plot of every cohort pair.
#'
#' @param x An `htna_comparison_group` from [compare_htna()].
#' @param ... Plot settings: `type` (`"scatter"`, `"heatmap"`,
#'   `"diff_hist"`, `"weight_dist"` or `"all"`) and `combined`.
#'
#' @return Invisibly, a named list with the plot of every cohort pair.
#'
#' @section Conditions:
#' An error of class `htna_empty_input` when `x` is empty.
#'
#' @seealso [compare_htna()], [plot_htna_diff()] for the difference network.
#' @examples
#' data(human_ai)
#' grp <- build_htna(human_ai, actor_type = "actor_type", group = "phase")
#' plot(compare_htna(grp), type = "scatter")
#' @export
plot.htna_comparison_group <- function(x, ...) {
  if (!length(x)) {
    .htna_stop("Cannot plot an empty `htna_comparison_group`.",
               class = "htna_empty_input")
  }
  out <- lapply(x, function(item) plot(item, ...))
  names(out) <- names(x)
  invisible(out)
}

#' @keywords internal
.compare_htna_group <- function(x, ...) {
  if (length(x) < 2L) {
    .htna_stop("An `htna_group` needs at least two cohorts for comparison.",
               class = "htna_empty_input")
  }
  if (!all(vapply(x, inherits, logical(1L), what = "htna"))) {
    .htna_stop("Every member of `x` must be an `htna` network.")
  }

  cohort_names <- names(x) %||% as.character(seq_along(x))
  if (anyNA(cohort_names) || any(!nzchar(cohort_names)) ||
      anyDuplicated(cohort_names)) {
    .htna_stop("An `htna_group` must have unique, non-empty cohort names.")
  }

  pairs <- utils::combn(seq_along(x), 2L)
  pair_names <- apply(pairs, 2L, function(idx) {
    paste(cohort_names[idx], collapse = " vs ")
  })
  dots <- list(...)
  out <- lapply(seq_len(ncol(pairs)), function(i) {
    idx <- pairs[, i]
    .compare_htna_pair(
      x[[idx[1L]]], x[[idx[2L]]],
      labels = cohort_names[idx], dots = dots
    )
  })
  names(out) <- pair_names

  comparison_index <- data.frame(
    comparison = pair_names,
    x = cohort_names[pairs[1L, ]],
    y = cohort_names[pairs[2L, ]],
    stringsAsFactors = FALSE
  )
  structure(
    out,
    class = c("htna_comparison_group", "list"),
    comparisons = comparison_index,
    actor_levels = attr(x, "actor_levels") %||% x[[1L]]$actor_levels
  )
}

#' @keywords internal
.compare_htna_pair <- function(x, y, labels, dots) {
  aligned <- .align_htna_comparison_pair(x, y)
  result <- do.call(
    Nestimate::compare_model,
    c(list(x = aligned$x, y = aligned$y), dots)
  )

  source_actor <- aligned$actor_lookup[as.character(result$edge_metrics$source)]
  target_actor <- aligned$actor_lookup[as.character(result$edge_metrics$target)]
  result$edge_metrics$source_actor <- factor(
    unname(source_actor), levels = aligned$actor_levels
  )
  result$edge_metrics$target_actor <- factor(
    unname(target_actor), levels = aligned$actor_levels
  )
  result$actor_pair_metrics <- .htna_actor_pair_comparison(
    result$edge_metrics, aligned$actor_levels
  )
  result$node_groups <- aligned$node_groups
  result$actor_levels <- aligned$actor_levels
  result$models <- stats::setNames(list(x, y), labels)
  result$comparison <- stats::setNames(labels, c("x", "y"))
  class(result) <- unique(c("htna_comparison", class(result)))
  result
}

#' @keywords internal
.align_htna_comparison_pair <- function(x, y) {
  px <- .htna_node_partition(x, "x")
  py <- .htna_node_partition(y, "y")

  if (!identical(px$actor_levels, py$actor_levels)) {
    .htna_stop("`x` and `y` must use identical `actor_levels`, including order.")
  }
  shared <- intersect(names(px$lookup), names(py$lookup))
  conflicts <- shared[px$lookup[shared] != py$lookup[shared]]
  if (length(conflicts)) {
    .htna_stop("Shared nodes assigned to different actors: ",
               paste(conflicts, collapse = ", "), ".",
               class = "htna_actor_conflict")
  }

  actor_lookup <- c(px$lookup, py$lookup[setdiff(names(py$lookup),
                                                 names(px$lookup))])
  all_nodes <- unique(c(px$nodes, py$nodes))
  actor_lookup <- actor_lookup[all_nodes]
  ordered_nodes <- unlist(lapply(px$actor_levels, function(actor) {
    sort(all_nodes[actor_lookup[all_nodes] == actor], method = "radix")
  }), use.names = FALSE)

  pad <- function(net) {
    W <- net$weights
    out <- matrix(
      0, nrow = length(ordered_nodes), ncol = length(ordered_nodes),
      dimnames = list(ordered_nodes, ordered_nodes)
    )
    out[rownames(W), colnames(W)] <- W
    out
  }
  groups <- unname(actor_lookup[ordered_nodes])
  node_groups <- data.frame(
    node = ordered_nodes,
    group = as.character(groups),
    stringsAsFactors = FALSE
  )
  attr(node_groups, "actor_levels") <- px$actor_levels

  list(
    x = pad(x),
    y = pad(y),
    node_groups = node_groups,
    actor_levels = px$actor_levels,
    actor_lookup = stats::setNames(as.character(groups), ordered_nodes)
  )
}

#' @keywords internal
.htna_actor_pair_comparison <- function(edge_metrics, actor_levels) {
  # Every directed (source actor, target actor) block, source-major.
  blocks <- expand.grid(target_actor = actor_levels,
                        source_actor = actor_levels,
                        stringsAsFactors = FALSE)
  src <- as.character(edge_metrics$source_actor)
  tgt <- as.character(edge_metrics$target_actor)
  rows <- Map(function(source_actor, target_actor) {
    block <- edge_metrics[src == source_actor & tgt == target_actor, ,
                          drop = FALSE]
    n <- nrow(block)
    data.frame(
      source_actor = source_actor,
      target_actor = target_actor,
      n_cells = n,
      n_nonzero_x = if (n) sum(block$weight_x != 0) else 0L,
      n_nonzero_y = if (n) sum(block$weight_y != 0) else 0L,
      n_changed = if (n) sum(block$absolute_difference > 0) else 0L,
      mean_difference = if (n) mean(block$raw_difference) else NA_real_,
      mean_absolute_difference = if (n) {
        mean(block$absolute_difference)
      } else NA_real_,
      rms_difference = if (n) {
        sqrt(mean(block$squared_difference))
      } else NA_real_,
      max_absolute_difference = if (n) {
        max(block$absolute_difference)
      } else NA_real_,
      stringsAsFactors = FALSE
    )
  }, blocks$source_actor, blocks$target_actor)
  out <- do.call(rbind, unname(rows))
  rownames(out) <- NULL
  out$source_actor <- factor(out$source_actor, levels = actor_levels)
  out$target_actor <- factor(out$target_actor, levels = actor_levels)
  out
}
