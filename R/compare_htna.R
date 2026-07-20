#' Compare Heterogeneous Transition Networks
#'
#' Computes the descriptive network comparison implemented by
#' [Nestimate::compare_model()] while retaining the HTNA actor partition.
#' Comparing two individual networks returns one `htna_comparison`. Passing an
#' `htna_group` as `x` with `y = NULL` compares every cohort pair and returns an
#' `htna_comparison_group`; callers do not need to select or subset cohorts.
#'
#' Networks may contain different subsets of the declared nodes. Their weight
#' matrices are aligned to the union of nodes, in canonical actor order, and
#' missing nodes are represented by zero rows and columns. Shared nodes must
#' have the same actor assignment in both networks.
#'
#' @param x An `htna` network or an `htna_group`.
#' @param y A second `htna` network. Must be `NULL` when `x` is an
#'   `htna_group`, in which case all cohort pairs are compared.
#' @param ... Additional arguments passed to [Nestimate::compare_model()],
#'   including `scaling`, `measures`, and `network`.
#'
#' @return For two networks, an object inheriting from `htna_comparison` and
#'   `net_comparison`. In addition to the Nestimate comparison fields it
#'   contains:
#'
#'   * `node_groups` and `actor_levels`: the aligned actor partition;
#'   * `edge_metrics$source_actor` and `edge_metrics$target_actor`;
#'   * `actor_pair_metrics`: difference summaries for every directed
#'     actor-type block;
#'   * `models`: the two original, unmodified HTNA networks.
#'
#'   For an `htna_group`, a named `htna_comparison_group` containing every
#'   pairwise comparison is returned.
#'
#' @seealso [Nestimate::compare_model()], [plot_htna_diff()]
#'
#' @examples
#' data(human_ai)
#' grouped <- build_htna(human_ai, actor_type = "actor_type", group = "phase")
#' comparisons <- compare_htna(grouped)
#' names(comparisons)
#'
#' @export
compare_htna <- function(x, y = NULL, ...) {
  if (inherits(x, "htna_group")) {
    if (!is.null(y)) {
      stop("When `x` is an `htna_group`, leave `y = NULL`; all cohort pairs ",
           "are compared automatically.", call. = FALSE)
    }
    return(.compare_htna_group(x, ...))
  }

  if (!inherits(x, "htna") || !inherits(y, "htna")) {
    stop("`x` and `y` must both be `htna` networks, or `x` must be an ",
         "`htna_group`.", call. = FALSE)
  }

  .compare_htna_pair(x, y, labels = c("x", "y"), dots = list(...))
}

#' Plot All Comparisons in an HTNA Comparison Group
#'
#' Applies Nestimate's `net_comparison` plot method to every cohort pair.
#'
#' @param x An `htna_comparison_group` from [compare_htna()].
#' @param ... Passed to the `net_comparison` plot method, including `type` and
#'   `combined`.
#'
#' @return Invisibly, a named list containing the plot for every comparison.
#' @export
plot.htna_comparison_group <- function(x, ...) {
  if (!length(x)) {
    stop("Cannot plot an empty `htna_comparison_group`.", call. = FALSE)
  }
  out <- lapply(x, function(item) plot(item, ...))
  names(out) <- names(x)
  invisible(out)
}

#' @keywords internal
.compare_htna_group <- function(x, ...) {
  if (length(x) < 2L) {
    stop("An `htna_group` needs at least two cohorts for comparison.",
         call. = FALSE)
  }
  if (!all(vapply(x, inherits, logical(1L), what = "htna"))) {
    stop("Every member of `x` must be an `htna` network.", call. = FALSE)
  }

  cohort_names <- names(x) %||% as.character(seq_along(x))
  if (anyNA(cohort_names) || any(!nzchar(cohort_names)) ||
      anyDuplicated(cohort_names)) {
    stop("An `htna_group` must have unique, non-empty cohort names.",
         call. = FALSE)
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
    stop("`x` and `y` must use identical `actor_levels`, including order.",
         call. = FALSE)
  }
  shared <- intersect(names(px$lookup), names(py$lookup))
  conflicts <- shared[px$lookup[shared] != py$lookup[shared]]
  if (length(conflicts)) {
    stop("Shared nodes assigned to different actors: ",
         paste(conflicts, collapse = ", "), ".", call. = FALSE)
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
  rows <- vector("list", length(actor_levels)^2L)
  k <- 0L
  for (source_actor in actor_levels) {
    for (target_actor in actor_levels) {
      k <- k + 1L
      keep <- as.character(edge_metrics$source_actor) == source_actor &
        as.character(edge_metrics$target_actor) == target_actor
      block <- edge_metrics[keep, , drop = FALSE]
      n <- nrow(block)
      rows[[k]] <- data.frame(
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
    }
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out$source_actor <- factor(out$source_actor, levels = actor_levels)
  out$target_actor <- factor(out$target_actor, levels = actor_levels)
  out
}
