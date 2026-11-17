#' Summarize a heterogeneous transition network
#'
#' @description
#' `summary()` summarizes a heterogeneous transition network by its actor
#' partition; `print()` shows the summary and `as.data.frame()` returns its
#' tables. A heterogeneous transition network is described first by which
#' codes belong to which actor type, and how the transitions between codes
#' fall within and between the actors. The summary reports the network's
#' size (sessions, nodes and non-zero edges), the nodes of each actor, the
#' number of non-zero edges in every directed actor-pair block (for example
#' Human to AI), and the standard network-level descriptive metrics
#' (density, mean distance, strength, degree centralization and
#' reciprocity).
#'
#' `summary()` returns the summary as a `summary.htna` object; printing it
#' shows the report. For a grouped network, the summary of every cohort is
#' returned and printed in turn. The tables of the summary are returned as
#' data frames by `as.data.frame()`.
#'
#' @param object An htna network from [build_htna()] or an `htna_group`.
#' @param max_nodes Maximum number of nodes listed per actor type when the
#'   summary is printed (default 12).
#' @param x A `summary.htna` or `summary.htna_group` object.
#' @param row.names,optional Ignored; present for the generic.
#' @param what Table to return: `"actors"` (default; one row per actor type
#'   with its number of nodes and their names), `"actor_pairs"` (one row per
#'   directed actor pair with the number of non-zero edges) or `"metrics"`
#'   (one row per network metric).
#' @param ... Unused.
#'
#' @return `summary()` returns an object of class `summary.htna`: a list
#'   with `actors` (data frame: `actor`, `n_nodes`, `nodes`),
#'   `edges_by_actor` (integer matrix of non-zero edge counts, source actors
#'   in rows), `network_metrics` (data frame: `metric`, `value`), and the
#'   scalars `n_nodes`, `n_edges`, `n_sessions`, `n_timesteps` and `method`.
#'   For an `htna_group` it returns a `summary.htna_group`, a named list of
#'   `summary.htna` objects. `print()` returns its input invisibly.
#'   `as.data.frame()` returns the table chosen by `what`; for a grouped
#'   summary the cohorts are stacked with a leading `group` column.
#'
#' @seealso [build_htna()], [as.data.frame.htna()] for the edge and node
#'   tables of the network itself.
#'
#' @examples
#' data(human_ai)
#' net <- build_htna(human_ai, actor_type = "actor_type")
#' summary(net)
#' as.data.frame(summary(net), what = "actor_pairs")
#'
#' grp <- build_htna(human_ai, actor_type = "actor_type", group = "phase")
#' as.data.frame(summary(grp), what = "metrics")
#'
#' @export
summary.htna <- function(object, max_nodes = 12L, ...) {
  out <- .htna_summary_object(object, .htna_network_summary(object))
  attr(out, "max_nodes") <- max_nodes
  out
}

#' @rdname summary.htna
#' @export
summary.htna_group <- function(object, max_nodes = 12L, ...) {
  nms <- names(object) %||% as.character(seq_along(object))
  out <- lapply(unclass(object), summary.htna, max_nodes = max_nodes)
  names(out) <- if (length(out)) nms else NULL
  structure(out, class = "summary.htna_group")
}

#' @rdname summary.htna
#' @export
print.summary.htna <- function(x, max_nodes = attr(x, "max_nodes") %||% 12L,
                               ...) {
  .print_htna_summary(x, max_nodes = max_nodes)
  invisible(x)
}

#' @rdname summary.htna
#' @export
print.summary.htna_group <- function(x, ...) {
  if (length(x) == 0L) {
    cat("<htna_group: empty>\n")
    return(invisible(x))
  }
  nms <- names(x)
  cat("<htna_group: ", length(x), " cohort",
      if (length(x) != 1L) "s", " (",
      paste(nms, collapse = ", "), ")>\n\n", sep = "")
  lapply(seq_along(x), function(i) {
    cat("=== ", nms[i], " ===\n", sep = "")
    print(x[[i]], ...)
    if (i != length(x)) cat("\n")
  })
  invisible(x)
}

#' @rdname summary.htna
#' @export
as.data.frame.summary.htna <- function(x, row.names = NULL, optional = FALSE,
                                       ...,
                                       what = c("actors", "actor_pairs",
                                                "metrics")) {
  what <- match.arg(what)
  switch(what,
    actors = x$actors,
    actor_pairs = .actor_pair_counts(x$edges_by_actor),
    metrics = {
      metrics <- x$network_metrics
      class(metrics) <- "data.frame"
      metrics
    }
  )
}

#' @rdname summary.htna
#' @export
as.data.frame.summary.htna_group <- function(x, row.names = NULL,
                                             optional = FALSE, ...,
                                             what = c("actors",
                                                      "actor_pairs",
                                                      "metrics")) {
  what <- match.arg(what)
  .stack_by(x, "group", function(item) {
    as.data.frame.summary.htna(item, what = what)
  })
}

#' Tidy one-row-per-block table from the actor edge-count matrix
#' @keywords internal
#' @noRd
.actor_pair_counts <- function(em) {
  actors <- rownames(em)
  blocks <- expand.grid(target_actor = actors, source_actor = actors,
                        stringsAsFactors = FALSE)
  data.frame(
    source_actor = factor(blocks$source_actor, levels = actors),
    target_actor = factor(blocks$target_actor, levels = actors),
    n_edges = em[cbind(blocks$source_actor, blocks$target_actor)],
    stringsAsFactors = FALSE
  )
}

#' Compute the summary list of one htna network
#' @keywords internal
#' @noRd
.htna_summary_object <- function(net, network_metrics = NULL) {
  ng <- net$node_groups
  if (!is.data.frame(ng) || nrow(ng) == 0L) {
    .htna_stop("`object` has no actor partition (`node_groups` is empty); ",
               "build it with `build_htna()`.",
               class = "htna_unassigned_node")
  }
  actor_levels <- net$actor_levels %||% unique(as.character(ng$group))
  W <- net$weights
  data_df <- net$data
  node_actor <- stats::setNames(as.character(ng$group),
                                as.character(ng$node))

  actor_nodes <- lapply(actor_levels, function(a) {
    as.character(ng$node[as.character(ng$group) == a])
  })
  actors_df <- data.frame(
    actor   = actor_levels,
    n_nodes = lengths(actor_nodes),
    nodes   = vapply(actor_nodes, paste, character(1L), collapse = ", "),
    stringsAsFactors = FALSE
  )

  nz <- which(W != 0, arr.ind = TRUE)
  em <- unclass(table(
    factor(node_actor[rownames(W)][nz[, 1L]], levels = actor_levels),
    factor(node_actor[colnames(W)][nz[, 2L]], levels = actor_levels)
  ))
  em <- matrix(as.integer(em), nrow = length(actor_levels),
               dimnames = list(actor_levels, actor_levels))

  structure(list(
    actors          = actors_df,
    edges_by_actor  = em,
    network_metrics = network_metrics,
    n_nodes         = nrow(W),
    n_edges         = sum(W != 0, na.rm = TRUE),
    n_sessions      = if (is.data.frame(data_df)) nrow(data_df) else NA_integer_,
    n_timesteps     = if (is.data.frame(data_df)) ncol(data_df) else NA_integer_,
    method          = net$method %||% "?"
  ), class = "summary.htna")
}

#' Print the report of one `summary.htna`
#' @keywords internal
#' @noRd
.print_htna_summary <- function(s, max_nodes = 12L) {
  cat("<htna network>\n")
  cat(sprintf("  Method:    %s\n", s$method))
  if (!is.na(s$n_sessions)) {
    cat(sprintf("  Sessions:  %d   (max %d timesteps)\n",
                s$n_sessions, s$n_timesteps))
  }
  cat(sprintf("  Nodes:     %d\n", s$n_nodes))
  cat(sprintf("  Edges:     %d / %d (non-zero)\n", s$n_edges,
              s$n_nodes^2))

  actors <- s$actors
  cat(sprintf("\nActor types (%d):\n", nrow(actors)))
  node_lists <- strsplit(actors$nodes, ", ", fixed = TRUE)
  shown <- vapply(node_lists, function(v) {
    suffix <- if (length(v) > max_nodes) {
      sprintf(", ... (+%d more)", length(v) - max_nodes)
    } else ""
    paste0(paste(utils::head(v, max_nodes), collapse = ", "), suffix)
  }, character(1L))
  cat(sprintf("  %-*s (%d nodes):  %s\n", max(nchar(actors$actor)),
              actors$actor, actors$n_nodes, shown), sep = "")

  if (nrow(actors) >= 2L) {
    cat("\nEdge counts by actor (rows = source, cols = target):\n")
    .print_actor_matrix(s$edges_by_actor)
  }
  if (!is.null(s$network_metrics)) {
    cat("\nNetwork metrics:\n")
    print.data.frame(s$network_metrics, row.names = FALSE)
  }
  invisible(s)
}

#' @keywords internal
#' @noRd
.htna_network_summary <- function(net) {
  # Remove only htna's leading dispatch class so the Nestimate engine's
  # summary method computes its canonical metric table. The network itself is
  # not modified.
  plain <- net
  class(plain) <- setdiff(class(plain), "htna")
  summary(plain)
}

#' @keywords internal
#' @noRd
.print_actor_matrix <- function(em) {
  rn <- rownames(em)
  cn <- colnames(em)
  col_w <- max(nchar(cn), max(nchar(as.character(em))), 5L)
  row_w <- max(nchar(rn))
  header <- paste0(sprintf("  %-*s", row_w + 2L, ""),
                   paste(sprintf(" %*s", col_w, cn), collapse = ""))
  rows <- vapply(seq_len(nrow(em)), function(i) {
    paste0(sprintf("  %-*s", row_w + 2L, rn[i]),
           paste(sprintf(" %*d", col_w, em[i, ]), collapse = ""))
  }, character(1L))
  cat(header, rows, sep = "\n")
}
