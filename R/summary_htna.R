#' Summarise a Heterogeneous Transition Network
#'
#' Prints a per-actor summary of an htna network: which nodes belong to
#' which actor type and how the non-zero edges distribute across the
#' actor partition. The result is also returned invisibly as a list so
#' callers can inspect the structured summary programmatically.
#'
#' @param object An htna network from [build_htna()] or an `htna_group`.
#' @param max_nodes Integer. Maximum number of nodes to list per actor
#'   type before truncating with an ellipsis. Default `12`.
#' @param ... Forwarded for compatibility; currently unused.
#'
#' @return Invisibly, a list with components:
#'   \itemize{
#'     \item `actors` - data frame with one row per actor type (`actor`,
#'       `n_nodes`, `nodes`).
#'     \item `edges_by_actor` - integer matrix of non-zero edge counts,
#'       rows are source actor, columns are target actor.
#'     \item `n_nodes`, `n_edges`, `n_sessions`, `n_timesteps`,
#'       `method`.
#'   }
#'
#' @examples
#' \dontrun{
#' data(human_long, ai_long, package = "Nestimate")
#' net <- build_htna(list(Human = human_long, AI = ai_long))
#' summary(net)
#' }
#'
#' @export
summary.htna <- function(object, max_nodes = 12L, ...) {
  invisible(.print_htna_summary(object, max_nodes = max_nodes))
}

#' @rdname summary.htna
#' @export
summary.htna_group <- function(object, max_nodes = 12L, ...) {
  if (length(object) == 0L) {
    cat("<htna_group: empty>\n")
    return(invisible(list()))
  }
  nms <- names(object) %||% as.character(seq_along(object))
  cat("<htna_group: ", length(object), " cohort",
      if (length(object) != 1L) "s", " (",
      paste(nms, collapse = ", "), ")>\n\n", sep = "")
  out <- vector("list", length(object))
  names(out) <- nms
  for (i in seq_along(object)) {
    cat("=== ", nms[i], " ===\n", sep = "")
    out[[i]] <- .print_htna_summary(object[[i]], max_nodes = max_nodes)
    if (i != length(object)) cat("\n")
  }
  invisible(out)
}

#' @keywords internal
.print_htna_summary <- function(net, max_nodes = 12L) {
  ng <- net$node_groups
  if (is.null(ng) || nrow(ng) == 0L) {
    cat("<htna network: no actor partition>\n")
    return(invisible(NULL))
  }

  actor_levels <- net$actor_levels %||% unique(as.character(ng$group))
  n_nodes      <- nrow(net$weights %||% matrix(0, 0, 0))
  W            <- net$weights
  n_edges_total <- if (!is.null(W)) length(W) else NA_integer_
  n_edges_nz    <- if (!is.null(W)) sum(W != 0, na.rm = TRUE) else NA_integer_

  data_df    <- net$data
  n_sessions <- if (is.data.frame(data_df)) nrow(data_df) else NA_integer_
  n_steps    <- if (is.data.frame(data_df)) ncol(data_df) else NA_integer_
  method     <- net$method %||% "?"

  # ---- Header --------------------------------------------------------
  cat("<htna network>\n")
  cat(sprintf("  Method:    %s\n", method))
  if (!is.na(n_sessions))
    cat(sprintf("  Sessions:  %d   (max %d timesteps)\n",
                n_sessions, n_steps))
  cat(sprintf("  Nodes:     %d\n", n_nodes))
  if (!is.na(n_edges_total))
    cat(sprintf("  Edges:     %d / %d (non-zero)\n",
                n_edges_nz, n_edges_total))

  # ---- Per-actor breakdown ------------------------------------------
  cat(sprintf("\nActor types (%d):\n", length(actor_levels)))
  label_w  <- max(nchar(actor_levels))
  actors_df <- data.frame(
    actor   = actor_levels,
    n_nodes = integer(length(actor_levels)),
    nodes   = character(length(actor_levels)),
    stringsAsFactors = FALSE
  )
  for (i in seq_along(actor_levels)) {
    a       <- actor_levels[i]
    a_nodes <- as.character(ng$node[as.character(ng$group) == a])
    actors_df$n_nodes[i] <- length(a_nodes)
    actors_df$nodes[i]   <- paste(a_nodes, collapse = ", ")
    n_show  <- min(length(a_nodes), max_nodes)
    shown   <- a_nodes[seq_len(n_show)]
    suffix  <- if (length(a_nodes) > max_nodes) {
      sprintf(", ... (+%d more)", length(a_nodes) - max_nodes)
    } else ""
    cat(sprintf("  %-*s (%d nodes):  %s%s\n",
                label_w, a, length(a_nodes),
                paste(shown, collapse = ", "), suffix))
  }

  # ---- Edge-count matrix by actor ----------------------------------
  if (!is.null(W) && length(actor_levels) >= 2L) {
    node_actor <- setNames(as.character(ng$group),
                           as.character(ng$node))
    src_actor  <- node_actor[rownames(W)]
    tgt_actor  <- node_actor[colnames(W)]
    em <- matrix(0L, length(actor_levels), length(actor_levels),
                 dimnames = list(actor_levels, actor_levels))
    nz <- which(W != 0, arr.ind = TRUE)
    if (nrow(nz) > 0L) {
      tab <- table(factor(src_actor[nz[, 1L]], levels = actor_levels),
                   factor(tgt_actor[nz[, 2L]], levels = actor_levels))
      em[] <- as.integer(tab)
    }
    cat("\nEdge counts by actor (rows = source, cols = target):\n")
    .print_actor_matrix(em)
  }

  invisible(list(
    actors         = actors_df,
    edges_by_actor = if (exists("em")) em else NULL,
    n_nodes        = n_nodes,
    n_edges        = n_edges_nz,
    n_sessions     = n_sessions,
    n_timesteps    = n_steps,
    method         = method
  ))
}

#' @keywords internal
.print_actor_matrix <- function(em) {
  rn <- rownames(em); cn <- colnames(em)
  col_w <- max(nchar(cn), max(nchar(as.character(em))), 5L)
  row_w <- max(nchar(rn))
  # Header
  cat(sprintf("  %-*s", row_w + 2L, ""))
  for (c in cn) cat(sprintf(" %*s", col_w, c))
  cat("\n")
  for (i in seq_len(nrow(em))) {
    cat(sprintf("  %-*s", row_w + 2L, rn[i]))
    for (j in seq_len(ncol(em))) {
      cat(sprintf(" %*d", col_w, em[i, j]))
    }
    cat("\n")
  }
}
