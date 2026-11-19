#' Internal Utilities
#'
#' Small, pure helpers shared across htna. Not exported.
#'
#' @keywords internal
#' @name htna-utils
#' @noRd
NULL

#' @keywords internal
#' @noRd
`%||%` <- function(a, b) if (is.null(a)) b else a

#' @keywords internal
#' @noRd
.weights_of <- function(net) {
  net$weights %||% as.matrix(net)
}

#' Validate and extract an HTNA node-to-actor partition
#' @keywords internal
#' @noRd
.htna_node_partition <- function(net, arg = "x") {
  if (!inherits(net, "htna")) {
    .htna_stop("`", arg, "` must be an `htna` network.")
  }
  W <- net$weights
  if (!is.matrix(W) || !is.numeric(W) || nrow(W) != ncol(W) ||
      is.null(rownames(W)) || is.null(colnames(W)) ||
      !identical(rownames(W), colnames(W))) {
    .htna_stop("`", arg, "$weights` must be a named numeric square matrix with ",
               "identical row and column order.")
  }
  ng <- net$node_groups
  if (!is.data.frame(ng) ||
      !all(c("node", "group") %in% names(ng))) {
    .htna_stop("`", arg, "` has no canonical `(node, group)` actor partition.")
  }
  nodes <- rownames(W)
  ng_nodes <- as.character(ng$node)
  ng_groups <- as.character(ng$group)
  if (anyNA(ng_nodes) || anyNA(ng_groups)) {
    .htna_stop("`", arg, "$node_groups` cannot contain missing values.")
  }
  if (anyDuplicated(ng_nodes)) {
    .htna_stop("`", arg, "$node_groups$node` must be unique.")
  }
  missing_nodes <- setdiff(nodes, ng_nodes)
  if (length(missing_nodes)) {
    .htna_stop("`", arg, "` has nodes without actor assignments: ",
               paste(missing_nodes, collapse = ", "), ".",
               class = "htna_unassigned_node")
  }
  actor_levels <- as.character(net$actor_levels %||% unique(ng_groups))
  unknown_groups <- setdiff(ng_groups[ng_nodes %in% nodes], actor_levels)
  if (length(unknown_groups)) {
    .htna_stop("`", arg, "` contains actor assignments absent from `actor_levels`: ",
               paste(unknown_groups, collapse = ", "), ".",
               class = "htna_unassigned_node")
  }
  lookup <- stats::setNames(ng_groups, ng_nodes)[nodes]
  list(nodes = nodes, lookup = lookup, actor_levels = actor_levels)
}

#' Raise a classed htna error
#'
#' Every user-facing error in htna inherits from `htna_error` plus a more
#' specific class, so callers and tests can catch it by class rather than by
#' message text.
#' @param ... Message pieces, pasted together with no separator.
#' @param class The specific condition class (default `"htna_bad_input"`).
#' @keywords internal
#' @noRd
.htna_stop <- function(..., class = "htna_bad_input") {
  stop(errorCondition(paste0(...), class = c(class, "htna_error"),
                      call = NULL))
}

#' Stack the tables of a named list of results
#'
#' Applies `fun` to every element of `x` and row-binds the resulting data
#' frames, adding a leading column `id_col` with the element's name.
#' @keywords internal
#' @noRd
.stack_by <- function(x, id_col, fun) {
  nms <- names(x) %||% as.character(seq_along(x))
  parts <- Map(function(item, nm) {
    d <- fun(item)
    id <- stats::setNames(data.frame(rep(nm, nrow(d)),
                                     stringsAsFactors = FALSE), id_col)
    cbind(id, d)
  }, unclass(x), nms)
  out <- do.call(rbind, unname(parts))
  rownames(out) <- NULL
  out
}

#' Put the actor partition of `src` back on a model the engine returned
#'
#' The estimation engine returns the model of a bootstrap, certainty or
#' reliability result as a plain network. The partition of the htna network
#' it was computed from is restored so the model plots and summarizes as an
#' htna network.
#' @keywords internal
#' @noRd
.restore_htna_model <- function(model, src) {
  if (is.null(model)) return(model)
  if (is.null(model$nodes$groups) && !is.null(src$nodes$groups)) {
    model$nodes$groups <- src$nodes$groups
  }
  if (is.null(model$node_groups) && !is.null(src$node_groups)) {
    model$node_groups <- src$node_groups
  }
  if (is.null(model$actor_levels) && !is.null(src$actor_levels)) {
    model$actor_levels <- src$actor_levels
  }
  if (!inherits(model, "htna")) {
    class(model) <- c("htna", class(model))
  }
  model
}
