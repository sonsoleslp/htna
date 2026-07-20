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
    stop("`", arg, "` must be an `htna` network.", call. = FALSE)
  }
  W <- net$weights
  if (!is.matrix(W) || !is.numeric(W) || nrow(W) != ncol(W) ||
      is.null(rownames(W)) || is.null(colnames(W)) ||
      !identical(rownames(W), colnames(W))) {
    stop("`", arg, "$weights` must be a named numeric square matrix with ",
         "identical row and column order.", call. = FALSE)
  }
  ng <- net$node_groups
  if (!is.data.frame(ng) ||
      !all(c("node", "group") %in% names(ng))) {
    stop("`", arg, "` has no canonical `(node, group)` actor partition.",
         call. = FALSE)
  }
  nodes <- rownames(W)
  ng_nodes <- as.character(ng$node)
  ng_groups <- as.character(ng$group)
  if (anyNA(ng_nodes) || anyNA(ng_groups)) {
    stop("`", arg, "$node_groups` cannot contain missing values.",
         call. = FALSE)
  }
  if (anyDuplicated(ng_nodes)) {
    stop("`", arg, "$node_groups$node` must be unique.", call. = FALSE)
  }
  missing_nodes <- setdiff(nodes, ng_nodes)
  if (length(missing_nodes)) {
    stop("`", arg, "` has nodes without actor assignments: ",
         paste(missing_nodes, collapse = ", "), ".", call. = FALSE)
  }
  actor_levels <- as.character(net$actor_levels %||% unique(ng_groups))
  unknown_groups <- setdiff(ng_groups[ng_nodes %in% nodes], actor_levels)
  if (length(unknown_groups)) {
    stop("`", arg, "` contains actor assignments absent from `actor_levels`: ",
         paste(unknown_groups, collapse = ", "), ".", call. = FALSE)
  }
  lookup <- stats::setNames(ng_groups, ng_nodes)[nodes]
  list(nodes = nodes, lookup = lookup, actor_levels = actor_levels)
}
