# Helpers for constructing HTNA groups from fitted clustering results --------

#' @keywords internal
.is_htna_clustering_input <- function(x) {
  inherits(x, c(
    "netobject_group", "net_clustering", "net_mmm",
    "net_mmm_clustering", "tna_clustering"
  ))
}

#' @keywords internal
.build_htna_from_clustering <- function(data, node_groups, action, method,
                                        dots = list()) {
  source <- data

  if (inherits(data, "netobject_group")) {
    nets <- data
  } else if (inherits(data, c("net_clustering", "net_mmm"))) {
    nets <- do.call(
      Nestimate::build_network,
      c(list(data = data, method = method), dots)
    )
  } else if (inherits(data, c("tna_clustering", "net_mmm_clustering"))) {
    nets <- .htna_build_assigned_networks(data, method = method, dots = dots)
  } else { # nocov start
    stop("Unsupported clustering result class: ", class(data)[1L],
         call. = FALSE) # nocov end
  }

  if (!inherits(nets, "netobject_group")) {
    stop("Clustering conversion must produce a `netobject_group`; got `",
         class(nets)[1L], "`.", call. = FALSE)
  }

  partition <- .htna_resolve_cluster_partition(
    nets = nets,
    source = source,
    node_groups = node_groups,
    action = action
  )

  all_nodes <- unique(unlist(lapply(
    nets,
    function(net) as.character(net$nodes$label)
  ), use.names = FALSE))
  missing <- setdiff(all_nodes, names(partition$lookup))
  if (length(missing)) {
    stop("Nodes in the clustered networks are not assigned to an actor: ",
         paste(missing, collapse = ", "), ".", call. = FALSE)
  }

  # Replace children in place so Nestimate's clustering diagnostics and
  # group attributes survive the conversion.
  for (i in seq_along(nets)) {
    nets[[i]] <- .inject_htna_partition(
      nets[[i]], partition$lookup, partition$actor_levels
    )
  }

  class(nets) <- unique(c("htna_group", "netobject_group", "list", class(nets)))
  attr(nets, "actor_levels") <- partition$actor_levels
  nets
}

#' @keywords internal
.htna_build_assigned_networks <- function(x, method, dots = list()) {
  seq_data <- x$data
  assignments <- x$assignments
  k <- x$k %||% max(assignments, na.rm = TRUE)
  long_only <- c(
    "actor", "time", "session", "order", "action",
    "state_cols", "metadata_cols"
  )
  dot_names <- names(dots)
  if (!is.null(dot_names)) {
    dots <- dots[!nzchar(dot_names) | !dot_names %in% long_only]
  }

  if (!is.data.frame(seq_data) && !is.matrix(seq_data)) {
    stop("The clustering result does not retain sequence data; its networks ",
         "cannot be built.", call. = FALSE)
  }
  if (length(assignments) != nrow(seq_data)) {
    stop("The clustering assignments do not align with the retained sequence ",
         "data.", call. = FALSE)
  }

  nets <- lapply(seq_len(k), function(cluster_id) {
    cluster_data <- seq_data[assignments == cluster_id, , drop = FALSE]
    do.call(
      Nestimate::build_network,
      c(list(data = cluster_data, method = method), dots)
    )
  })
  names(nets) <- paste("Cluster", seq_len(k))
  attr(nets, "group_col") <- "cluster"
  attr(nets, "clustering") <- x
  class(nets) <- c("netobject_group", "list")
  nets
}

#' @keywords internal
.htna_resolve_cluster_partition <- function(nets, source, node_groups, action) {
  if (!is.null(node_groups)) {
    result <- .htna_normalize_cluster_partition(node_groups, action = action)

    # Canonical HTNA node_groups deliberately stores character labels; actor
    # order lives separately in actor_levels. When a preserved partition is
    # available, retain that order instead of re-deriving it from node row
    # order (which is commonly alphabetical by node label).
    preserved_levels <- attr(source, "actor_levels") %||%
      attr(nets, "actor_levels")
    if (is.null(preserved_levels) && inherits(source, "netobject")) {
      preserved_levels <- source$actor_levels
    }
    if (is.null(preserved_levels) && length(nets)) {
      preserved_levels <- nets[[1L]]$actor_levels
    }
    if (!is.null(preserved_levels) &&
        setequal(as.character(preserved_levels), result$actor_levels)) {
      result$actor_levels <- as.character(preserved_levels)
    }
    return(result)
  }

  candidates <- list(
    source$htna_partition,
    attr(source, "clustering")$htna_partition,
    attr(nets, "clustering")$htna_partition
  )
  embedded <- Filter(function(x) {
    is.list(x) && is.data.frame(x$node_groups)
  }, candidates)
  if (length(embedded)) {
    result <- .htna_normalize_cluster_partition(
      embedded[[1L]]$node_groups,
      action = action
    )
    if (!is.null(embedded[[1L]]$actor_levels)) {
      result$actor_levels <- as.character(embedded[[1L]]$actor_levels)
    }
    return(result)
  }

  child_groups <- lapply(nets, function(net) net$node_groups)
  child_groups <- Filter(function(x) {
    is.data.frame(x) && all(c("node", "group") %in% names(x))
  }, child_groups)
  if (length(child_groups)) {
    combined <- unique(do.call(rbind, lapply(child_groups, function(x) {
      data.frame(
        node = as.character(x$node),
        group = as.character(x$group),
        stringsAsFactors = FALSE
      )
    })))
    result <- .htna_normalize_cluster_partition(combined, action = action)
    levels_from_source <- attr(nets, "actor_levels") %||%
      nets[[1L]]$actor_levels
    if (!is.null(levels_from_source)) {
      result$actor_levels <- as.character(levels_from_source)
    }
    return(result)
  }

  stop(
    "The clustering result has no preserved HTNA actor partition. Pass ",
    "`node_groups` as a named list or a `(node, group)` data frame.",
    call. = FALSE
  )
}

#' @keywords internal
.htna_normalize_cluster_partition <- function(node_groups, action = "code") {
  declared_levels <- attr(node_groups, "actor_levels")
  if (is.data.frame(node_groups)) {
    if (all(c("node", "group") %in% names(node_groups))) {
      node_values <- as.character(node_groups$node)
      group_values_raw <- node_groups$group
    } else {
      if (ncol(node_groups) != 2L || !action %in% names(node_groups)) {
        stop("A clustering `node_groups` data frame must contain either ",
             "`node` and `group`, or the action column `", action,
             "` and one actor-type column.", call. = FALSE)
      }
      group_col <- setdiff(names(node_groups), action)
      node_values <- as.character(node_groups[[action]])
      group_values_raw <- node_groups[[group_col]]
    }
    group_values <- as.character(group_values_raw)
    actor_levels <- if (is.factor(group_values_raw)) {
      levels(group_values_raw)[levels(group_values_raw) %in% group_values]
    } else {
      unique(group_values)
    }
  } else if (is.list(node_groups) && !is.null(names(node_groups)) &&
             all(nzchar(names(node_groups))) &&
             all(vapply(node_groups, is.character, logical(1L)))) {
    actor_levels <- names(node_groups)
    node_values <- unlist(node_groups, use.names = FALSE)
    group_values <- rep(actor_levels, lengths(node_groups))
  } else {
    stop("`node_groups` must be a named list of node vectors or a two-column ",
         "data frame.", call. = FALSE)
  }

  if (anyNA(node_values) || anyNA(group_values)) {
    stop("`node_groups` cannot contain missing nodes or actor assignments.",
         call. = FALSE)
  }
  if (!is.null(declared_levels) &&
      setequal(as.character(declared_levels), unique(group_values))) {
    actor_levels <- as.character(declared_levels)
  }
  duplicated_nodes <- unique(node_values[duplicated(node_values)])
  if (length(duplicated_nodes)) {
    conflicts <- vapply(duplicated_nodes, function(node) {
      length(unique(group_values[node_values == node])) > 1L
    }, logical(1L))
    if (any(conflicts)) {
      stop("Nodes assigned to more than one actor: ",
           paste(duplicated_nodes[conflicts], collapse = ", "), ".",
           call. = FALSE)
    }
    keep <- !duplicated(node_values)
    node_values <- node_values[keep]
    group_values <- group_values[keep]
  }

  list(
    lookup = stats::setNames(group_values, node_values),
    actor_levels = actor_levels
  )
}
